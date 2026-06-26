{-# LANGUAGE OverloadedStrings #-}
-- Verify baseline-risk NMR IS the spring system (the honest spring representation).
-- Usage: stack runghc -- test/nmr_baseline_risk_spring_verify.hs <studies.json> <ref>
--
-- Coordinates: eta_i (study baselines), d_t (contrasts), B (baseline-risk slope).
-- Each arm is a BinomialSpring whose length is the logit linear predictor:
--   reference arm:  L = eta_i
--   active arm:     L = eta_i + d_t + B*(eta_i - mx)
-- The library's own springForce/springEnergy make the claims exact:
--   springEnergy(BinomialSpring e n L) = n*log(1+e^L) - e*L  =  the negative log-likelihood
--   springForce(BinomialSpring e n L)  = e - n*sigmoid(L)    =  the score
-- so minimum energy = MLE and net force = 0 is the MLE.  We verify:
--  (1) the MLE is the force-balance EQUILIBRIUM: max|net force| ~ 0.
--  (2) the active-arm rod is SELF-REFERENTIAL & COUPLED: dL/d eta_i = 1+B (not 1) and
--      dL/dB = eta_i - mx (length depends on the baseline NODE position, not a constant).
--  (3) both arms of a study anchor to ONE shared baseline node eta_i = randomization made
--      explicit (the treated group, untreated, would share the control group's baseline).
--  (4) the equilibrium reproduces nmaRegressionBaselineRiskBin (d, B).
--  (5) RE: linearise to working springs, add tau-springs on per-arm delta; the tau2
--      posterior is the normalised spring partition function Z(tau2); delta integrates out.
module Main where

import System.Environment (getArgs)
import Data.List (sort, nub, foldl')
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), tidOfArm, Arm (..))
import Data.Meta.Studies (StudyId (..), getStudyId, getStudyArms, readStudies, Study (..))
import Data.Meta.NMA (Spring (..), springForce, hardness, naturalLength, springEnergy,
                      nmaRegressionBaselineRiskBin, nmaRegressionBaselineRiskBinRE,
                      BaselineRiskFit (..))

-- compact dense linear algebra (partial pivoting)
solveLU :: [[Double]] -> [Double] -> [Double]
solveLU a0 b0 = back (elim (zipWith (\r y -> r ++ [y]) a0 b0) 0)
  where n = length b0
        elim m c | c >= n = m
                 | otherwise = let p = foldl' (\bi i -> if abs (m!!i!!c) > abs (m!!bi!!c) then i else bi) c [c..n-1]
                                   m1 = swap c p m; pr = m1!!c
                               in elim [ if r<=c then m1!!r else let f=(m1!!r!!c)/(pr!!c) in zipWith (\u v->u-f*v) (m1!!r) pr | r<-[0..n-1]] (c+1)
        swap i j m = [ if k==i then m!!j else if k==j then m!!i else m!!k | k<-[0..n-1]]
        back m = foldr step (replicate n 0) [0..n-1]
          where step i xs = let row=m!!i in (\s -> take i xs ++ [(row!!n - s)/(row!!i)] ++ drop (i+1) xs) (sum [row!!j*xs!!j | j<-[i+1..n-1]])
matVec :: [[Double]] -> [Double] -> [Double]
matVec m v = [ sum (zipWith (*) r v) | r <- m ]
matInv :: [[Double]] -> [[Double]]
matInv a = let n=length a in [ [ (solveLU a (unit j))!!i | j<-[0..n-1] ] | i<-[0..n-1] ]
  where unit j = [ if k==j then 1 else 0 | k<-[0..length a-1] ]
logDet :: [[Double]] -> Double
logDet a0 = go a0 0 0
  where n = length a0
        go m c acc | c>=n = acc | otherwise =
          let p = foldl' (\bi i -> if abs (m!!i!!c) > abs (m!!bi!!c) then i else bi) c [c..n-1]
              m1 = [ if k==c then m!!p else if k==p then m!!c else m!!k | k<-[0..n-1]]; pv=m1!!c!!c
          in go [ if r<=c then m1!!r else let f=(m1!!r!!c)/pv in zipWith (\u v->u-f*v) (m1!!r) (m1!!c) | r<-[0..n-1]] (c+1) (acc + log (abs pv))

main :: IO ()
main = do
  args <- getArgs
  let (path, refName) = case args of (p:r:_) -> (p,r); [p] -> (p,"A"); _ -> ("test/nmr_certolizumab.json","Placebo")
      ref = TreatmentId (StringId refName)
  Right studies <- readStudies path
  let treats = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      nonref = filter (/= ref) treats; nT = length nonref
      sids = map getStudyId studies; nS = length sids
      sidx = Map.fromList (zip sids [0..]); tidx = Map.fromList (zip nonref [0..])
      nP = nS + nT + 1; betaCol = nS + nT
      dCol t = nS + tidx Map.! t
      -- arm payload: (study idx, isRef, dCol(if active), events, n)
      armOf (sid,a) = case a of
        BinaryArm t ev ns -> (sidx Map.! sid, t==ref, if t==ref then -1 else dCol t, ev, ns)
        _ -> error "baseline-risk spring verify: expects binary arms"
      arms = [ armOf (getStudyId s, a) | s<-studies, a<-getStudyArms s ]
      nArm = length arms
      -- centering mx = mean continuity-corrected control log-odds
      ctrlLO = [ let rd=if fromIntegral ev==(0::Double) then 0.5 else fromIntegral ev
                     nd=if ev==0 then fromIntegral ns+1 else fromIntegral ns; pp=rd/nd
                 in log (pp/(1-pp))
               | s<-studies, BinaryArm t ev ns <- getStudyArms s, t==ref ]
      mx = sum ctrlLO / fromIntegral (length ctrlLO)
      unit i = [ if j==i then 1 else 0 | j<-[0..nP-1] ]
      -- length L and geometry g = dL/dtheta for an arm at coords theta
      geomOf theta (si,isRef,dc,_,_) =
        let etaI = theta!!si; bB = theta!!betaCol
        in if isRef
             then (etaI, unit si)
             else ( etaI + theta!!dc + bB*(etaI - mx)
                  , [ (if j==si then 1+bB else 0) + (if j==dc then 1 else 0)
                        + (if j==betaCol then etaI - mx else 0) | j<-[0..nP-1] ] )
      -- the explicit binomial springs at coords theta (library's Spring type)
      springsAt theta = [ let (l,g)=geomOf theta a in (g, BinomialSpring e n l) | a@(_,_,_,e,n)<-arms ]
      netForce theta = let ss=springsAt theta in [ sum [ springForce spr*(g!!j) | (g,spr)<-ss ] | j<-[0..nP-1] ]
      hessF theta = let ss=springsAt theta in [ [ sum [ hardness spr*(g!!i)*(g!!j) | (g,spr)<-ss ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
      energy theta = sum [ springEnergy spr | (_,spr)<-springsAt theta ]
      -- find the equilibrium by damped Newton on the spring forces
      theta0 = [ if i<nS then ctrlLO!!i else 0 | i<-[0..nP-1] ]
      step theta = let dir = solveLU (hessF theta) (netForce theta)
                       e0 = energy theta
                       try a k | k<=(0::Int) = theta
                               | not (isNaN et || isInfinite et) && et <= e0 + 1e-9 = cand
                               | otherwise = try (a/2) (k-1)
                         where cand = zipWith (\x dx -> x + a*dx) theta dir; et = energy cand
                   in try 1.0 40
      eqLoop theta it | it>(200::Int) = theta
                      | maximum (map abs (zipWith (-) theta theta')) < 1e-12 = theta'
                      | otherwise = eqLoop theta' (it+1)
        where theta' = step theta
      thetaEq = eqLoop theta0 0
      force = netForce thetaEq
      -- an example active arm for the coupled-rod display
      (aSi,_,aDc,_,_) = head [ a | a@(_,isR,_,_,_)<-arms, not isR ]
      bEq = thetaEq!!betaCol

  printf "=== baseline-risk NMR IS the spring system:  %s  ref=%s ===\n" path refName
  printf "explicit binomial spring net: coords=%d (eta_i:%d + d:%d + B:1), springs=%d (one per arm)\n"
         nP nS nT nArm
  printf "spring energy = sum [n*log(1+e^L) - e*L] = negative log-likelihood; force = e - n*sigmoid(L) = score\n\n"

  printf "(1) FORCE-BALANCE EQUILIBRIUM\n"
  printf "    max |net force| at solution = %.2e   (0 => equilibrium = MLE)\n" (maximum (map abs force))
  printf "    total spring energy (=neg log-lik) at equilibrium = %.6f\n\n" (energy thetaEq)

  printf "(2) the active-arm rod is SELF-REFERENTIAL & COUPLED (example arm, study %d)\n" aSi
  printf "    dL/d eta_i = 1 + B = %.4f   (NOT 1: the spring's grip on its baseline node depends on B)\n" (1+bEq)
  printf "    dL/d B     = eta_i - mx = %.4f   (rod LENGTH depends on the baseline NODE position)\n" (thetaEq!!aSi - mx)
  printf "    => external-covariate NMR has a fixed-length rod beta*x; here the rod ties to a node.\n\n"

  printf "(3) SHARED baseline node = randomization made explicit\n"
  printf "    both arms of each study attach to ONE coordinate eta_i; the treated arm's length is\n"
  printf "    eta_i + d_t + B*(eta_i-mx): same eta_i it would have had untreated (control), by randomization.\n\n"

  case nmaRegressionBaselineRiskBin studies ref of
    Left e -> printf "(4) library cross-check FAILED: %s\n" e
    Right fit -> do
      printf "(4) equilibrium reproduces nmaRegressionBaselineRiskBin\n"
      printf "    B:   spring eq = %.6f   library = %.6f\n" bEq (brB fit)
      mapM_ (\t -> printf "    d[%s]: spring eq = %.6f   library = %.6f\n"
                     (nm t) (thetaEq!!dCol t) (brD fit Map.! t)) nonref

  -- (5) RE: linearise to working springs at the FE mode; tau-springs pin per-arm delta.
  -- The tau2 posterior is the normalised spring partition function Z(tau2). delta integrates out.
  let working = [ let (l,g)=geomOf thetaEq a; spr=BinomialSpring e n l
                      w = hardness spr; z = naturalLength spr      -- working response (Gaussian spring)
                  in (g, z, max 1e-9 w) | a@(_,_,_,e,n)<-arms ]
      -- marginal coords eta,d,B with weight 1/(1/w + tau2/2)
      marg t2 = let t2a=t2/2
                    sps = [ (1/(1/w + t2a), g, z) | (g,z,w)<-working ]
                    aM = [ [ sum [ kk*(g!!i)*(g!!j) | (kk,g,_)<-sps ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
                    bM = [ sum [ kk*z*(g!!i) | (kk,g,z)<-sps ] | i<-[0..nP-1] ]
                    th = solveLU aM bM
                    sumlogv = sum [ log (1/w + t2a) | (_,_,w)<-working ]
                    zwz = sum [ z*z/(1/w + t2a) | (_,z,w)<-working ]
                    bty = sum (zipWith (*) bM th)
                    logz = -0.5*(sumlogv + logDet aM + (zwz - bty))
                in (th!!betaCol, logz)
      grid = [ 0.002 + 0.004*fromIntegral i | i<-[0..50::Int] ]
      cells = [ (t2, snd (marg t2)) | t2<-grid ]
      lmx = maximum (map snd cells); ws = [ exp (lz-lmx) | (_,lz)<-cells ]; tot=sum ws
      post = zipWith (\(t,_) w -> (t, w/tot)) cells ws
      modeT = fst (foldr1 (\a@(_,la) b@(_,lb) -> if la>=lb then a else b) cells)
      meanT = sum [ t*p | (t,p)<-post ]
  printf "\n(5) RE: tau2 posterior = normalised spring partition function Z(tau2)\n"
  printf "    logZ(tau2) = -1/2[ sum log(1/w+tau2/2) + logdet(A) + RSS ]   (the spring free energy)\n"
  printf "    tau2 mode = %.4f   mean = %.4f   (delta-per-arm integrated out)\n" modeT meanT
  case nmaRegressionBaselineRiskBinRE studies ref Nothing of
    Right ref' -> printf "    library nmaRegressionBaselineRiskBinRE tau2 = %.4f   (PQL point)\n" (brTau2 ref')
    Left _     -> return ()
  where
    nm (TreatmentId (StringId s)) = s
    nm (TreatmentId (IntId i)) = show i
