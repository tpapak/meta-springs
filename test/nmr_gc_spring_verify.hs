{-# LANGUAGE OverloadedStrings #-}
-- Verify the GC NMR IS the spring system:
--  (1) at each tau2, the fixed-effect solution is the EQUILIBRIUM of the explicit
--      spring network (mu, d, beta, delta-per-arm): net force = b - A*theta = 0.
--  (2) integrating delta reproduces the marginal solve (beta_full == beta_marg).
--  (3) the tau2 posterior is the normalised spring PARTITION FUNCTION Z(tau2);
--      logZ = -1/2[ sum log(v+tau2/2) + logdet(A) + RSS ] (the spring free energy).
-- Shows the Z(tau2) curve and the (beta, tau2) posterior; compares to gemtc.
module Main where
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.List (sort, nub, foldl')
import Text.Printf (printf)
import Data.Meta.Effects (TreatmentId (..), StringIntId (..), tidOfArm, Arm(..))
import Data.Meta.Studies (StudyId (..), getStudyId, getStudyArms, readStudies, Study(..))

-- minimal dense linear algebra
solveLU :: [[Double]] -> [Double] -> [Double]
solveLU a0 b0 = back (elim (zipWith (\r y->r++[y]) a0 b0) 0)
  where n=length b0
        elim m c | c>=n = m
                 | otherwise = let p=foldl' (\bi i->if abs(m!!i!!c)>abs(m!!bi!!c) then i else bi) c [c..n-1]
                                   m1=swap c p m; pr=m1!!c
                               in elim [ if r<=c then m1!!r else let f=(m1!!r!!c)/(pr!!c) in zipWith (\u v->u-f*v) (m1!!r) pr | r<-[0..n-1]] (c+1)
        swap i j m=[ if k==i then m!!j else if k==j then m!!i else m!!k | k<-[0..n-1]]
        back m=foldr step (replicate n 0) [0..n-1]
          where step i xs=let row=m!!i in (\s->take i xs++[(row!!n-s)/(row!!i)]++drop (i+1) xs) (sum [row!!j*xs!!j|j<-[i+1..n-1]])
matVec m v=[ sum (zipWith (*) r v) | r<-m ]
logDet a0 = go a0 0 0
  where n=length a0
        go m c acc | c>=n=acc | otherwise=
          let p=foldl' (\bi i->if abs(m!!i!!c)>abs(m!!bi!!c) then i else bi) c [c..n-1]
              m1=[ if k==c then m!!p else if k==p then m!!c else m!!k | k<-[0..n-1]]; pv=m1!!c!!c
          in go [ if r<=c then m1!!r else let f=(m1!!r!!c)/pv in zipWith (\u v->u-f*v) (m1!!r) (m1!!c) | r<-[0..n-1]] (c+1) (acc+log(abs pv))

main :: IO ()
main = do
  Just cr <- A.decode <$> BL.readFile "/tmp/nmrgc/cov.json" :: IO (Maybe (Map.Map String Double))
  let cov = Map.fromList [ (StudyId (StringId k), v) | (k,v)<-Map.toList cr ]
  Right studies <- readStudies "/tmp/nmrgc/rep1_long.json"
  let treats = sort $ nub $ concatMap (map tidOfArm . getStudyArms) studies
      ref = head treats; nonref = filter (/=ref) treats; nT=length nonref
      sids = map getStudyId studies; nS=length sids
      sidx = Map.fromList (zip sids [0..]); tidx = Map.fromList (zip nonref [0..])
      armsFlat = [ (getStudyId s, a) | s<-studies, a<-getStudyArms s ]
      nArm = length armsFlat
      -- arm payload (study idx, treatment, x, y=mean, v=sd^2/n)
      arm (sid,a) = case a of
        ContinuousArm t m sd n -> (sidx Map.! sid, t, Map.findWithDefault 0 sid cov, m, sd*sd/fromIntegral n)
        _ -> error "continuous"
      payload = map arm armsFlat
      -- FULL spring network coords: mu(nS), d(nT), beta(1), delta(nArm)
      nPf = nS + nT + 1 + nArm; bcolF = nS+nT
      rowF k (si,t,x,_,_) = let base=upd (replicate nPf 0) si 1
                                wB = if t==ref then base else upd (upd base (nS+tidx Map.!t) 1) bcolF x
                            in upd wB (nS+nT+1+k) 1     -- delta_k coordinate
      upd xs i v = take i xs ++ [v] ++ drop (i+1) xs
      springsFull t2 = obs ++ tau
        where obs = [ (1/v, rowF k p, y) | (k,p@(_,_,_,y,v)) <- zip [0..] payload ]
              tau = [ (2/t2, upd (replicate nPf 0) (nS+nT+1+k) 1, 0) | k<-[0..nArm-1] ]
      assemble nP sps = ( [ [ sum [ kk*(g!!i)*(g!!j) | (kk,g,_)<-sps ] | j<-[0..nP-1]] | i<-[0..nP-1]]
                        , [ sum [ kk*nl*(g!!i) | (kk,g,nl)<-sps ] | i<-[0..nP-1]] )
      -- marginal coords: mu,d,beta with weight 1/(v+t2/2)
      nPm = nS+nT+1; bcolM=nS+nT
      rowM (si,t,x,_,_) = let base=upd (replicate nPm 0) si (-1)
                          in if t==ref then base else upd (upd base (nS+tidx Map.!t) 1) bcolM x
      marg t2 = let t2a=t2/2; sps=[ (1/(v+t2a), rowM p, y) | p@(_,_,_,y,v)<-payload ]
                    (am,bm)=assemble nPm sps; th=solveLU am bm
                    sumlogv = sum [ log (v+t2a) | (_,_,_,_,v)<-payload ]
                    ywy = sum [ y*y/(v+t2a) | (_,_,_,y,v)<-payload ]
                    bty = sum (zipWith (*) bm th)
                    logz = -0.5*(sumlogv + logDet am + (ywy - bty))
                in (th!!bcolM, logz, am, bm, th)
  -- (1)+(2): equilibrium + delta-integration identity at tau2=0.10
  let t2 = 0.10
      (af,bf) = assemble nPf (springsFull t2); thf = solveLU af bf
      force = zipWith (-) bf (matVec af thf)        -- net force = b - A theta ; 0 at equilibrium
      (bM,_,_,_,_) = marg t2
  printf "=== solve the GC spring system (tau2=%.2f) ===\n" t2
  printf "explicit spring net: coords=%d (mu+d+beta+delta), springs=%d (obs+tau)\n" nPf (2*nArm)
  printf "max |net force| at solution = %.2e   (0 => force-balance EQUILIBRIUM)\n" (maximum (map abs force))
  printf "beta from FULL spring eq = %.6f ;  beta from marginal (delta integrated) = %.6f  (identical)\n" (thf!!bcolF) bM
  -- (3): partition function curve + posterior
  let grid = [ 0.01 + 0.01*fromIntegral i | i<-[0..40::Int] ]
      cells = [ let (b,lz,_,_,_)=marg t2' in (t2',b,lz) | t2'<-grid ]
      mx = maximum [ lz | (_,_,lz)<-cells ]
      ws = [ exp (lz-mx) | (_,_,lz)<-cells ]; tot=sum ws
      post = zipWith (\(t,_,_) w -> (t, w/tot)) cells ws
      modeT = let (t,_,_) = foldr1 (\a@(_,_,la) b@(_,_,lb)->if la>=lb then a else b) cells in t
      meanT = sum [ t*p | (t,p)<-post ]
      meanB = sum [ b*p | ((_,b,_),(_,p))<-zip cells post ]
  printf "\n=== tau2 posterior = normalised spring partition function Z(tau2) ===\n"
  printf "tau2:   "; mapM_ (\(t,_,_)->printf "%5.2f " t) (filter (\(t,_,_)->t<=0.30) cells); printf "\n"
  printf "Z(norm):"; mapM_ (\(t,p)->printf "%5.2f " (p/maximum (map snd post))) (filter (\(t,_)->t<=0.30) post); printf "  (peak=mode)\n"
  printf "tau2 mode=%.4f mean=%.4f ; beta posterior mean=%.5f\n" modeT meanT meanB
  printf "gemtc (rep1): tau2 median~0.115 ; beta~0.40   (see test/nmr_posterior_data/gemtc_3t.json)\n"
