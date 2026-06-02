-- Wiring exchangeable β as literal tau-springs.
--
-- We build the FULL spring network for an exchangeable-β NMR as one list of
-- springs over coordinates (μ_i, d_t, β_t, B, δ_a). Every spring is the same
-- object  ½·k·(Σ coef·coord − ℓ₀)² :
--   * obs spring per arm     k=1/v,    ℓ₀=y,  couples μ_i + d_t + β_t·x_i + δ_a
--   * HETEROGENEITY τ-spring k=2/τ²,  ℓ₀=0,  ties δ_a → 0           (random effects)
--   * SLOPE σ_B²-spring      k=1/σ_B², ℓ₀=0,  ties β_t → B          (exchangeable β)
-- The τ-spring and the slope-spring are IDENTICAL in form — exchangeable β is just
-- the heterogeneity construct applied to the slope coordinates. We assemble the
-- precision A = Σ k·g·gᵀ, b = Σ k·ℓ₀·g, solve, and cross-check β,B against the
-- marginal solve (δ integrated ⇒ arm variance v+τ²/2 + the β–B coupling), which is
-- exactly what nmaRegressionPosteriorB computes. They agree to machine precision.
module Main where
import Data.List (intercalate)
import Text.Printf (printf)

-- tiny dataset: treatments A(ref),B,C ; 5 two-arm studies with covariate x
-- (study, treatment, x, y=observed arm mean)
arms :: [(Int,Char,Double,Double)]
arms =
  [ (0,'A',1.0, 0.10),(0,'B',1.0, 0.95)
  , (1,'A',2.0,-0.05),(1,'C',2.0, 1.70)
  , (2,'A',0.0, 0.20),(2,'B',0.0, 0.55)
  , (3,'A',-1.0,0.00),(3,'C',-1.0,0.90)
  , (4,'B',1.0, 0.30),(4,'C',1.0, 1.15) ]

v, tau2, sigB2 :: Double
v = 0.04 ; tau2 = 0.10 ; sigB2 = 0.05

nS = 5 :: Int
nonref = "BC"
tIdx c = case c of 'B'->0; 'C'->1; _->error "ref"
nA = length arms
-- coordinate layout
muC i  = i                       -- 0..4
dC  c  = nS + tIdx c             -- 5,6
bC  c  = nS + 2 + tIdx c         -- 7,8
bigB   = nS + 4                  -- 9
deC a  = nS + 5 + a              -- 10..19
nP = nS + 5 + nA                 -- 20

type Spring = (String, Double, [(Int,Double)], Double)   -- (label, k, incidence, ℓ₀)

springs :: [Spring]
springs =
     [ ("obs",  1/v, [(muC s,1)] ++ (if t/='A' then [(dC t,1),(bC t,x)] else []) ++ [(deC a,1)], y)
     | (a,(s,t,x,y)) <- zip [0..] arms ]
  ++ [ ("tau",  2/tau2, [(deC a,1)], 0)                  -- heterogeneity τ-springs
     | a <- [0..nA-1] ]
  ++ [ ("slope",1/sigB2, [(bC t,1),(bigB,-1)], 0)        -- exchangeable σ_B² slope-springs
     | t <- nonref ]

-- assemble precision A = Σ k g gᵀ and rhs b = Σ k ℓ₀ g
assemble :: [Spring] -> ([[Double]],[Double])
assemble sps =
  ( [ [ sum [ k * coef i g * coef j g | (_,k,g,_) <- sps ] | j<-[0..nP-1] ] | i<-[0..nP-1] ]
  , [ sum [ k * l * coef i g | (_,k,g,l) <- sps ] | i<-[0..nP-1] ] )
  where coef i g = maybe 0 id (lookup i g)

solve :: [[Double]] -> [Double] -> [Double]    -- Gauss-Jordan
solve a0 b0 = let aug = zipWith (\r y->r++[y]) a0 b0 in map last (gj aug 0)
  where n = length b0
        gj m c | c>=n = m
               | otherwise =
                   let p = head [ i | i<-[c..n-1], abs(m!!i!!c) > 1e-12 ]
                       m1 = swap c p m
                       pr = m1!!c ; pivot = pr!!c
                       pr' = map (/pivot) pr
                       m2 = [ if r==c then pr' else let f=(m1!!r!!c) in zipWith (\u w->u-f*w) (m1!!r) pr' | r<-[0..n-1] ]
                   in gj m2 (c+1)
        swap i j m = [ if k==i then m!!j else if k==j then m!!i else m!!k | k<-[0..n-1] ]

-- marginal solve over (μ,d,β,B): arm weight 1/(v+τ²/2) + the β–B slope springs
marginal :: ([[Double]],[Double])
marginal =
  let mP = nS + 5
      w  = 1/(v + tau2/2)
      dsp = [ ("obs", w, [(muC s,1)] ++ (if t/='A' then [(dC t,1),(bC t,x)] else []), y) | (s,t,x,y) <- map snd (zip [0..] arms) ]
      ssp = [ ("slope",1/sigB2,[(bC t,1),(bigB,-1)],0) | t<-nonref ]
      sps = dsp++ssp
      coef i g = maybe 0 id (lookup i g)
  in ( [ [ sum [ k*coef i g*coef j g | (_,k,g,_)<-sps ] | j<-[0..mP-1] ] | i<-[0..mP-1] ]
     , [ sum [ k*l*coef i g | (_,k,g,l)<-sps ] | i<-[0..mP-1] ] )

main :: IO ()
main = do
  putStrLn "Spring list (note: tau and slope springs have IDENTICAL form k·(coord−target)²):\n"
  let grp lbl = [ s | s@(l,_,_,_)<-springs, l==lbl ]
      shw (l,k,g,nl) = printf "  %-6s k=%-7.3f ℓ₀=%-4.2f  incidence=%s\n" l k nl
                         (intercalate " " [ printf "%s%+.2g" (nm i) c | (i,c)<-g ])
      nm i | i<nS = "μ"++show i | i<nS+2 = "d"++[nonref!!(i-nS)]
           | i<nS+4 = "β"++[nonref!!(i-nS-2)] | i==bigB="B" | otherwise="δ"++show(i-nS-5)
  putStrLn "obs springs:";   mapM_ shw (grp "obs")
  putStrLn "HETEROGENEITY τ-springs (k=2/τ²):"; mapM_ shw (take 3 (grp "tau")); putStrLn "  ... (one per arm)"
  putStrLn "EXCHANGEABLE β slope-springs (k=1/σ_B²):"; mapM_ shw (grp "slope")

  let (af,bf) = assemble springs ; xf = solve af bf
      (am,bm) = marginal ; xm = solve am bm
  printf "\nFull spring solve (μ,d,β,B,δ all explicit):  β_B=%.5f  β_C=%.5f  B=%.5f\n" (xf!!bC 'B') (xf!!bC 'C') (xf!!bigB)
  printf "Marginal solve (δ integrated ⇒ v+τ²/2):       β_B=%.5f  β_C=%.5f  B=%.5f\n" (xm!!bC 'B') (xm!!bC 'C') (xm!!bigB)
  printf "max|difference| = %.2e   (the τ-spring and σ_B²-spring are one machinery)\n"
         (maximum [ abs (xf!!c - xm!!c) | c <- [dC 'B',dC 'C',bC 'B',bC 'C',bigB] ])
  printf "\n(unshrunk slopes if σ_B²→∞ would be the per-arm fits; here σ_B²=%.2f pulls β_B,β_C toward B)\n" sigB2
