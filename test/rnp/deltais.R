#gives delta and tau2 by calculating BLUP variance

#Estimated theta, Ls are Yi(study effects) and ks are the weights
thetaest <- function (Ls, ks, tau2) {
  ktau <- 1/tau2
  wol <- sum(ks*ktau/(ks+ktau))
  l <- sum(ks*ktau/(ks+ktau)*Ls/wol)
  l
}

#Study estimated effects
deltais <- function (Ls,ks,tau2) {
  l <- thetaest(Ls,ks,tau2)
  kt <- 1/tau2
  res <- ks*(l-Ls)/(ks+kt)
  res
}
  
vdtau2 <- function(Ls,ses,inittau2,tol=10^(-8),maxiters=300){
  #study weights
  ks <- 1/ses^2
  gettau2 <- function(tau2,iters){
   if(tau2<0)return(tol)
   if(iters>maxiters){
     return(tau2)
     }else{
     n <- length(Ls)
     dis <- deltais(Ls, ks, tau2)
     kis <- function(ses, tau2){
      ki <- function(i){
        w <- 1/sum(1/(ses[-c(i)]^2+tau2))
        res <- (1/tau2) + (1/(ses[i]^2+w))
        res
      }
      sapply(1:length(ses),ki)
     }
     ## Mean of Vis
     vis <- 1/kis(ses, tau2)
     ## THIS IS THE EStimator
     newtau2 <- sum(dis^2)/(n) + sum(vis)/(n)
     res <- if (abs(tau2-newtau2)<tol){
       newtau2
     }else{
       gettau2(newtau2,iters+1)
     }
     res
   }
  }
resTau2 <- gettau2(inittau2,1)
resdelta <- thetaest(Ls, ks, resTau2)
out <-list(theta = resdelta, tau2=resTau2)
return(out)
}
