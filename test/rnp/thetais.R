#gives theta and tau2 by calculating BLUP variance

#Estimated theta
thetaest <- function (Ls, ks, tau2) {
  ktau <- 1/tau2
  wol <- sum(ks*ktau/(ks+ktau))
  l <- sum(ks*ktau/(ks+ktau)*Ls/wol)
  l
}

#Study estimated effects
thetais <- function (Ls,ks,tau2) {
  l <- thetaest(Ls,ks,tau2)
  kt <- 1/tau2
  res <- l - (ks*(l-Ls)/(ks+kt))
  res
}
  
vbtau2 <- function(Ls,ses,inittau2,tol=10^(-6),maxiters=300){
  #study weights
  ks <- 1/ses^2
  gettau2 <- function(tau2,iters){
   if(tau2<0)return(tol)
   if(iters>maxiters){
     return(tau2)
     }else{
     n <- length(Ls)
     ths <- thetais(Ls, ks, tau2)
    kis <- function(ses, tau2){
      ki <- function(i){
        w1 <- 1/sum(1/(ses[-c(i)]^2+tau2))
        w <- sum(1/(ses^2+tau2))
        res1 <- (1/ses[i]^2) + (1/(tau2+w1))
        res1 <- (1/tau2) + (1/(ses[i]^2+w1))
        res <- (ses[i]^2 * tau2 / (ses[i]^2 + tau2) + ses[i]^4 / 
                  (w * (ses[i]^2+tau2)^2))
        res
      }
      sapply(1:length(ses),ki)
      }
     vti <- function(ses, tau2){
        ki <- function(i){
        w <- sum(1/(ses^2+tau2))
          res <- (1/w) * ses[i]^2 * (ses[i]^2+2*tau2)/(ses[i]^2+tau2)^2 
               + tau2^2/(ses[i]^2+tau2)
          res
          }
        sapply(1:length(ses),ki)
     }
     tcovs <- function(ses, tau2){
        ki <- function(i){
        w <- sum(1/(ses^2+tau2))
          res <- (1/w) * ses[i]^2*tau2/(ses[i]^2+tau2)^2 + tau2^2/(ses[i]^2+tau2)
          res
          }
        sapply(1:length(ses),ki)
     }
      l <- mean(ths)
      wis <- kis(ses, tau2)
      vtis <- vti(ses, tau2)
      cvs <- tcovs(ses,tau2)
      newtau2 <- sum((l-ths)^2)/n
      newtau2 <- sum(wis)/(n) + sum((l-ths)^2)/(n) - sum(cvs)/n
      newtau2 <- sum(wis+cvs)/(n) - sum((l-ths)^2)/n
      newtau2 <- sum(wis)/(n) + sum((l-ths)^2)/(n)
     res <- if (abs(tau2-newtau2)<tol){
       newtau2
     }else{
       gettau2(newtau2,iters+1)
     }
     res
   }
  }
resTau2 <- gettau2(inittau2,1)
restheta <- thetaest(Ls, ks, resTau2)
out <-list(theta = restheta, tau2=resTau2)
return(out)
}
