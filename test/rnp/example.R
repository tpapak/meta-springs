rm(list=ls())
### NP estimator usage example
study_i <- function(v_AB, sample_size){
  e_is <- rnorm(sample_size, 0, sqrt(v_AB))
  e_i <- mean(e_is)
  se_i <- sqrt(var(e_is)/sample_size)
  return(c(e_i=e_i, se_i=se_i))
}

library(distr)
library(dplyr)
getStudies <- function(ftau2, ftheta, fv_AB, 
                       min_sample_size, max_sample_size,
                       fn){
  fsim_theta_i <- rnorm(fn, ftheta, sqrt(ftau2))
  smNs <- min_sample_size + (max_sample_size - min_sample_size)/14
  lrgNs <- max_sample_size - (max_sample_size - min_sample_size)/14
  sdNs <- (max_sample_size - min_sample_size)/4
  splitNorm <- UnivarMixingDistribution(Truncate(Norm(smNs,sdNs)
                                                , lower=min_sample_size
                                                , upper=max_sample_size), 
                                        Truncate(Norm(lrgNs,sdNs)
                                                , lower=min_sample_size
                                                , upper=max_sample_size), 
                                        mixCoeff=c(0.8, 0.2))
  rsplitNorm <- r(splitNorm)
  sample_sizes <- rsplitNorm(fn) %>% round()
  # hist(sample_sizes)
  range(sample_sizes)
  sample_sizes <- runif(fn, min_sample_size, max_sample_size)
  esses <- data.frame()
  esses <- Reduce(function(acc,ss){
    crow <- study_i(fv_AB, ss)
    return(rbind(acc,crow))
    },sample_sizes,esses)
  names(esses)<-c("e_i","se_i")
  studies <- data.frame(y_i=fsim_theta_i+esses$e_i
                       , se_i=esses$se_i
                       , e_i=esses$e_i
                       , n=sample_sizes
                       , theta_i = fsim_theta_i)
  return(studies)
}


# Execute function "estimate_theta_RE"
simparameters <- list(ftau2 = 3
                     ,ftheta = 4.0
                     ,fv_AB = 100.0
                     ,min_sample_size = 10
                     ,max_sample_size = 100
                     ,fn = 3)
  
studies <- with(simparameters, getStudies( ftau2
                                         , ftheta
                                         , fv_AB
                                         , min_sample_size
                                         , max_sample_size
                                         , fn))

studies$treatmentA <- "A"
studies$treatmentB <- "B"
studies$meanB <- 0
studies$meanA <- studies$y_i
studies$study <- 1:dim(studies)[1]
studies$sdA <- studies$se_i/sqrt(2)
studies$sdB <- studies$sdA
studies$nA <- 1
studies$nB <- 1

library(meta)

# 
# mdsML = metacont( mean.e=studies$meanA, n.e=studies$nA, sd.e=studies$sdA
#                 , mean.c=studies$meanB, n.c=studies$nB, sd.c=studies$sdB
#                 , studlab=studies$study, sm="MD", method.tau="ML")
# 
# metacont_tau2ML <- mdsML$tau2

mdsDL = metacont( mean.e=studies$meanA, n.e=studies$nA, sd.e=studies$sdA
                , mean.c=studies$meanB, n.c=studies$nB, sd.c=studies$sdB
                , studlab=studies$study, sm="MD", method.tau="DL")
DL <- data.frame( theta = c(mdsDL$TE.random)
                , tau2 = c(mdsDL$tau2)
                , label = "DL"
                , timer ="0"
                )
 
 # mdsPM = metacont( mean.e=studies$meanA, n.e=studies$nA, sd.e=studies$sdA
 #                 , mean.c=studies$meanB, n.c=studies$nB, sd.c=studies$sdB
 #                 , studlab=studies$study, sm="MD", method.tau="PM")
 # 
 # PM <- data.frame( theta = c(mdsPM$TE.random)
 #                 , tau2 = c(mdsPM$tau2)
 #                 , label = "PM"
 #                 )
library(tictoc)
library(metafor)
tic()
met1 <- rma(studies$y_i, (studies$se_i)^2,verbose = T,method="REML",
            control=list(tol=10^(-8)))
remlt <- toc()$callback_msg
#print(summary(met1))
print("reml tau2")
print(met1$tau2)
reml <- data.frame( theta = c(met1$beta[1])
                  , tau2 = c(met1$tau2)
                  , label = "REML"
                  , timer = remlt
                  )

tic()
mdsML = metacont( mean.e=studies$meanA, n.e=studies$nA, sd.e=studies$sdA
                , mean.c=studies$meanB, n.c=studies$nB, sd.c=studies$sdB
                , studlab=studies$study, sm="MD", method.tau="ML")
mlt <- toc()$callback_msg
ML <- data.frame( theta = c(mdsML$TE.random)
                , tau2 = c(mdsML$tau2)
                , label = "ML"
                , timer = mlt
                )

source("deltais.R")
tic()
VD <- vdtau2(studies$y_i, studies$se_i, 0.5)
vdt <- toc()$callback_msg
print(VD)
VD$label <- "VD"
VD$timer <- vdt
print(vdt)
VD <- as.data.frame(VD)

#  
source("deltaischecks.R")
tic()
VDVarCheck <- vdVarCheck(studies$y_i, studies$se_i, 0.5)
vdVCt <- toc()$callback_msg
VDVarCheck$label <- "VDVarCheck"
VDVarCheck$timer <- vdVCt
VDVarCheck <- as.data.frame(VDVarCheck)

# source("deltaischecks.R")
# tic()
# VDe <- vde(studies$y_i, studies$se_i, 0.5)
# vdet <- toc()$callback_msg
# VDe$label <- "VDe"
# VDe$timer <- vdet
# VDe <- as.data.frame(VDe)

#  
# source("thetais.R")
# VB <- vbtau2(studies$y_i, studies$se_i, 0.9)
# print(VB)
# VB$label <- "VB"
# VB <- as.data.frame(VB)
#  

getBiases <- function(studies, estimators, parameters){
  tau2var <- var(studies$theta_i)
  thetaBias <- estimators$theta - parameters$ftheta
  estimators <- cbind(estimators, thetaBias)
  theta_est <- mean(studies$theta_i)
  thetaBias_estimated <- estimators$theta - theta_est
  estimators <- cbind(estimators, thetaBias_estimated)
  tau2Bias <- estimators$tau2 - parameters$ftau2
  estimators <- cbind(estimators, tau2Bias)
  tau2Bias_estimated <- estimators$tau2 - tau2var
  estimators <- cbind(estimators, tau2Bias_estimated)
  estimators
}
restab <- rbind( VD
               , VDVarCheck
               , reml
               , ML
               , DL
               # , VDe
               # , VB
               # , VB
               )
results <- getBiases(studies,restab,simparameters)
print(results,digits=18)

# csvfilename = paste(names(simparameters),simparameters,sep="_") %>% 
#   paste(collapse='') %>% paste(".csv",sep='')
# write.csv(studies, csvfilename)
# 
# library(jsonlite)
# 
# jsonfilename = paste(strsplit(csvfilename, "\\.csv")[[1]][1],".json",sep="")
# csvfile = read.csv(csvfilename)
# write_json(csvfile, path=jsonfilename)
