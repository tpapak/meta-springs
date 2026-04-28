### Excercise 2

study_i <- function(v_AB, sample_size){
  e_is <- rnorm(sample_size, 0, sqrt(v_AB/sample_size))
  e_i <- mean(e_is)
  se_i <- sqrt(var(e_is))
  return(c(e_i=e_i, se_i=se_i))
}

library(purrr)
getStudies <- function(ftau2, ftheta, fv_AB, 
                       min_sample_size, max_sample_size,
                       fn){
  fsim_theta_i <- rnorm(fn, ftheta, sqrt(ftau2))
  
  sample_sizes <- runif(fn, min_sample_size, max_sample_size)
  
  esses <- data.frame()
  esses <- Reduce(function(acc,ss){
    crow <- study_i(fv_AB, ss)
    return(rbind(acc,crow))
    },sample_sizes,esses)
  names(esses)<-c("e_i","se_i")
  
  studies <- data.frame(y_i=fsim_theta_i+esses$e_i, se_i=esses$se_i, n=sample_sizes)
  return(studies)
}

nstudies <- 300

# Execute function "estimate_theta_RE"
studies <- getStudies(ftau2 = 1.0
                     ,ftheta = 1.0
                     ,fv_AB =  0.4
                     ,min_sample_size = 1000
                     ,max_sample_size = 1000
                     ,fn = nstudies)
#studies <- read.csv("sim30.csv")
print(studies)
studies$treatmentA <- "A"
studies$treatmentB <- "B"
studies$meanB <- 0
studies$meanA <- studies$y_i
studies$study <- 1:dim(studies)[1]
studies$sdA <- studies$se_i/sqrt(2)
studies$sdB <- studies$sdA
studies$nA <- 1
studies$nB <- 1

library(jsonlite)

csvfilename = "simfew.csv"
write.csv(studies, csvfilename)
jsonfilename = paste(strsplit(csvfilename, "\\.")[[1]][1],".json",sep="")
csvfile = read.csv(csvfilename)
write_json(csvfile, path=jsonfilename)

library(metafor)
met1 <- rma(studies$y_i, (studies$se_i)^2, verbose=TRUE, digits=5)
print(summary(met1))
print("tau2")
print(met1$tau2)
