# Monte-Carlo generator for binary baseline-risk NMR verification.
#   FE shared-slope DGP:  logit(p_ctrl)=eta_i ,  logit(p_act)=eta_i+d_t+B*(eta_i-mxT_i)
# Writes /tmp/brmc/rep_NNN.json (binary pairwise format readStudies reads) + truth.json.
set.seed(424242)
dir.create("/tmp/brmc", showWarnings=FALSE, recursive=TRUE)

NREP   <- 200
actives<- c("B","C")
nper   <- c(B=15, C=15)          # studies per active-vs-control comparison
d_true <- c(B=0.8, C=1.2)        # contrasts at baseline = mean baseline
B_true <- -0.50                  # shared baseline-risk slope
narm   <- 120                    # arm size
eta_m  <- -1.0; eta_sd <- 0.85   # study baseline spread (identifies the slope)

writeRep <- function(rep) {
  rows <- list(); k <- 0; etas <- c(); meta <- list()
  for (a in actives) for (j in 1:nper[a]) { k<-k+1; etas<-c(etas, rnorm(1, eta_m, eta_sd)); meta[[k]]<-a }
  mxT <- mean(etas)                                   # this rep's true mean baseline
  for (k in seq_along(meta)) {
    a <- meta[[k]]; eta <- etas[k]
    pc <- plogis(eta); pa <- plogis(eta + d_true[a] + B_true*(eta - mxT))
    rows[[k]] <- list(study=sprintf("S%02d",k), treatmentA="A",
                      eventsA=rbinom(1,narm,pc), nA=narm,
                      treatmentB=a, eventsB=rbinom(1,narm,pa), nB=narm)
  }
  jsonlite::write_json(rows, sprintf("/tmp/brmc/rep_%03d.json", rep), auto_unbox=TRUE)
}
suppressMessages(library(jsonlite))
for (r in 1:NREP) writeRep(r)
write_json(list(NREP=NREP, B=B_true, d=as.list(d_true), treats=c("A",actives)),
           "/tmp/brmc/truth.json", auto_unbox=TRUE)
writeLines(as.character(NREP), "/tmp/brmc/nrep.txt")
cat(sprintf("wrote %d reps to /tmp/brmc  (truth B=%.2f d=[B %.2f, C %.2f])\n",
            NREP, B_true, d_true["B"], d_true["C"]))
