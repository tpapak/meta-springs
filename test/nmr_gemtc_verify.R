suppressMessages(library(gemtc))
set.seed(1)
v1<-sqrt(0.05); v2<-sqrt(0.06)
armData <- data.frame(
  study=c('S1','S1','S2','S2','S3','S3','S4','S4','S5','S5','S6','S6'),
  treatment=c('A','B','A','B','A','C','A','C','B','C','B','C'),
  mean=c(0.00,1.03, 0.20,2.18, -0.10,1.26, 0.10,2.57, 1.30,1.72, 2.25,2.64),
  std.err=c(v1,v1, v1,v1, v2,v2, v2,v2, v1,v1, v1,v1),
  stringsAsFactors=FALSE)
studyData <- data.frame(study=c('S1','S2','S3','S4','S5','S6'),
                        x=c(0,1,0,1,0,1), stringsAsFactors=FALSE)

net <- mtc.network(data.ab=armData, studies=studyData)
reg <- list(coefficient='shared', variable='x', control='A')
model <- mtc.model(net, type='regression', regressor=reg,
                   likelihood='normal', link='identity', linearModel='fixed')
res <- mtc.run(model, n.adapt=20000, n.iter=80000, thin=2)
s <- summary(res)
cat("\n===== gemtc fixed-effect NMR (shared beta), covariate at x=0 =====\n")
print(round(s$summaries$statistics[, c('Mean','SD')], 3))
print(round(s$summaries$quantiles[, c('2.5%','50%','97.5%')], 3))
