#Replicate continuousFixedTau test
library(devtools)
#library(nmadb)
library(netmeta)

#catalog <- getNMADB()

#conts <- subset(catalog, Verified=="True"&Effect.Measure=="mean difference")

#conts$Record.ID

#contNoMultiarm <- readByID(501408)$data
#net1 <- runnetmeta(501408, model="fixed")
#net2 <- runnetmeta(501408, model="random")
setwd("~/Sync/Documents/meta-analysis/test")

contNoMultiarm <- read.csv("contNoMultiarm.csv")

contNoMultiarm$study <- contNoMultiarm$id
contNoMultiarm$treatment <- contNoMultiarm$t
contNoMultiarm$mean <- contNoMultiarm$y

constudies <- pairwise( treat=t
                      , n=n
                      , mean=y
                      , sd=sd
                      , studlab=id
                      , data=contNoMultiarm
                      , sm="MD"
                      )
print(constudies)

net2 <- netmeta(data=constudies
               ,TE=TE
               ,seTE=seTE
               ,studlab=study
               ,treat1=treat1
               ,treat2=treat2
               ,details.chkmultiarm = T
               ,sm="MD"
               ,method.tau="REML"
               )

#library(jsonlite)
#write_json(contNoMultiarm, path="contNoMultiarm.json")

print(net2$TE.random)
print(net2$seTE.random[,1]^2)
