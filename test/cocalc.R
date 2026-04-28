treat1 <- c("A+B","A+B","A+B")
treat2 <- c("B","B","B")
TE <- c(-0.1,-0.3,-0.2)
seTE <- c(0.1,0.1,0.1)
studlab <- 1:3
net1 <- netmeta(TE, seTE, treat1, treat2, studlab, sm = "RD", ref = "B", random = FALSE)
net1
cm1 <- netcomb(net1)
summary(cm1)
forest(cm1)