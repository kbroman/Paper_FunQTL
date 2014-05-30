library(qtl)
load("../RDatas/spal_data.RData")
spal <- calc.genoprob(spal, step=1)

slodqtl <- makeqtl(spal, c(1, 4), c(61, 42), what="prob")
mlodqtl <- makeqtl(spal, c(1, 3, 4), c(61, 76.1, 42), what="prob")

slodeff <- vector("list", nphe(spal))
mlodeff <- vector("list", nphe(spal))
for(i in 1:nphe(spal)) {
  slodeff[[i]] <- summary(fitqtl(spal, phe=i, qtl=slodqtl, method="hk", get.ests=TRUE, dropone=FALSE))$ests[,1]*c(1,2,2)
  mlodeff[[i]] <- summary(fitqtl(spal, phe=i, qtl=mlodqtl, method="hk", get.ests=TRUE, dropone=FALSE))$ests[,1]*c(1,2,2,2)
}

nam <- names(slodeff[[1]])
slodeff <- matrix(unlist(slodeff), byrow=TRUE, ncol=length(nam))
colnames(slodeff) <- nam

nam <- names(mlodeff[[1]])
mlodeff <- matrix(unlist(mlodeff), byrow=TRUE, ncol=length(nam))
colnames(mlodeff) <- nam

time <- (0:240)/30

postscript("../Figs/fig4.eps", height=6.5, width=6.5, pointsize=14, onefile=FALSE, horizontal=FALSE)

xd <- 0.05
yd <- 0.12

par(mfrow=c(2,2))
par(las=1)
plot(time, slodeff[,1], lwd=2, type="l",
     xlab="Time (hours)",
     ylab="Tip angle (degrees)", col="red", ylim=c(-110,0))
lines(time, mlodeff[,1], lwd=2, lty=2, col="blue")
mtext("baseline curve", side=3, line=0.5)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "A", font=2, xpd=TRUE)

plot(time, slodeff[,2], lwd=2, ylim = c(-5,9), type="l",
     xlab="Time (hours)",
     ylab="QTL effect", col="red")
lines(time, mlodeff[,2], lwd=2, lty=2, col="blue")
abline(h=0)
mtext("chr 1, 61 cM", side=3, line=0.5)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "B", font=2, xpd=TRUE)

plot(time, mlodeff[,3], lwd=2, ylim = c(-5,9), lty=2, type="l",
     xlab="Time (hours)",
     ylab="QTL effect", col="blue")
abline(h=0)
mtext("chr 3, 76 cM", side=3, line=0.5)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "C", font=2, xpd=TRUE)

plot(time, slodeff[,3], lwd=2, ylim = c(-5,9), type="l",
     xlab="Time (hours)",
     ylab="QTL effect", col="red")
lines(time, mlodeff[,4], lwd=2, lty=2, col="blue")
mtext("chr 4, 42 cM", side=3, line=0.5)
abline(h=0)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "D", font=2, xpd=TRUE)

dev.off()
