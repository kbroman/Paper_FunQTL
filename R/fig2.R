library(qtl)
load("../RDatas/spal_data.RData")
spal <- calc.genoprob(spal, step=1)

library(funqtl)
outout1 <- scanoneF(spal, pheno.cols=1:241, method="hk")

SLOD1 <- outout1[,1:3]
MLOD1 <- outout1[,c(1,2,4)]

load("../RDatas/paper1sen1.RData")

postscript("../Figs/fig2.eps", height=6.5, width=6.5, pointsize=14, onefile=FALSE, horizontal=FALSE)

thr <- c(1.85, 3.32, 5.72, 0.0559)
m <- 2

xd <- 0.05
yd <- 0.12

par(mfrow=c(2,2))
par(las=1)
plot(SLOD1, ylab="SLOD", ylim=c(0,thr[1]*m))
mtext(side=3, "SLOD", line=0.5)
abline(h = thr[1], col="red")
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "A", font=2, xpd=TRUE)

plot(MLOD1, ylab="MLOD", ylim=c(0, thr[2]*m))
mtext(side=3, "MLOD", line=0.5)
abline(h = thr[2], col="red")
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "B", font=2, xpd=TRUE)

plot(D1qf.out, ylab="EE(Wald)", ylim=c(0,thr[3]*m))
mtext(side=3, "EE(Wald)", line=0.5)
abline(h = thr[3], col="red")
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "C", font=2, xpd=TRUE)

plot(D1ss.out, ylab="EE(Residual)", ylim=c(0,thr[4]*m))
mtext(side=3, "EE(Residual)", line=0.5)
abline(h = thr[4], col="red")
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "D", font=2, xpd=TRUE)

dev.off()
