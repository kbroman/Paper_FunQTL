library(qtl)

postscript("../Figs/fig3.eps", height=7, width=5, pointsize=12, onefile=FALSE, horizontal=FALSE)

par(mfrow=c(2,1))
par(las=1)

xd <- 0.05
yd <- 0.12

load("../RDatas/LODSset1q2.RData")
load("../RDatas/pfset1q2.RData")
plot(a2, ylim=c(0,9.5), ylab="Profile SLOD score")
mtext(side=3, "SLOD", line=0.5)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "A", font=2, xpd=TRUE)

load("../RDatas/LODSset1q3.RData")
load("../RDatas/pfset1q3.RData")
plot(a3, ylim=c(0,9.5), ylab="Profile MLOD score")
mtext(side=3, "MLOD", line=0.5)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "B", font=2, xpd=TRUE)

dev.off()
