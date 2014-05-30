library(qtl)
load("../RDatas/spal_data.RData")
pheno <- spal$pheno
cormat <- cor(pheno, use="pairwise")
times <- names(pheno)
times <- as.numeric(substr(times, 2, nchar(times)))/60 # times in hours

ave <- colMeans(pheno, na.rm=TRUE)
SD <- apply(pheno, 2, sd, na.rm=TRUE)

postscript("../Figs/figS2.eps", height=6.8, width=5.5, pointsize=12, onefile=FALSE, horizontal=FALSE)

par(mar=c(4.1, 4.1, 2.1, 2.1), las=1)

layout(rbind(c(rep(1,3), rep(2,3)), c(rep(3,5), 4)), height=c(1, 2))

plot(times, ave, lwd=2, type="l", xlab="Time (hours)", ylab="Ave. root tip angle (degrees)",
     col="slateblue", ylim=c(-100, 0), yaxs="i", xaxs="i")
u <- par("usr")
text(u[1]-diff(u[1:2])*0.2, u[4]+diff(u[3:4])*0.08, "A", font=2, xpd=TRUE, cex=1.1)
plot(times, SD, lwd=2, type="l", xlab="Time (hours)", ylab="SD root tip angle (degrees)",
     ylim=c(0, 11), yaxs="i", xaxs="i", col="slateblue")
u <- par("usr")
text(u[1]-diff(u[1:2])*0.2, u[4]+diff(u[3:4])*0.08, "B", font=2, xpd=TRUE, cex=1.1)


ncol=256
val <- sqrt(seq(0, 1, len=ncol))
col <- c(rgb(1, 1, 1), rgb(rev(val), rev(val), rev(val))[-1])

image(cormat, zlim=c(0,1), col=col, mgp=c(2.6, 1, 0), bty="n", xaxt="n", yaxt="n", xlab = "Time (hours)", ylab = "Time (hours)")
u <- par("usr")
text(u[1]-diff(u[1:2])*0.17*3/5, u[4]+diff(u[3:4])*0.04, "C", font=2, xpd=TRUE, cex=1.1)

t <- times/max(times)
tlab <- times
atpos <- c( which(tlab== 0), which(tlab == 2), which(tlab == 4), which(tlab == 6), which(tlab ==8))
axis(side=1, at = t[atpos], lab=pretty(tlab))
axis(side=2, at = t[atpos], lab=pretty(tlab))
u <- par("usr")
rect(u[1], u[3], u[2], u[4])


par(mar=c(8.1, 0.6, 4.6, 4.1))
  lodscale <- seq(0, 1, length=length(col))
  image(0, lodscale, rbind(lodscale), xaxt="n", xlab="", yaxt="n",
        col=col, zlim=c(0,1))

  axis(side=4, at=pretty(lodscale), lab=abs(pretty(lodscale)))
u <- par("usr")
rect(u[1], u[3], u[2], u[4])

dev.off()


     
