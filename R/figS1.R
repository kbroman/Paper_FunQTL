library(qtl)
load("../RDatas/spal_data.RData")
spal <- calc.genoprob(spal, step=1)

pheno1 <- spal$pheno

postscript("../Figs/figS1.eps", height=8, width=4, pointsize=12, onefile=FALSE, horizontal=FALSE)

par(mfrow=c(2,1), las=1, mar=c(5.1, 4.1, 1.1, 0.6))

map10 <- pull.map(spal)
plot(map10, main="")
u <- par("usr")
text(u[1]-diff(u[1:2])*0.2, u[4]+diff(u[3:4])*0.05, "A", font=2, xpd=TRUE)

plot((1:241)/30, pheno1[160,], type="n", xlab="Time (hours)", ylim=c(-120,0), ylab="Root Tip Angle (degrees)", main="",
     yaxs="i", xaxs="i")
for(i in c(19, 20, 132, 72, 160))
  lines((1:241)/30, pheno1[i,])
u <- par("usr")
text(u[1]-diff(u[1:2])*0.2, u[4]+diff(u[3:4])*0.05, "B", font=2, xpd=TRUE)


dev.off()
