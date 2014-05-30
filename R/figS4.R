library(qtl)
load("../RDatas/herit.RData")

postscript("../Figs/figS4.eps", height=7.5, width=6, pointsize=11.2, onefile=FALSE, horizontal=FALSE)
par(mfrow=c(3,1), las=1)
plot(0:9, her.au[[1]], xlim=c(0,9), ylim=c(0,.3), type="l",
     col="black", main="Autocorrelated", ylab="Heritability",
     xlab="Time")
for(j in 2:4)
    lines(0:9, her.au[[j]], col=c("red", "blue", "green")[j-1], lwd=2)
legend(8,.3, col = c("black", "red", "blue", "green"), lwd=2, legend = c("c = 1", "c = 2", "c = 3", "c = 6") )


plot(0:9, her.eq[[1]], xlim=c(0,9), ylim=c(0,.3), type="l",
     col="black", main="Equicorrelated", ylab="Heritability",
     xlab="Time")
for(j in 2:4)
    lines(0:9, her.eq[[j]], col=c("red", "blue", "green")[j-1], lwd=2)
legend(8,.3, col = c("black", "red", "blue", "green"), lwd = 2, legend = c("c = 1", "c = 2", "c = 3", "c = 6") )

plot(0:9, her.st[[1]], xlim=c(0,9), ylim=c(0,.3), type="l",
     col="black", main="Unstructured", ylab="Heritability",
     xlab="Time")
for(j in 2:4)
    lines(0:9, her.st[[j]], col=c("red", "blue", "green")[j-1], lwd=2)
legend(8,.3, col = c("black", "red", "blue", "green"), lwd=2, legend = c("c = 0.5", "c = 1", "c = 2", "c = 3") )
dev.off()
