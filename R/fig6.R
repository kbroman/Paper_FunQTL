load("../RDatas/newrmsepower.RData")
load("../RDatas/newrmsepowermore.RData")
x <- list( c(0.06566826, 0.03637798, 0.02400827, 0.01377438) * 100,
          c(0.06749013, 0.03547250, 0.02556244, 0.01320752) * 100,
          c(0.12273658, 0.07119094, 0.04024743, 0.02562654) * 100 )

postscript("../Figs/fig6.eps", height=6, width=6, pointsize=11.2, onefile=FALSE, horizontal=FALSE)

aut200power <- lapply(list(aut200powersa, aut200powerse, aut200powerss), function(a) a*100)
aut200powergi1 <- lapply(list(aut200powersagi1, aut200powersegi1, aut200powerssgi1), function(a) a*100)
aut200powergi2 <- lapply(list(aut200powersagi2, aut200powersegi2, aut200powerssgi2), function(a) a*100)

par(mfrow=c(3,3), las=1, xaxs="i", yaxs="i",
    mar=c(5.1, 4.1, 2.1, 1.1), oma=c(0, 3.1, 2.1, 0.0))

for(i in 1:3) {
  plot(x[[i]], aut200power[[i]][5,], xlim=c(0,13), ylim=c(0,100), type="l",
       col="black", main="", ylab="Power",
       xlab="Percent variance explained by QTL")
  for(j in 1:4)
    lines(x[[i]], aut200power[[i]][j,], col=c("red", "blue", "green", "brown")[j])

  mtext(side=2, c("Autocorrelated", "Equicorrelated", "Unstructured")[i], line=5.5, las=0, col="darkslateblue")
  if(i==1) mtext(side=3, "noise = 0", line=2, col="darkslateblue")

  plot(x[[i]], aut200powergi1[[i]][5,], xlim=c(0,13), ylim=c(0,100), type="l",
       col="black", main="", ylab="Power",
       xlab="Percent variance explained by QTL")
  for(j in 1:4)
    lines(x[[i]], aut200powergi1[[i]][j,], col=c("red", "blue", "green", "brown")[j])
  if(i==1) mtext(side=3, "noise = 1", line=2, col="darkslateblue")


  plot(x[[i]], aut200powergi2[[i]][5,], xlim=c(0,13), ylim=c(0,100), type="l",
       col="black", main="", ylab="Power",
       xlab="Percent variance explained by QTL")
  for(j in 1:4)
    lines(x[[i]], aut200powergi2[[i]][j,], col=c("red", "blue", "green", "brown")[j])

  if(i==1) mtext(side=3, "noise = 2", line=2, col="darkslateblue")
}

dev.off()
