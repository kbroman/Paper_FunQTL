load("../RDatas/newrmsepower.RData")
x <- list( c(0.06566826, 0.03637798, 0.02400827, 0.01377438) * 100,
          c(0.06749013, 0.03547250, 0.02556244, 0.01320752) * 100,
          c(0.12273658, 0.07119094, 0.04024743, 0.02562654) * 100 )

postscript("../Figs/figS5.eps", height=6, width=6, pointsize=11.2, onefile=FALSE, horizontal=FALSE)

aut100rmse <- list(aut100rmsea, aut100rmsee, aut100rmses)
aut200rmse <- list(aut200rmsea, aut200rmsee, aut200rmses)
aut400rmse <- list(aut400rmsea, aut400rmsee, aut400rmses)

par(mfrow=c(3,3), las=1, xaxs="i", yaxs="i",
    mar=c(5.1, 4.1, 2.1, 1.1), oma=c(0, 3.1, 2.1, 0.0))

for(i in 1:3) {
  plot(x[[i]], aut100rmse[[i]][5,], xlim=c(0,13), ylim=c(0,40),
       type="l", col="black", main="", ylab="RMSE position (cM)", xlab="Percent variance explained by QTL")
  for(j in 1:4)
      lines(x[[i]], aut100rmse[[i]][j,], col=c("red", "blue", "green", "brown")[j])

  mtext(side=2, c("Autocorrelated", "Equicorrelated", "Unstructured")[i], line=5.5, las=0, col="darkslateblue")
  if(i==1) mtext(side=3, "n=100", line=2, col="darkslateblue")

  plot(x[[i]], aut200rmse[[i]][5,], xlim=c(0,13), ylim=c(0,40),
       type="l", col="black", main="", ylab="RMSE position (cM)", xlab="Percent variance explained by QTL")
  for(j in 1:4)
      lines(x[[i]], aut200rmse[[i]][j,], col=c("red", "blue", "green", "brown")[j])

  if(i==1) mtext(side=3, "n=200", line=2, col="darkslateblue")

  plot(x[[i]], aut400rmse[[i]][5,], xlim=c(0,13), ylim=c(0,40),
       type="l", col="black", main="", ylab="RMSE position (cM)", xlab="Percent variance explained by QTL")
  for(j in 1:4)
      lines(x[[i]], aut400rmse[[i]][j,], col=c("red", "blue", "green", "brown")[j])

  if(i==1) mtext(side=3, "n=400", line=2, col="darkslateblue")
}

dev.off()
