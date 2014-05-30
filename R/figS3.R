logisticFun <- function(beta,tt)
  {
    beta[1] / (1+ beta[2]*exp(-beta[3]*tt))
  }


beta.coef <- rbind( c(29, 7, .7),
                   c(28.5, 6.5, .73),
                   c(27.5, 5, .75)
                   )

tt <- 0:9

postscript("../Figs/figS3.eps", height=6, width=6, pointsize=11.2, onefile=FALSE, horizontal=FALSE)

for(i in 1:3) {
  par(las=1)
  plot(tt, logisticFun(beta.coef[i,],tt) , type = "l", ylim = c(0,30),
       col = c("black", "red", "blue")[i], lwd=2,
       xlab = "Time", ylab = "Average phenotypes")

  if(i != 3) par(new = T)
}

legend(7,10, col = rev(c("black", "red", "blue")), lwd=2, legend = rev(c("AA", "AB", "BB")) )

dev.off()



