logisticFun <- function(beta,tt)
  {
    beta[1] / (1+ beta[2]*exp(-beta[3]*tt))
  }

beta.coef <- rbind( c(29, 7, .7),
                   c(28.5, 6.5, .73),
                   c(27.5, 5, .75)
                   )


postscript("../Figs/simulatedtrait.eps", height=6, width=6, pointsize=11.2, onefile=FALSE, horizontal=FALSE)
for(i in 1:3) {
    plot(tt, logisticFun(beta.coef[i,],tt) , type = "l", ylim = c(0,30), col = c("black", "red", "blue")[i], lty=c(1:3)[i], xlab = "Time", ylab = "Simulated Value", main = "Simulated trait curves" )
    if(i != 3) par(new = T)
}
legend(7,10, col = c("black", "red", "blue"), lty = 1:3, legend = c("QQ", "Qq", "qq") )

dev.off()

