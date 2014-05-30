library(qtl)
source("forsim.R")

beta.coef <- rbind( c(29, 7, .7),
                   c(28.5, 6.5, .73),
                   c(27.5, 5, .75)
                   )

her.au = NULL
D1 <- gen.data3(50000, "autocorr", beta.coef,1 )
D1 <- calc.genoprob(D1, step = 2)
her.au[[1]] = herits3(D1)

D1 <- gen.data3(50000, "autocorr", beta.coef,2 )
D1 <- calc.genoprob(D1, step = 2)
her.au[[2]] = herits3(D1)

D1 <- gen.data3(50000, "autocorr", beta.coef,3 )
D1 <- calc.genoprob(D1, step = 2)
her.au[[3]] = herits3(D1)

D1 <- gen.data3(50000, "autocorr", beta.coef,6 )
D1 <- calc.genoprob(D1, step = 2)
her.au[[4]] = herits3(D1)


her.eq <- NULL
D1 <- gen.data3(50000, "equicorr", beta.coef,1 )
D1 <- calc.genoprob(D1, step = 2)
her.eq[[1]] = herits3(D1)

D1 <- gen.data3(50000, "equicorr", beta.coef,2 )
D1 <- calc.genoprob(D1, step = 2)
her.eq[[2]] = herits3(D1)

D1 <- gen.data3(50000, "equicorr", beta.coef,3 )
D1 <- calc.genoprob(D1, step = 2)
her.eq[[3]] = herits3(D1)

D1 <- gen.data3(50000, "equicorr", beta.coef,6 )
D1 <- calc.genoprob(D1, step = 2)
her.eq[[4]] = herits3(D1)


her.st <- NULL
D1 <- gen.data3(50000, "structured", beta.coef,.5 )
D1 <- calc.genoprob(D1, step = 2)
her.st[[1]] = herits3(D1)

D1 <- gen.data3(50000, "structured", beta.coef,1 )
D1 <- calc.genoprob(D1, step = 2)
her.st[[2]] = herits3(D1)

D1 <- gen.data3(50000, "structured", beta.coef,2 )
D1 <- calc.genoprob(D1, step = 2)
her.st[[3]] = herits3(D1)

D1 <- gen.data3(50000, "structured", beta.coef,3 )
D1 <- calc.genoprob(D1, step = 2)
her.st[[4]] = herits3(D1)

postscript("../Figs/herit.eps", height=6, width=6, pointsize=11.2, onefile=FALSE, horizontal=FALSE)
par(mfrow=c(3,1))
plot(0:9, her.au[[1]], xlim=c(0,9), ylim=c(0,.3), type="l",
     col="black", main="Autocorrelated", ylab="Heritability",
     xlab="Time")
for(j in 2:4)
    lines(0:9, her.au[[j]], col=c("red", "blue", "green")[j-1], lty = j)
legend(8,.3, col = c("black", "red", "blue", "green"), lty = c(1,2,3,4), legend = c("c = 1", "c = 2", "c = 3", "c = 6") )


plot(0:9, her.eq[[1]], xlim=c(0,9), ylim=c(0,.3), type="l",
     col="black", main="Equicorrelated", ylab="Heritability",
     xlab="Time")
for(j in 2:4)
    lines(0:9, her.eq[[j]], col=c("red", "blue", "green")[j-1], lty = j)
legend(8,.3, col = c("black", "red", "blue", "green"), lty = c(1,2,3,4), legend = c("c = 1", "c = 2", "c = 3", "c = 6") )

plot(0:9, her.st[[1]], xlim=c(0,9), ylim=c(0,.3), type="l",
     col="black", main="Unstructured", ylab="Heritability",
     xlab="Time")
for(j in 2:4)
    lines(0:9, her.st[[j]], col=c("red", "blue", "green")[j-1], lty = j)
legend(8,.3, col = c("black", "red", "blue", "green"), lty = c(1,2,3,4), legend = c("c = 0.5", "c = 1", "c = 2", "c = 3") )
dev.off()
