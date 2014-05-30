library(qtl)
source("forsim.R")
beta.coef <- rbind( c(29, 7, .7),
                   c(28.5, 6.5, .73),
                   c(27.5, 5, .75)
                   )

Daut1 <- gen.data3(50000, "autocorr", beta.coef,1 )
Daut1 <- calc.genoprob(Daut1, step = 4)

Daut2 <- gen.data3(50000, "autocorr", beta.coef,2 )
Daut2 <- calc.genoprob(Daut2, step = 4)

Daut3 <- gen.data3(50000, "autocorr", beta.coef,3 )
Daut3 <- calc.genoprob(Daut3, step = 4)

Deq1 <- gen.data3(50000, "equicorr", beta.coef,1 )
Deq1 <- calc.genoprob(Deq1, step = 4)

Deq2 <- gen.data3(50000, "equicorr", beta.coef,2 )
Deq2 <- calc.genoprob(Deq2, step = 4)

Deq3 <- gen.data3(50000, "equicorr", beta.coef,3 )
Deq3 <- calc.genoprob(Deq3, step = 4)

Dst1 <- gen.data3(50000, "structured", beta.coef,1 )
Dst1 <- calc.genoprob(Dst1, step = 4)

Dst2 <- gen.data3(50000, "structured", beta.coef,2 )
Dst2 <- calc.genoprob(Dst2, step = 4)

Dst3 <- gen.data3(50000, "structured", beta.coef,3 )
Dst3 <- calc.genoprob(Dst3, step = 4)

Daut4 <- gen.data3(50000, "autocorr", beta.coef,6 )
Daut4 <- calc.genoprob(Daut4, step = 4)

Deq4 <- gen.data3(50000, "equicorr", beta.coef,6 )
Deq4 <- calc.genoprob(Deq4, step = 4)

Dst4 <- gen.data3(50000, "structured", beta.coef,0.5 )
Dst4 <- calc.genoprob(Dst4, step = 4)

her.au = NULL
her.eq = NULL
her.st = NULL
for(i in 1:4) {
    dataname1 = paste("Daut", i, sep="")
    dataname2 = paste("Deq", i, sep="")
    dataname3 = paste("Dst", i, sep="")
    her.au[[i]] = herits3(dataname1)
    her.eq[[i]] = herits3(dataname2)
    her.st[[i]] = herits3(dataname3)
}

save(her.au, her.eq, her.st, file = "../RData/herit.RData")

# time points
t1 <- c(mean(herits3(Daut1)), mean(herits3(Daut2)), mean(herits3(Daut3)), mean(herits3(Daut4)) )

t2 <- c(mean(herits3(Deq1)), mean(herits3(Deq2)), mean(herits3(Deq3)), mean(herits3(Deq4)) )

t3 <- c(mean(herits3(Dst4)), mean(herits3(Dst1)), mean(herits3(Dst2)), mean(herits3(Dst3)) )

