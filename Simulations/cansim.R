source("forsim2.R")

## calculate thresholds for 162 samples and 324 samples
th1 <- NULL;
th2 <- NULL;
for( i in 1:1000) {
    D1 <- genD(162)
    D1 <- calc.genoprob(D1, step = 2 )
    o <- sample(162)

    D1$pheno <- D1$pheno[o,]

    res <- scanoneF(D1, pheno.cols=1:241, method="hk")

    th1 <- c(th1, max(res[,3]))
    th2 <- c(th2, max(res[,4]))
}

th1.2 <- NULL;
th2.2 <- NULL;
for( i in 1:1000) {
    D1 <- genD(162*2)
    D1 <- calc.genoprob(D1, step = 2 )
    o <- sample(162*2)

    D1$pheno <- D1$pheno[o,]
    res <- scanoneF(D1, pheno.cols=1:241, method="hk")

    th1.2 <- c(th1.2, max(res[,3]))
    th2.2 <- c(th2.2, max(res[,4]))
}

quantile(th1,.95)
quantile(th2,.95)

quantile(th1.2,.95)
quantile(th2.2,.95)

# slod : 1.91
# mlod : 3.35

## This takes long.!!!
# Save stepwiseqtl search result of 162 sampled data using "slod" method in QTs
QTs <- NULL;
for(rep in 1:2) {
    D1 <- genD(162)
    D1 <- calc.genoprob(D1, step = 2 )

    res <- stepwiseqtlF(D1, pheno.cols = 1:241 ,max.qtl=5 , usec = "slod", method = "hk", penalties = c(1.91, 5, 5) )
    QTLs[[rep]] <- res
}


# Save stepwiseqtl search result of 162 sampled data using "mlod" method in QTm
QTm <- NULL;
for(rep in 1:2) {
    D1 <- genD(162)
    D1 <- calc.genoprob(D1, step = 2 )

    res <- stepwiseqtlF(D1, pheno.cols = 1:241 ,max.qtl=5 , usec = "mlod", method = "hk", penalties = c(3.35, 5, 5) )
    QTLsm[[rep]] <- res
}

# Save stepwiseqtl search result of 324 sampled data using "slod" method in QTs2
QTs2 <- NULL;
for(rep in 1:2) {
    D1 <- genD(162*2)
    D1 <- calc.genoprob(D1, step = 2 )

    res <- stepwiseqtlF(D1, pheno.cols = 1:241 ,max.qtl=5 , usec = "slod", method = "hk", penalties = c(1.91, 5, 5) )
    QTLs[[rep]] <- res
}


# Save stepwiseqtl search result of 324 sampled data using "slod" method in QTm2
QTm2 <- NULL;
for(rep in 1:2) {
    D1 <- genD(162*2)
    D1 <- calc.genoprob(D1, step = 2 )

    res <- stepwiseqtlF(D1, pheno.cols = 1:241 ,max.qtl=5 , usec = "mlod", method = "hk", penalties = c(3.35, 5, 5) )
    QTLsm[[rep]] <- res
}

# 2000 of simulation results are saved in QTLs.RData
