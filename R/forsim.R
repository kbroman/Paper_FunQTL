gen.data3 <- function(sample.size, cov.fcn, beta.coef,er, gitterr = 0){
  ##popu.size <- 10000  # population size

  ## simulate genotypes
  mp <- sim.map(100, n.mar=6, include.x=F, eq.spacing=T) # simulate map
  md <- c(1,32,0,0)                       # one QTL at 32cM on chrom. 1
  samples <- sim.cross(map=mp, model=md, type='f2', n.ind=sample.size, keep.qtlgeno=T)

  ## retrieve sample genotypes
  ## ind <- sample(popu.size, sample.size)
  ## samples <- subset.cross(cross, ind=ind)
  ## ## subsetting qtlgeno by hand since subset.cross does not do it.
  ## qtlgeno <- samples$qtlgeno[ind]
  ## samples$qtlgeno <- qtlgeno

  ## simulate phenotypes
  ## 1. get the means
  mean.vals <- matrix(0., nrow=3, ncol=len.tt) # only 3 genotypes means only 3 mean curves
  for (i in 1:3){
    mean.vals[i,] <- logisticFun(beta.coef[i,],tt)
  }
  sample.means <- mean.vals[samples$qtlgeno,]
  ## 2. get the noises
  if (cov.fcn == 'autocorr'){           # case (1)
    ee <- rnormAutocor(sample.size, tt, 0.6, 3.0*er) # rho = 0.6, sigma^2 = 3
  }
  else if (cov.fcn == 'equicorr'){      # case (2)
    cov <- matrix(0.5, nrow=len.tt, ncol=len.tt)  # rho = 0.5
    diag(cov) <- 1.0
    cov <- 3.0 * cov                    # sigma^2 = 3.0
    ee <- rnormMulti(sample.size, cov*er)
  }
  else if (cov.fcn == 'structured'){    # case(3)
    ee <- rnormMulti(sample.size, structured.cov*er)
  }
  else{
    stop('Unknown covariance function.')
  }
  if (gitterr == 0 ) {
      gitterrr <- 0
  } else {
      gitterrr <- matrix(rnorm(sample.size*10, mean = 0, sd = gitterr), sample.size,10)
  }

  Y <- sample.means + ee + gitterrr

  samples$pheno <- data.frame(Y)
  return(samples)
}




compsim3 <- function(iter, N, method, beta.coef,er,gi = 0 ) {
    results = NULL;
    resultm = NULL;
    resultss = NULL;
    resultqf = NULL;
    resultMq = NULL;
    for(rep in 1:iter) {
        D1 <- gen.data3(N, method, beta.coef,er, gitterr = gi)
        D1 <- calc.genoprob(D1, step=2)
        D1.out <- scanoneF(D1, pheno.cols=1:10, method="hk")

        pheno <- D1$pheno;
        tt <- 1:(ncol(pheno))
        phi5 <-  bs(tt, df=5, intercept = FALSE)
        D1ss.out <- funcScanone(pheno, D1, phi5, crit = "ss")
        D1qf.out <- funcScanone(pheno, D1, phi5, crit = "qf")

        Myo <- grofit.control( model.type=c("logistic"), interactive = FALSE)
        time <- data.frame(matrix(rep(c(0:9), N), N, 10, byrow = T))
        DDD <- data.frame(a = 0, b= 0, c = 0 , D1$pheno)
        est <- grofit(time = time, data = DDD, control = Myo )
        Y = NULL
        for(i in 1:N) {
            Y <- rbind(Y,
                       c(est$gcFit$gcFittedModels[[i]]$parameters$A,
                         est$gcFit$gcFittedModels[[i]]$parameters$mu,
                         est$gcFit$gcFittedModels[[i]]$parameters$lambda )
                       )
        }

        if ( any(is.na(Y)) == T ) {
            na.loc <- which(is.na(Y[,1]) == T)
            Y <- Y[-na.loc,]
            temp <- D1
            temp <- subset(temp, ind = -na.loc)
            D1Mq <- scanoneM(temp, Y = Y, method = "hk")
        } else {
            D1Mq <- scanoneM(D1, Y = Y, method = "hk")
        }

        locamult <- D1Mq[which(D1Mq[,3] == max(D1Mq[,3])),c(1,2,3)]

        locass <- D1ss.out[which(D1ss.out$lod == max(D1ss.out$lod)), 1:3]
        locaqf <- D1qf.out[which(D1qf.out$lod == max(D1qf.out$lod)), 1:3]

        locas <- D1.out[which(D1.out$slod == max(D1.out$slod)),1:3]
        locam <- D1.out[which(D1.out$mlod == max(D1.out$mlod)),c(1,2,4)]
                                        #        if(loca[1] == 1)


        results <- rbind(results, as.vector(locas))
        resultm <- rbind(resultm, as.vector(locam))
        resultss <- rbind(resultss, as.vector(locass))
        resultqf <- rbind(resultqf, as.vector(locaqf))

        resultMq <- rbind(resultMq, as.vector(locamult) )
    }

    out <- list( slod = results, mlod = resultm, ss = resultss, qf = resultqf, mult = resultMq)
}

permsim3 <- function(iter, N, method, beta.coef,er, gi = 0 ) {
    results = NULL;
    resultm = NULL;
    resultss = NULL;
    resultqf = NULL;
    resultMq = NULL;
    for(rep in 1:iter) {
        D1 <- gen.data3(N, method, beta.coef,er, gitterr = gi)
        o <- sample(N)

        pheno <- D1$pheno[o,];
        D1$pheno <-  pheno

        D1 <- calc.genoprob(D1, step=4)
        D1.out <- scanoneF(D1, pheno.cols=1:10, method="hk")

        tt <- 1:(ncol(pheno))
        phi5 <-  bs(tt, df=5, intercept = FALSE)
        D1ss.out <- funcScanone(pheno, D1, phi5, crit = "ss")
        D1qf.out <- funcScanone(pheno, D1, phi5, crit = "qf")


        Myo <- grofit.control( model.type=c("logistic"), interactive = FALSE)
        time <- data.frame(matrix(rep(c(0:9), N), N, 10, byrow = T))
        DDD <- data.frame(a = 0, b= 0, c = 0 , D1$pheno)
        est <- grofit(time = time, data = DDD, control = Myo )
        Y = NULL
        for(i in 1:N) {
            Y <- rbind(Y,
                       c(est$gcFit$gcFittedModels[[i]]$parameters$A,
                         est$gcFit$gcFittedModels[[i]]$parameters$mu,
                         est$gcFit$gcFittedModels[[i]]$parameters$lambda )
                       )
        }

        if ( any(is.na(Y)) == T ) {
            na.loc <- which(is.na(Y[,1]) == T)
            Y <- Y[-na.loc,]
            temp <- D1
            temp <- subset(temp, ind = -na.loc)
            D1Mq <- scanoneM(temp, Y = Y, method = "hk")
        } else {
            D1Mq <- scanoneM(D1, Y = Y, method = "hk")
        }

        locamult <- D1Mq[which(D1Mq[,3] == max(D1Mq[,3])),c(1,2,3)]



        locass <- D1ss.out[which(D1ss.out$lod == max(D1ss.out$lod)), 1:3]
        locaqf <- D1qf.out[which(D1qf.out$lod == max(D1qf.out$lod)), 1:3]

        locas <- D1.out[which(D1.out$slod == max(D1.out$slod)),1:3]
        locam <- D1.out[which(D1.out$mlod == max(D1.out$mlod)),c(1,2,4)]
                                        #        if(loca[1] == 1)
        results <- rbind(results, as.vector(locas))
        resultm <- rbind(resultm, as.vector(locam))
        resultss <- rbind(resultss, as.vector(locass))
        resultqf <- rbind(resultqf, as.vector(locaqf))
        resultMq <- rbind(resultMq, as.vector(locamult) )

    }

    out <- list( slod = results, mlod = resultm, ss = resultss, qf = resultqf, mult =resultMq)
}

prtout <- function( res, lodcrit, window ) {
    respos <- res[res[,1] == "1", 2]
    reslod <- res[res[,1] == "1", 3]

    Pos = reslod > lodcrit
    TP = ( abs(respos - 32) < window & reslod > lodcrit )
    FP = ( abs(respos - 32) >= window & reslod > lodcrit )
    FN = ( abs(respos - 32) < window & reslod <= lodcrit )
    TN = ( abs(respos - 32) >= window & reslod <= lodcrit )
    prtout <- list(length =  length(respos),
                   mean = mean(respos),
                   Pmean = mean(respos[reslod > lodcrit]),
                   sd = sd(respos),
                   rmse = sqrt(mean((respos-32)^2)),
                   TP = sum(TP), FP = sum(FP), FN = sum(FN), TN = sum(TN),
                   P = sum(Pos),
                   conf.int = binom.test(sum(Pos),10000)$conf.int
                   )
}


herits3 <- function(Dat) {
    out <- NULL;
    for(i in 1:10) {
        out1 <- scanone(Dat, pheno.col=i, method="hk")
        out <- c(out, 1 - 10^(-2*out1[9,3] / 50000 ) )
    }
    out
}

herits2 <- function(Dat) {

    out <- NULL;
    for(i in 1:10) {
        qtl1 <- makeqtl(Dat, chr = c(1),
                        pos = c(32), what = "prob")
        out.Dat <- fitqtl(Dat, pheno.col=i, qtl=qtl1, method="hk")

        out <- c(out, (out.Dat$result.full[1,2]) / (out.Dat$result.full[3,2]) )
    }
    out
}
