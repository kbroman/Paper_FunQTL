covAutocor <- function(u,r)
{
  toeplitz(exp(log(r)*abs(u-u[1])))
}

logisticFun <- function(beta,tt)
  {
    ## a/(1+b*exp(-c*t))
    ## initial value = a/(1+b)
    ## asymptotic limit = a
    ## rate of growth = c
    beta[1] / (1+ beta[2]*exp(-beta[3]*tt))
  }
rnormAutocor <- function(n,u,r,s2=1)
  {
    sigma <- s2*covAutocor(u,r)
    ee <- rnormMulti(n,sigma)
    ee
  }

## function to simulate a multivariate normal distribution with
## an arbitrary covariance matrix (and zero mean)
rnormMulti <- function(n,sigma)
  {
    ## d <- dim(sigma)
    ## if(d[1]!=d[2])
    ##   stop("Sigma not square.")
    ## else

    ## chol() checks for square size in addition to positive-definiteness
    p <- nrow(sigma)
    ee <- rnorm(n*p)
    A <- Matrix::chol(sigma)
    ee <- matrix(ee,nrow=n)%*%A

    return(ee)
  }

structured.cov <- matrix(
c(0.72, 0.39, 0.45, 0.48, 0.50, 0.53, 0.60, 0.64, 0.68, 0.68,
  0.39, 1.06, 1.61, 1.60, 1.50, 1.48, 1.55, 1.47, 1.35, 1.29,
  0.45, 1.61, 3.29, 3.29, 3.17, 3.09, 3.19, 3.04, 2.78, 2.53,
  0.48, 1.60, 3.29, 3.98, 4.07, 4.01, 4.17, 4.18, 4.00, 3.69,
  0.50, 1.50, 3.17, 4.07, 4.70, 4.68, 4.66, 4.78, 4.70, 4.36,
  0.53, 1.48, 3.09, 4.07, 4.68, 5.56, 6.23, 6.87, 7.11, 6.92,
  0.60, 1.55, 3.19, 4.17, 4.66, 6.23, 8.59, 10.16,10.80,10.70,
  0.64, 1.47, 3.04, 4.18, 4.78, 6.87, 10.16,12.74,13.80,13.80,
  0.68, 1.35, 2.78, 4.00, 4.70, 7.11, 10.80,13.80,15.33,15.35,
  0.68, 1.29, 2.53, 3.69, 4.36, 6.92, 10.70,13.80,15.35,15.77), byrow=T, nrow=10)


# Return a simulation data set.
# sample.size = sample size
# cov.fcn = covariance function
# beta.coef = beta coefficient
# er = c parameter in the paper
# gitter = sd of normal noise

gen.data3 <- function(sample.size, cov.fcn, beta.coef,er, gitterr = 0){
  ##popu.size <- 10000  # population size
  tt = 0:9
  len.tt = length(tt)
  ## simulate genotypes
  mp <- sim.map(100, n.mar=6, include.x=F, eq.spacing=T) # simulate map
  md <- c(1,32,0,0)                       # one QTL at 32cM on chrom. 1
  samples <- sim.cross(map=mp, model=md, type='f2', n.ind=sample.size, keep.qtlgeno=T)

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



# Return list of location and lod scores that maximize slod, mlod, sens(ss), sens(qf) and multi-trait qtl methods.
# iter = iteration
# N = Number of sampled individuals
# method = variance structure
# beta.coef = beta coefficient
# er = c parameter in the paper
# gi = normal noise
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


# Return list of location and lod scores that maximize slod, mlod, sens(ss), sens(qf) and multi-trait qtl methods after permutation.
# The purpose of this function is to get permutation threshold for each methods

# iter = iteration
# N = Number of sampled individuals
# method = variance structure
# beta.coef = beta coefficient
# er = c parameter in the paper
# gi = normal noise
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

# print TP, FP, FN, TN, Power, rmse.
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

# function to calculate heritability scores.
herits3 <- function(Dat) {
    out <- NULL;
    for(i in 1:10) {
        out1 <- scanone(Dat, pheno.col=i, method="hk")
        out <- c(out, 1 - 10^(-2*out1[9,3] / 50000 ) )
    }
    out
}

