library(qtl)
library(funqtl)

# load first replicate of Spalding data
spal <- read.cross("csv", "", "../Datas/input_rev.csv", genotypes=c("A","B"), na.strings="*")
spal <- convert2riself(spal) # no fcn exist
spal <- jittermap(spal)
#spal <- fill.geno(spal, method=c("imp"))
#spal <- calc.genoprob(spal, step=1)

# drop the two inbred strains
spal <- spal[, c(-(nind(spal)-1), -nind(spal))]

# jitter positions for markers on top of one another
spal <- jittermap(spal)

# grab genetic map
map <- pull.map(spal)

# QTL locations
qtl <- rbind(c(1, 61),
             c(3, 76),
             c(4, 40))

qtlnames <- paste0("QTL", 1:nrow(qtl))

# add QTL positions to the map
mapwqtl <- map
for(i in 1:nrow(qtl)) {
  chr <- qtl[i,1]
  pos <- qtl[i,2]
  mapwqtl[[chr]] <- c(mapwqtl[[chr]], pos)

  # add name
  names(mapwqtl[[chr]])[length(mapwqtl[[chr]])] <- qtlnames[i]

  # sort
  mapwqtl[[chr]] <- sort(mapwqtl[[chr]])
}

# simulate RIL

V <- (1:241)/241

rnormMulti <-
    function(n, V)
    matrix(rnorm(ncol(V)*n), ncol=ncol(V)) %*% chol(V)

poly <- function(beta,tt)  # legendre polynomials
    beta[1] + beta[2]*tt + beta[3]*tt^2 + beta[4]*tt^3



# generate data set with sample size 162 or 324.
genD <- function(sampsize) {
    ril <- sim.cross(mapwqtl, n.ind=sampsize, type="riself")
                                        # pull out genotypes for QTL
    qtlgeno <- pull.geno(ril)[, qtlnames]
                                        # drop QTL from cross object
    ril <- drop.markers(ril, qtlnames)

# same pattern of missing data as in spalding data set

    if(sampsize==162) {
        for(i in 1:nchr(ril))
            ril$geno[[i]]$data[is.na(spal$geno[[i]]$data)] <- NA
    } else {
        for(i in 1:nchr(ril))
            ril$geno[[i]]$data[rbind(is.na(spal$geno[[i]]$data),is.na(spal$geno[[i]]$data))] <- NA
    }

    m.beta <- c( -0.2677979, -264.5592720,  228.2765707,  -59.4271070)
    covv <- cbind( c(58.99011,  -177.7696,   185.106,   -45.43739),
                  c(-177.76965,  3848.7050, -7274.829,  3595.37470),
                  c(185.10603, -7274.8290, 16897.558, -9702.32353),
                  c(-45.43739,  3595.3747, -9702.324,  6096.71405) )


    Q1 <- c(0.2125015,  8.7758669,  1.2101953, -8.7782531)
    Q2 <- c(-1.909488,  3.395117, -4.683843,  2.680493)
    Q3 <- c(2.481852,   8.988952, -23.307630,  12.737423)



    AA <- (qtlgeno - 1.5 )*2
    ttt <- 1:241
    xi <- rnormMulti(sampsize,covv)
    Dat <- NULL;
    for( i in 1:sampsize) {
        Dat <- rbind(Dat,
                     m.beta + Q1 * AA[i,1] + Q2 * AA[i,2] + Q3*AA[i,3] + xi[i,] )
    }

    pheno1 <- NULL;
    for(i in 1:sampsize) {
        pheno1 <- rbind(pheno1, poly(Dat[i,],V) + rnorm(241, sd = 1) )
    }

    ril$pheno <- data.frame(pheno1)

    ril
}
