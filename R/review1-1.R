library(qtl)
library(funqtl)
library(fda)
source("fr.R")
source("logistic.R")

load("../RDatas/spal_data.RData")

spal <- calc.genoprob(spal, step=1)

t1.n <- proc.time()
outout1 <- scanoneF(spal, pheno.cols=1:241, method="hk")
t2.n <- proc.time()
t2.n[1] - t1.n[1]

pheno <- spal$pheno
tt <- 1:(ncol(pheno))
phi5 <-  bs(tt, df=5, intercept = FALSE)

t1.s1 <- proc.time()
D1ss.out <- funcScanone(pheno, spal, phi5, crit = "ss")
t2.s1 <- proc.time()
t2.s1[1] - t1.s1[1]

t1.s2 <- proc.time()
D1qf.out <- funcScanone(pheno, spal, phi5, crit = "qf")
t2.s2 <- proc.time()
t2.s2[1] - t1.s2[1]


# computing time for our method
(ours <- t2.n[1] - t1.n[1])
# .78 in my computer

# computing time for sen's method
(sen1 <- t2.s1[1] - t1.s1[1])
(sen2 <- t2.s2[1] - t1.s2[1])
# 2.86 and 206.791 in my computer

sen1 / ours
sen2 / ours
# our method 3.67 times faster than "ss(wald)" method
# our method 265 times faster than "qf(residual)" method

# They used "fda" package in their program. Treating functional itself takes lots of time.
