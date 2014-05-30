library(qtl)
source("plotlod.R")

load("../RDatas/spal_data.RData")
file <- "../RDatas/spal_results.RData"
if(file.exists(file)) {
  load(file)
} else {
  spal <- calc.genoprob(spal, step=1)

  out <- scanone(spal, pheno.col = 1:nphe(spal), method="hk")
  phe <- as.matrix(spal$pheno)
  eff <- NULL
  for(i in 1:nchr(spal)) {
    cat(i,"\n")
    pr <- spal$geno[[i]]$prob[,,2]
    thiseff <- t(apply(pr, 2, function(a,b) lm(b~a)$coef[2,], phe))
    eff <- rbind(eff, thiseff)
  }
  save(out, eff, file=file)
}

postscript("../Figs/fig1.eps", height=6, width=6.5, pointsize=12, onefile=FALSE, horizontal=FALSE)

plotlod2(out, eff, gap=15)

dev.off()
