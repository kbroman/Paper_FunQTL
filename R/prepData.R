# drop the parental lines and save the rest to a .RData file
library(qtl)
spal <- read.cross("csv", "", "../Datas/input_rev.csv", genotypes=c("A","B"), na.strings="*")
n_uniq_geno <- apply(pull.geno(spal), 1, function(a) length(unique(a[!is.na(a)])))
spal <- spal[,n_uniq_geno > 1]
spal <- convert2riself(spal)
spal <- jittermap(spal)
save(spal, file="../RDatas/spal_data.RData")
