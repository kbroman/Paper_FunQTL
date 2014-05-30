library(qtl)
load("QTLs.RData")

q1 <- NULL
q2 <- NULL
q3 <- NULL
q1lod <- NULL
q2lod <- NULL
q3lod <- NULL
for (i in 1:2000) {
    if (length(QTs[[i]]) != 0) {
        c1 = 0; c2 = 0; c3 = 0
        for(j in 1:QTs[[i]]$n.qtl) {
            if(QTs[[i]]$chr[j]=="1" && c1 == 0) {
                q1 <- c(q1, QTs[[i]]$pos[j])
                c1 = 1
            }
            if(QTs[[i]]$chr[j]=="3" && c2 == 0) {
                q2 <- c(q2, QTs[[i]]$pos[j])
                c2 = 1
            }
            if(QTs[[i]]$chr[j]=="4" && c3 == 0) {
                q3 <- c(q3, QTs[[i]]$pos[j])
                c3 = 1
            }
        }
    }
}


qq1 <- NULL ; qq1lod <- NULL
qq2 <- NULL ; qq2lod <- NULL
qq3 <- NULL ; qq3lod <- NULL
for (i in 1:2000) {
    if (length(QTm[[i]]) != 0) {
        c1 = 0; c2 = 0; c3 = 0
        for(j in 1:QTm[[i]]$n.qtl) {
            if(QTm[[i]]$chr[j]=="1" && c1 == 0) {
                qq1 <- c(qq1, QTm[[i]]$pos[j])
                c1 = 1
            }
            if(QTm[[i]]$chr[j]=="3" && c2 == 0) {
                qq2 <- c(qq2, QTm[[i]]$pos[j])
                c2 = 1
            }
            if(QTm[[i]]$chr[j]=="4" && c3 == 0) {
                qq3 <- c(qq3, QTm[[i]]$pos[j])
                c3 = 1
            }
        }
    }
}

q12 <- NULL ; q12lod <- NULL
q22 <- NULL ; q22lod <- NULL
q32 <- NULL ; q32lod <- NULL
for (i in 1:2000) {
    if (length(QTs2[[i]]) != 0) {
        c1 = 0; c2 = 0; c3 = 0
        for(j in 1:QTs2[[i]]$n.qtl) {

            if(QTs2[[i]]$chr[j]=="1" && c1 == 0) {
                q12 <- c(q12, QTs2[[i]]$pos[j])
                c1 = 1
            }
            if(QTs2[[i]]$chr[j]=="3" && c2 == 0) {
                q22 <- c(q22, QTs2[[i]]$pos[j])
                c2 = 1
            }
            if(QTs2[[i]]$chr[j]=="4" && c3 == 0 ) {
                q32 <- c(q32, QTs2[[i]]$pos[j])
                c3 = 1
            }
        }
    }
}


qq12 <- NULL ; qq12lod <- NULL
qq22 <- NULL ; qq22lod <- NULL
qq32 <- NULL ; qq32lod <- NULL
for (i in 1:2000) {
    if (length(QTm2[[i]]) != 0) {
        c1 = 0; c2 = 0; c3 = 0
        for(j in 1:QTm2[[i]]$n.qtl) {
            if(QTm2[[i]]$chr[j]=="1" && c1 == 0) {
                qq12 <- c(qq12, QTm2[[i]]$pos[j])
                c1 = 1
            }
            if(QTm2[[i]]$chr[j]=="3" && c2 == 0) {
                qq22 <- c(qq22, QTm2[[i]]$pos[j])
                c2 = 1
            }
            if(QTm2[[i]]$chr[j]=="4" && c3 == 0) {
                qq32 <- c(qq32, QTm2[[i]]$pos[j])
                c3 = 1
            }
        }
    }
}




## produce numbers in Table 1

length(q1)/2000;length(q2)/2000; length(q3)/2000
mean(q1);sd(q1)
mean(q2);sd(q2)
mean(q3);sd(q3)

length(qq1)/2000;length(qq2)/2000; length(qq3)/2000
mean(qq1);sd(qq1)
mean(qq2);sd(qq2)
mean(qq3);sd(qq3)



length(q12)/2000;length(q22)/2000; length(q32)/2000
mean(q12);sd(q12)
mean(q22);sd(q22)
mean(q32);sd(q32)

length(qq12)/2000;length(qq22)/2000; length(qq32)/2000
mean(qq12);sd(qq12)
mean(qq22);sd(qq22)
mean(qq32);sd(qq32)

