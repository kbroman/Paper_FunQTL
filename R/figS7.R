V <- (1:241)/241
m.beta <- c( -0.2677979, -264.5592720,  228.2765707,  -59.4271070)
covv <- cbind( c(58.99011,  -177.7696,   185.106,   -45.43739),
              c(-177.76965,  3848.7050, -7274.829,  3595.37470),
              c(185.10603, -7274.8290, 16897.558, -9702.32353),
              c(-45.43739,  3595.3747, -9702.324,  6096.71405) )


Q1 <- c(0.2125015,  8.7758669,  1.2101953, -8.7782531)
Q2 <- c(-1.909488,  3.395117, -4.683843,  2.680493)
Q3 <- c(2.481852,   8.988952, -23.307630,  12.737423)


polynn <- function(beta,tt)  # legendre polynomials
    beta[1] + beta[2]*tt + beta[3]*tt^2 + beta[4]*tt^3



postscript("../Figs/figS7.eps", height=6.5, width=6.5, pointsize=14, onefile=FALSE, horizontal=FALSE)

xd <- 0.05
yd <- 0.12

par(mfrow=c(2,2))
par(las=1)
plot(V,polynn(beta=m.beta,V), type="l", xlab="Time",ylab="Root Tip Angle", ylim=c(-100,0), lwd=2, col="blue")
mtext("baseline curve", side=3, line=0.5)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "A", font=2, xpd=TRUE)

plot(V, (polynn(m.beta+Q1,V) - polynn(m.beta,V)), type="l", xlab="Time",ylab="QTL effect", ylim=c(-5,5) ,lwd=2, col="blue" )
abline(h=0)
mtext("chr 1, 61 cM", side=3, line=0.5)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "B", font=2, xpd=TRUE)

plot(V, (polynn(m.beta+Q2,V) - polynn(m.beta,V)), type="l", xlab="Time", ylab="QTL effect", ylim=c(-5,5) ,lwd=2, col="blue")
abline(h=0)
mtext("chr 3, 76 cM", side=3, line=0.5)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "C", font=2, xpd=TRUE)


plot(V, (polynn(m.beta+Q3,V) - polynn(m.beta,V)), type="l",xlab="Time", ylab="QTL effect", ylim=c(-5,5) ,lwd=2, col="blue")
mtext("chr 4, 42 cM", side=3, line=0.5)
abline(h=0)
u <- par("usr")
text(u[1]-diff(u[1:2])*xd, u[4]+diff(u[3:4])*yd, "D", font=2, xpd=TRUE)

dev.off()
