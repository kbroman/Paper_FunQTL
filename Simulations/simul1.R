library(qtl)
library(fda)
source("fr.R")
source("logistic.R")
source("comparison_yap.R")
source("forsim.R")
library(funqtl)
library(grofit)

beta.coef <- rbind( c(29, 7, .7),
                   c(28.5, 6.5, .73),
                   c(27.5, 5, .75)
                   )

load("outpoutresults.RData") # from simfile.R


out.100.au.1 <- out100aut1
out.100.eq.1 <- out100eq1
out.100.st.1 <- out100st1
out.400.au.1 <- out400aut1
out.400.eq.1 <- out400eq1
out.400.st.1 <- out400st1

out.100.au.2 <- out100aut2
out.100.eq.2 <- out100eq2
out.100.st.2 <- out100st2
out.400.au.2 <- out400aut2
out.400.eq.2 <- out400eq2
out.400.st.2 <- out400st2

out.100.au.3 <- out100aut3
out.100.eq.3 <- out100eq3
out.100.st.3 <- out100st3
out.400.au.3 <- out400aut3
out.400.eq.3 <- out400eq3
out.400.st.3 <- out400st3

out.100.au.4 <- out100aut4
out.100.eq.4 <- out100eq4
out.100.st.4 <- out100st4
out.400.au.4 <- out400aut4
out.400.eq.4 <- out400eq4
out.400.st.4 <- out400st4

out.200.au.1 <- out200aut1
out.200.eq.1 <- out200eq1
out.200.st.1 <- out200st1
out.200.au.2 <- out200aut2
out.200.eq.2 <- out200eq2
out.200.st.2 <- out200st2
out.200.au.3 <- out200aut3
out.200.eq.3 <- out200eq3
out.200.st.3 <- out200st3
out.200.au.4 <- out200aut4
out.200.eq.4 <- out200eq4
out.200.st.4 <- out200st4


out.200.au.1.gi1 <- out200aut1gi1
out.200.au.1.gi2 <- out200aut1gi2
out.200.eq.1.gi1 <- out200eq1gi1
out.200.eq.1.gi2 <- out200eq1gi2
out.200.st.1.gi1 <- out200st1gi1
out.200.st.1.gi2 <- out200st1gi2

out.200.au.2.gi1 <- out200aut2gi1
out.200.au.2.gi2 <- out200aut2gi2
out.200.eq.2.gi1 <- out200eq2gi1
out.200.eq.2.gi2 <- out200eq2gi2
out.200.st.2.gi1 <- out200st2gi1
out.200.st.2.gi2 <- out200st2gi2

out.200.au.3.gi1 <- out200aut3gi1
out.200.au.3.gi2 <- out200aut3gi2
out.200.eq.3.gi1 <- out200eq3gi1
out.200.eq.3.gi2 <- out200eq3gi2
out.200.st.3.gi1 <- out200st3gi1
out.200.st.3.gi2 <- out200st3gi2

out.200.au.4.gi1 <- out200aut4gi1
out.200.au.4.gi2 <- out200aut4gi2
out.200.eq.4.gi1 <- out200eq4gi1
out.200.eq.4.gi2 <- out200eq4gi2
out.200.st.4.gi1 <- out200st4gi1
out.200.st.4.gi2 <- out200st4gi2




# get permutation thresh holds
(slod.aut.100.1 <- quantile(as.vector(pout100aut1$slod[,3]), 0.95))
(mlod.aut.100.1 <- quantile(as.vector(pout100aut1$mlod[,3]), 0.95))
(ss.aut.100.1 <- quantile(as.vector(pout100aut1$ss[,3]), 0.95))
(qf.aut.100.1 <- quantile(as.vector(pout100aut1$qf[,3]), 0.95))
(mt.aut.100.1 <- quantile(as.vector(pout100aut1$mult[,3]), 0.95))

(slod.aut.100.2 <- quantile(as.vector(pout100aut2$slod[,3]), 0.95))
(mlod.aut.100.2 <- quantile(as.vector(pout100aut2$mlod[,3]), 0.95))
(ss.aut.100.2 <- quantile(as.vector(pout100aut2$ss[,3]), 0.95))
(qf.aut.100.2 <- quantile(as.vector(pout100aut2$qf[,3]), 0.95))
(mt.aut.100.2 <- quantile(as.vector(pout100aut2$mult[,3]), 0.95))

(slod.aut.100.3 <- quantile(as.vector(pout100aut3$slod[,3]), 0.95))
(mlod.aut.100.3 <- quantile(as.vector(pout100aut3$mlod[,3]), 0.95))
(ss.aut.100.3 <- quantile(as.vector(pout100aut3$ss[,3]), 0.95))
(qf.aut.100.3 <- quantile(as.vector(pout100aut3$qf[,3]), 0.95))
(mt.aut.100.3 <- quantile(as.vector(pout100aut3$mult[,3]), 0.95))

(slod.aut.100.4 <- quantile(as.vector(pout100aut4$slod[,3]), 0.95))
(mlod.aut.100.4 <- quantile(as.vector(pout100aut4$mlod[,3]), 0.95))
(ss.aut.100.4 <- quantile(as.vector(pout100aut4$ss[,3]), 0.95))
(qf.aut.100.4 <- quantile(as.vector(pout100aut4$qf[,3]), 0.95))
(mt.aut.100.4 <- quantile(as.vector(pout100aut4$mult[,3]), 0.95))

(slod.aut.400.1 <- quantile(as.vector(pout400aut1$slod[,3]), 0.95))
(mlod.aut.400.1 <- quantile(as.vector(pout400aut1$mlod[,3]), 0.95))
(ss.aut.400.1 <- quantile(as.vector(pout400aut1$ss[,3]), 0.95))
(qf.aut.400.1 <- quantile(as.vector(pout400aut1$qf[,3]), 0.95))
(mt.aut.400.1 <- quantile(as.vector(pout400aut1$mult[,3]), 0.95))

(slod.aut.400.2 <- quantile(as.vector(pout400aut2$slod[,3]), 0.95))
(mlod.aut.400.2 <- quantile(as.vector(pout400aut2$mlod[,3]), 0.95))
(ss.aut.400.2 <- quantile(as.vector(pout400aut2$ss[,3]), 0.95))
(qf.aut.400.2 <- quantile(as.vector(pout400aut2$qf[,3]), 0.95))
(mt.aut.400.2 <- quantile(as.vector(pout400aut2$mult[,3]), 0.95))

(slod.aut.400.3 <- quantile(as.vector(pout400aut3$slod[,3]), 0.95))
(mlod.aut.400.3 <- quantile(as.vector(pout400aut3$mlod[,3]), 0.95))
(ss.aut.400.3 <- quantile(as.vector(pout400aut3$ss[,3]), 0.95))
(qf.aut.400.3 <- quantile(as.vector(pout400aut3$qf[,3]), 0.95))
(mt.aut.400.3 <- quantile(as.vector(pout400aut3$mult[,3]), 0.95))

(slod.aut.400.4 <- quantile(as.vector(pout400aut4$slod[,3]), 0.95))
(mlod.aut.400.4 <- quantile(as.vector(pout400aut4$mlod[,3]), 0.95))
(ss.aut.400.4 <- quantile(as.vector(pout400aut4$ss[,3]), 0.95))
(qf.aut.400.4 <- quantile(as.vector(pout400aut4$qf[,3]), 0.95))
(mt.aut.400.4 <- quantile(as.vector(pout400aut4$mult[,3]), 0.95))



(slod.eq.100.1 <- quantile(as.vector(pout100eq1$slod[,3]), 0.95))
(mlod.eq.100.1 <- quantile(as.vector(pout100eq1$mlod[,3]), 0.95))
(ss.eq.100.1 <- quantile(as.vector(pout100eq1$ss[,3]), 0.95))
(qf.eq.100.1 <- quantile(as.vector(pout100eq1$qf[,3]), 0.95))
(mt.eq.100.1 <- quantile(as.vector(pout100eq1$mult[,3]), 0.95))

(slod.eq.100.2 <- quantile(as.vector(pout100eq2$slod[,3]), 0.95))
(mlod.eq.100.2 <- quantile(as.vector(pout100eq2$mlod[,3]), 0.95))
(ss.eq.100.2 <- quantile(as.vector(pout100eq2$ss[,3]), 0.95))
(qf.eq.100.2 <- quantile(as.vector(pout100eq2$qf[,3]), 0.95))
(mt.eq.100.2 <- quantile(as.vector(pout100eq2$mult[,3]), 0.95))

(slod.eq.100.3 <- quantile(as.vector(pout100eq3$slod[,3]), 0.95))
(mlod.eq.100.3 <- quantile(as.vector(pout100eq3$mlod[,3]), 0.95))
(ss.eq.100.3 <- quantile(as.vector(pout100eq3$ss[,3]), 0.95))
(qf.eq.100.3 <- quantile(as.vector(pout100eq3$qf[,3]), 0.95))
(mt.eq.100.3 <- quantile(as.vector(pout100eq3$mult[,3]), 0.95))

(slod.eq.100.4 <- quantile(as.vector(pout100eq4$slod[,3]), 0.95))
(mlod.eq.100.4 <- quantile(as.vector(pout100eq4$mlod[,3]), 0.95))
(ss.eq.100.4 <- quantile(as.vector(pout100eq4$ss[,3]), 0.95))
(qf.eq.100.4 <- quantile(as.vector(pout100eq4$qf[,3]), 0.95))
(mt.eq.100.4 <- quantile(as.vector(pout100eq4$mult[,3]), 0.95))


(slod.eq.400.1 <- quantile(as.vector(pout400eq1$slod[,3]), 0.95))
(mlod.eq.400.1 <- quantile(as.vector(pout400eq1$mlod[,3]), 0.95))
(ss.eq.400.1 <- quantile(as.vector(pout400eq1$ss[,3]), 0.95))
(qf.eq.400.1 <- quantile(as.vector(pout400eq1$qf[,3]), 0.95))
(mt.eq.400.1 <- quantile(as.vector(pout400eq1$mult[,3]), 0.95))

(slod.eq.400.2 <- quantile(as.vector(pout400eq2$slod[,3]), 0.95))
(mlod.eq.400.2 <- quantile(as.vector(pout400eq2$mlod[,3]), 0.95))
(ss.eq.400.2 <- quantile(as.vector(pout400eq2$ss[,3]), 0.95))
(qf.eq.400.2 <- quantile(as.vector(pout400eq2$qf[,3]), 0.95))
(mt.eq.400.2 <- quantile(as.vector(pout400eq2$mult[,3]), 0.95))

(slod.eq.400.3 <- quantile(as.vector(pout400eq3$slod[,3]), 0.95))
(mlod.eq.400.3 <- quantile(as.vector(pout400eq3$mlod[,3]), 0.95))
(ss.eq.400.3 <- quantile(as.vector(pout400eq3$ss[,3]), 0.95))
(qf.eq.400.3 <- quantile(as.vector(pout400eq3$qf[,3]), 0.95))
(mt.eq.400.3 <- quantile(as.vector(pout400eq3$mult[,3]), 0.95))

(slod.eq.400.4 <- quantile(as.vector(pout400eq4$slod[,3]), 0.95))
(mlod.eq.400.4 <- quantile(as.vector(pout400eq4$mlod[,3]), 0.95))
(ss.eq.400.4 <- quantile(as.vector(pout400eq4$ss[,3]), 0.95))
(qf.eq.400.4 <- quantile(as.vector(pout400eq4$qf[,3]), 0.95))
(mt.eq.400.4 <- quantile(as.vector(pout400eq4$mult[,3]), 0.95))



(slod.st.100.1 <- quantile(as.vector(pout100st1$slod[,3]), 0.95))
(mlod.st.100.1 <- quantile(as.vector(pout100st1$mlod[,3]), 0.95))
(ss.st.100.1 <- quantile(as.vector(pout100st1$ss[,3]), 0.95))
(qf.st.100.1 <- quantile(as.vector(pout100st1$qf[,3]), 0.95))
(mt.st.100.1 <- quantile(as.vector(pout100st1$mult[,3]), 0.95))

(slod.st.100.2 <- quantile(as.vector(pout100st2$slod[,3]), 0.95))
(mlod.st.100.2 <- quantile(as.vector(pout100st2$mlod[,3]), 0.95))
(ss.st.100.2 <- quantile(as.vector(pout100st2$ss[,3]), 0.95))
(qf.st.100.2 <- quantile(as.vector(pout100st2$qf[,3]), 0.95))
(mt.st.100.2 <- quantile(as.vector(pout100st2$mult[,3]), 0.95))

(slod.st.100.3 <- quantile(as.vector(pout100st3$slod[,3]), 0.95))
(mlod.st.100.3 <- quantile(as.vector(pout100st3$mlod[,3]), 0.95))
(ss.st.100.3 <- quantile(as.vector(pout100st3$ss[,3]), 0.95))
(qf.st.100.3 <- quantile(as.vector(pout100st3$qf[,3]), 0.95))
(mt.st.100.3 <- quantile(as.vector(pout100st3$mult[,3]), 0.95))

(slod.st.100.4 <- quantile(as.vector(pout100st4$slod[,3]), 0.95))
(mlod.st.100.4 <- quantile(as.vector(pout100st4$mlod[,3]), 0.95))
(ss.st.100.4 <- quantile(as.vector(pout100st4$ss[,3]), 0.95))
(qf.st.100.4 <- quantile(as.vector(pout100st4$qf[,3]), 0.95))
(mt.st.100.4 <- quantile(as.vector(pout100st4$mult[,3]), 0.95))


(slod.st.400.1 <- quantile(as.vector(pout400st1$slod[,3]), 0.95))
(mlod.st.400.1 <- quantile(as.vector(pout400st1$mlod[,3]), 0.95))
(ss.st.400.1 <- quantile(as.vector(pout400st1$ss[,3]), 0.95))
(qf.st.400.1 <- quantile(as.vector(pout400st1$qf[,3]), 0.95))
(mt.st.400.1 <- quantile(as.vector(pout400st1$mult[,3]), 0.95))

(slod.st.400.2 <- quantile(as.vector(pout400st2$slod[,3]), 0.95))
(mlod.st.400.2 <- quantile(as.vector(pout400st2$mlod[,3]), 0.95))
(ss.st.400.2 <- quantile(as.vector(pout400st2$ss[,3]), 0.95))
(qf.st.400.2 <- quantile(as.vector(pout400st2$qf[,3]), 0.95))
(mt.st.400.2 <- quantile(as.vector(pout400st2$mult[,3]), 0.95))

(slod.st.400.3 <- quantile(as.vector(pout400st3$slod[,3]), 0.95))
(mlod.st.400.3 <- quantile(as.vector(pout400st3$mlod[,3]), 0.95))
(ss.st.400.3 <- quantile(as.vector(pout400st3$ss[,3]), 0.95))
(qf.st.400.3 <- quantile(as.vector(pout400st3$qf[,3]), 0.95))
(mt.st.400.3 <- quantile(as.vector(pout400st3$mult[,3]), 0.95))

(slod.st.400.4 <- quantile(as.vector(pout400st4$slod[,3]), 0.95))
(mlod.st.400.4 <- quantile(as.vector(pout400st4$mlod[,3]), 0.95))
(ss.st.400.4 <- quantile(as.vector(pout400st4$ss[,3]), 0.95))
(qf.st.400.4 <- quantile(as.vector(pout400st4$qf[,3]), 0.95))
(mt.st.400.4 <- quantile(as.vector(pout400st4$mult[,3]), 0.95))








(slod.aut.200.1 <- quantile(as.vector(pout200aut1$slod[,3]), 0.95))
(mlod.aut.200.1 <- quantile(as.vector(pout200aut1$mlod[,3]), 0.95))
(ss.aut.200.1 <- quantile(as.vector(pout200aut1$ss[,3]), 0.95))
(qf.aut.200.1 <- quantile(as.vector(pout200aut1$qf[,3]), 0.95))
(mt.aut.200.1 <- quantile(as.vector(pout200aut1$mult[,3]), 0.95))

(slod.aut.200.2 <- quantile(as.vector(pout200aut2$slod[,3]), 0.95))
(mlod.aut.200.2 <- quantile(as.vector(pout200aut2$mlod[,3]), 0.95))
(ss.aut.200.2 <- quantile(as.vector(pout200aut2$ss[,3]), 0.95))
(qf.aut.200.2 <- quantile(as.vector(pout200aut2$qf[,3]), 0.95))
(mt.aut.200.2 <- quantile(as.vector(pout200aut2$mult[,3]), 0.95))

(slod.aut.200.3 <- quantile(as.vector(pout200aut3$slod[,3]), 0.95))
(mlod.aut.200.3 <- quantile(as.vector(pout200aut3$mlod[,3]), 0.95))
(ss.aut.200.3 <- quantile(as.vector(pout200aut3$ss[,3]), 0.95))
(qf.aut.200.3 <- quantile(as.vector(pout200aut3$qf[,3]), 0.95))
(mt.aut.200.3 <- quantile(as.vector(pout200aut3$mult[,3]), 0.95))

(slod.aut.200.4 <- quantile(as.vector(pout200aut4$slod[,3]), 0.95))
(mlod.aut.200.4 <- quantile(as.vector(pout200aut4$mlod[,3]), 0.95))
(ss.aut.200.4 <- quantile(as.vector(pout200aut4$ss[,3]), 0.95))
(qf.aut.200.4 <- quantile(as.vector(pout200aut4$qf[,3]), 0.95))
(mt.aut.200.4 <- quantile(as.vector(pout200aut4$mult[,3]), 0.95))


(slod.eq.200.1 <- quantile(as.vector(pout200eq1$slod[,3]), 0.95))
(mlod.eq.200.1 <- quantile(as.vector(pout200eq1$mlod[,3]), 0.95))
(ss.eq.200.1 <- quantile(as.vector(pout200eq1$ss[,3]), 0.95))
(qf.eq.200.1 <- quantile(as.vector(pout200eq1$qf[,3]), 0.95))
(mt.eq.200.1 <- quantile(as.vector(pout200eq1$mult[,3]), 0.95))

(slod.eq.200.2 <- quantile(as.vector(pout200eq2$slod[,3]), 0.95))
(mlod.eq.200.2 <- quantile(as.vector(pout200eq2$mlod[,3]), 0.95))
(ss.eq.200.2 <- quantile(as.vector(pout200eq2$ss[,3]), 0.95))
(qf.eq.200.2 <- quantile(as.vector(pout200eq2$qf[,3]), 0.95))
(mt.eq.200.2 <- quantile(as.vector(pout200eq2$mult[,3]), 0.95))

(slod.eq.200.3 <- quantile(as.vector(pout200eq3$slod[,3]), 0.95))
(mlod.eq.200.3 <- quantile(as.vector(pout200eq3$mlod[,3]), 0.95))
(ss.eq.200.3 <- quantile(as.vector(pout200eq3$ss[,3]), 0.95))
(qf.eq.200.3 <- quantile(as.vector(pout200eq3$qf[,3]), 0.95))
(mt.eq.200.3 <- quantile(as.vector(pout200eq3$mult[,3]), 0.95))

(slod.eq.200.4 <- quantile(as.vector(pout200eq4$slod[,3]), 0.95))
(mlod.eq.200.4 <- quantile(as.vector(pout200eq4$mlod[,3]), 0.95))
(ss.eq.200.4 <- quantile(as.vector(pout200eq4$ss[,3]), 0.95))
(qf.eq.200.4 <- quantile(as.vector(pout200eq4$qf[,3]), 0.95))
(mt.eq.200.4 <- quantile(as.vector(pout200eq4$mult[,3]), 0.95))

(slod.st.200.1 <- quantile(as.vector(pout200st1$slod[,3]), 0.95))
(mlod.st.200.1 <- quantile(as.vector(pout200st1$mlod[,3]), 0.95))
(ss.st.200.1 <- quantile(as.vector(pout200st1$ss[,3]), 0.95))
(qf.st.200.1 <- quantile(as.vector(pout200st1$qf[,3]), 0.95))
(mt.st.200.1 <- quantile(as.vector(pout200st1$mult[,3]), 0.95))

(slod.st.200.2 <- quantile(as.vector(pout200st2$slod[,3]), 0.95))
(mlod.st.200.2 <- quantile(as.vector(pout200st2$mlod[,3]), 0.95))
(ss.st.200.2 <- quantile(as.vector(pout200st2$ss[,3]), 0.95))
(qf.st.200.2 <- quantile(as.vector(pout200st2$qf[,3]), 0.95))
(mt.st.200.2 <- quantile(as.vector(pout200st2$mult[,3]), 0.95))

(slod.st.200.3 <- quantile(as.vector(pout200st3$slod[,3]), 0.95))
(mlod.st.200.3 <- quantile(as.vector(pout200st3$mlod[,3]), 0.95))
(ss.st.200.3 <- quantile(as.vector(pout200st3$ss[,3]), 0.95))
(qf.st.200.3 <- quantile(as.vector(pout200st3$qf[,3]), 0.95))
(mt.st.200.3 <- quantile(as.vector(pout200st3$mult[,3]), 0.95))

(slod.st.200.4 <- quantile(as.vector(pout200st4$slod[,3]), 0.95))
(mlod.st.200.4 <- quantile(as.vector(pout200st4$mlod[,3]), 0.95))
(ss.st.200.4 <- quantile(as.vector(pout200st4$ss[,3]), 0.95))
(qf.st.200.4 <- quantile(as.vector(pout200st4$qf[,3]), 0.95))
(mt.st.200.4 <- quantile(as.vector(pout200st4$mult[,3]), 0.95))





(slod.aut.200.1.gi1 <- quantile(as.vector(pout200aut1gi1$slod[,3]), 0.95))
(mlod.aut.200.1.gi1 <- quantile(as.vector(pout200aut1gi1$mlod[,3]), 0.95))
(ss.aut.200.1.gi1 <- quantile(as.vector(pout200aut1gi1$ss[,3]), 0.95))
(qf.aut.200.1.gi1 <- quantile(as.vector(pout200aut1gi1$qf[,3]), 0.95))
(mt.aut.200.1.gi1 <- quantile(as.vector(pout200aut1gi1$mult[,3]), 0.95))

(slod.aut.200.1.gi2 <- quantile(as.vector(pout200aut1gi2$slod[,3]), 0.95))
(mlod.aut.200.1.gi2 <- quantile(as.vector(pout200aut1gi2$mlod[,3]), 0.95))
(ss.aut.200.1.gi2 <- quantile(as.vector(pout200aut1gi2$ss[,3]), 0.95))
(qf.aut.200.1.gi2 <- quantile(as.vector(pout200aut1gi2$qf[,3]), 0.95))
(mt.aut.200.1.gi2 <- quantile(as.vector(pout200aut1gi2$mult[,3]), 0.95))


(slod.eq.200.1.gi1 <- quantile(as.vector(pout200eq1gi1$slod[,3]), 0.95))
(mlod.eq.200.1.gi1 <- quantile(as.vector(pout200eq1gi1$mlod[,3]), 0.95))
(ss.eq.200.1.gi1 <- quantile(as.vector(pout200eq1gi1$ss[,3]), 0.95))
(qf.eq.200.1.gi1 <- quantile(as.vector(pout200eq1gi1$qf[,3]), 0.95))
(mt.eq.200.1.gi1 <- quantile(as.vector(pout200eq1gi1$mult[,3]), 0.95))

(slod.eq.200.1.gi2 <- quantile(as.vector(pout200eq1gi2$slod[,3]), 0.95))
(mlod.eq.200.1.gi2 <- quantile(as.vector(pout200eq1gi2$mlod[,3]), 0.95))
(ss.eq.200.1.gi2 <- quantile(as.vector(pout200eq1gi2$ss[,3]), 0.95))
(qf.eq.200.1.gi2 <- quantile(as.vector(pout200eq1gi2$qf[,3]), 0.95))
(mt.eq.200.1.gi2 <- quantile(as.vector(pout200eq1gi2$mult[,3]), 0.95))


(slod.st.200.1.gi1 <- quantile(as.vector(pout200st1gi1$slod[,3]), 0.95))
(mlod.st.200.1.gi1 <- quantile(as.vector(pout200st1gi1$mlod[,3]), 0.95))
(ss.st.200.1.gi1 <- quantile(as.vector(pout200st1gi1$ss[,3]), 0.95))
(qf.st.200.1.gi1 <- quantile(as.vector(pout200st1gi1$qf[,3]), 0.95))
(mt.st.200.1.gi1 <- quantile(as.vector(pout200st1gi1$mult[,3]), 0.95))

(slod.st.200.1.gi2 <- quantile(as.vector(pout200st1gi2$slod[,3]), 0.95))
(mlod.st.200.1.gi2 <- quantile(as.vector(pout200st1gi2$mlod[,3]), 0.95))
(ss.st.200.1.gi2 <- quantile(as.vector(pout200st1gi2$ss[,3]), 0.95))
(qf.st.200.1.gi2 <- quantile(as.vector(pout200st1gi2$qf[,3]), 0.95))
(mt.st.200.1.gi2 <- quantile(as.vector(pout200st1gi2$mult[,3]), 0.95))



(slod.aut.200.2.gi1 <- quantile(as.vector(pout200aut2gi1$slod[,3]), 0.95))
(mlod.aut.200.2.gi1 <- quantile(as.vector(pout200aut2gi1$mlod[,3]), 0.95))
(ss.aut.200.2.gi1 <- quantile(as.vector(pout200aut2gi1$ss[,3]), 0.95))
(qf.aut.200.2.gi1 <- quantile(as.vector(pout200aut2gi1$qf[,3]), 0.95))
(mt.aut.200.2.gi1 <- quantile(as.vector(pout200aut2gi1$mult[,3]), 0.95))

(slod.aut.200.2.gi2 <- quantile(as.vector(pout200aut2gi2$slod[,3]), 0.95))
(mlod.aut.200.2.gi2 <- quantile(as.vector(pout200aut2gi2$mlod[,3]), 0.95))
(ss.aut.200.2.gi2 <- quantile(as.vector(pout200aut2gi2$ss[,3]), 0.95))
(qf.aut.200.2.gi2 <- quantile(as.vector(pout200aut2gi2$qf[,3]), 0.95))
(mt.aut.200.2.gi2 <- quantile(as.vector(pout200aut2gi2$mult[,3]), 0.95))

(slod.eq.200.2.gi1 <- quantile(as.vector(pout200eq2gi1$slod[,3]), 0.95))
(mlod.eq.200.2.gi1 <- quantile(as.vector(pout200eq2gi1$mlod[,3]), 0.95))
(ss.eq.200.2.gi1 <- quantile(as.vector(pout200eq2gi1$ss[,3]), 0.95))
(qf.eq.200.2.gi1 <- quantile(as.vector(pout200eq2gi1$qf[,3]), 0.95))
(mt.eq.200.2.gi1 <- quantile(as.vector(pout200eq2gi1$mult[,3]), 0.95))

(slod.eq.200.2.gi2 <- quantile(as.vector(pout200eq2gi2$slod[,3]), 0.95))
(mlod.eq.200.2.gi2 <- quantile(as.vector(pout200eq2gi2$mlod[,3]), 0.95))
(ss.eq.200.2.gi2 <- quantile(as.vector(pout200eq2gi2$ss[,3]), 0.95))
(qf.eq.200.2.gi2 <- quantile(as.vector(pout200eq2gi2$qf[,3]), 0.95))
(mt.eq.200.2.gi2 <- quantile(as.vector(pout200eq2gi2$mult[,3]), 0.95))

(slod.st.200.2.gi1 <- quantile(as.vector(pout200st2gi1$slod[,3]), 0.95))
(mlod.st.200.2.gi1 <- quantile(as.vector(pout200st2gi1$mlod[,3]), 0.95))
(ss.st.200.2.gi1 <- quantile(as.vector(pout200st2gi1$ss[,3]), 0.95))
(qf.st.200.2.gi1 <- quantile(as.vector(pout200st2gi1$qf[,3]), 0.95))
(mt.st.200.2.gi1 <- quantile(as.vector(pout200st2gi1$mult[,3]), 0.95))

(slod.st.200.2.gi2 <- quantile(as.vector(pout200st2gi2$slod[,3]), 0.95))
(mlod.st.200.2.gi2 <- quantile(as.vector(pout200st2gi2$mlod[,3]), 0.95))
(ss.st.200.2.gi2 <- quantile(as.vector(pout200st2gi2$ss[,3]), 0.95))
(qf.st.200.2.gi2 <- quantile(as.vector(pout200st2gi2$qf[,3]), 0.95))
(mt.st.200.2.gi2 <- quantile(as.vector(pout200st2gi2$mult[,3]), 0.95))



(slod.aut.200.3.gi1 <- quantile(as.vector(pout200aut3gi1$slod[,3]), 0.95))
(mlod.aut.200.3.gi1 <- quantile(as.vector(pout200aut3gi1$mlod[,3]), 0.95))
(ss.aut.200.3.gi1 <- quantile(as.vector(pout200aut3gi1$ss[,3]), 0.95))
(qf.aut.200.3.gi1 <- quantile(as.vector(pout200aut3gi1$qf[,3]), 0.95))
(mt.aut.200.3.gi1 <- quantile(as.vector(pout200aut3gi1$mult[,3]), 0.95))

(slod.aut.200.3.gi2 <- quantile(as.vector(pout200aut3gi2$slod[,3]), 0.95))
(mlod.aut.200.3.gi2 <- quantile(as.vector(pout200aut3gi2$mlod[,3]), 0.95))
(ss.aut.200.3.gi2 <- quantile(as.vector(pout200aut3gi2$ss[,3]), 0.95))
(qf.aut.200.3.gi2 <- quantile(as.vector(pout200aut3gi2$qf[,3]), 0.95))
(mt.aut.200.3.gi2 <- quantile(as.vector(pout200aut3gi2$mult[,3]), 0.95))

(slod.eq.200.3.gi1 <- quantile(as.vector(pout200eq3gi1$slod[,3]), 0.95))
(mlod.eq.200.3.gi1 <- quantile(as.vector(pout200eq3gi1$mlod[,3]), 0.95))
(ss.eq.200.3.gi1 <- quantile(as.vector(pout200eq3gi1$ss[,3]), 0.95))
(qf.eq.200.3.gi1 <- quantile(as.vector(pout200eq3gi1$qf[,3]), 0.95))
(mt.eq.200.3.gi1 <- quantile(as.vector(pout200eq3gi1$mult[,3]), 0.95))

(slod.eq.200.3.gi2 <- quantile(as.vector(pout200eq3gi2$slod[,3]), 0.95))
(mlod.eq.200.3.gi2 <- quantile(as.vector(pout200eq3gi2$mlod[,3]), 0.95))
(ss.eq.200.3.gi2 <- quantile(as.vector(pout200eq3gi2$ss[,3]), 0.95))
(qf.eq.200.3.gi2 <- quantile(as.vector(pout200eq3gi2$qf[,3]), 0.95))
(mt.eq.200.3.gi2 <- quantile(as.vector(pout200eq3gi2$mult[,3]), 0.95))

(slod.st.200.3.gi1 <- quantile(as.vector(pout200st3gi1$slod[,3]), 0.95))
(mlod.st.200.3.gi1 <- quantile(as.vector(pout200st3gi1$mlod[,3]), 0.95))
(ss.st.200.3.gi1 <- quantile(as.vector(pout200st3gi1$ss[,3]), 0.95))
(qf.st.200.3.gi1 <- quantile(as.vector(pout200st3gi1$qf[,3]), 0.95))
(mt.st.200.3.gi1 <- quantile(as.vector(pout200st3gi1$mult[,3]), 0.95))

(slod.st.200.3.gi2 <- quantile(as.vector(pout200st3gi2$slod[,3]), 0.95))
(mlod.st.200.3.gi2 <- quantile(as.vector(pout200st3gi2$mlod[,3]), 0.95))
(ss.st.200.3.gi2 <- quantile(as.vector(pout200st3gi2$ss[,3]), 0.95))
(qf.st.200.3.gi2 <- quantile(as.vector(pout200st3gi2$qf[,3]), 0.95))
(mt.st.200.3.gi2 <- quantile(as.vector(pout200st3gi2$mult[,3]), 0.95))



(slod.aut.200.4.gi1 <- quantile(as.vector(pout200aut4gi1$slod[,3]), 0.95))
(mlod.aut.200.4.gi1 <- quantile(as.vector(pout200aut4gi1$mlod[,3]), 0.95))
(ss.aut.200.4.gi1 <- quantile(as.vector(pout200aut4gi1$ss[,3]), 0.95))
(qf.aut.200.4.gi1 <- quantile(as.vector(pout200aut4gi1$qf[,3]), 0.95))
(mt.aut.200.4.gi1 <- quantile(as.vector(pout200aut4gi1$mult[,3]), 0.95))

(slod.aut.200.4.gi2 <- quantile(as.vector(pout200aut4gi2$slod[,3]), 0.95))
(mlod.aut.200.4.gi2 <- quantile(as.vector(pout200aut4gi2$mlod[,3]), 0.95))
(ss.aut.200.4.gi2 <- quantile(as.vector(pout200aut4gi2$ss[,3]), 0.95))
(qf.aut.200.4.gi2 <- quantile(as.vector(pout200aut4gi2$qf[,3]), 0.95))
(mt.aut.200.4.gi2 <- quantile(as.vector(pout200aut4gi2$mult[,3]), 0.95))

(slod.eq.200.4.gi1 <- quantile(as.vector(pout200eq4gi1$slod[,3]), 0.95))
(mlod.eq.200.4.gi1 <- quantile(as.vector(pout200eq4gi1$mlod[,3]), 0.95))
(ss.eq.200.4.gi1 <- quantile(as.vector(pout200eq4gi1$ss[,3]), 0.95))
(qf.eq.200.4.gi1 <- quantile(as.vector(pout200eq4gi1$qf[,3]), 0.95))
(mt.eq.200.4.gi1 <- quantile(as.vector(pout200eq4gi1$mult[,3]), 0.95))

(slod.eq.200.4.gi2 <- quantile(as.vector(pout200eq4gi2$slod[,3]), 0.95))
(mlod.eq.200.4.gi2 <- quantile(as.vector(pout200eq4gi2$mlod[,3]), 0.95))
(ss.eq.200.4.gi2 <- quantile(as.vector(pout200eq4gi2$ss[,3]), 0.95))
(qf.eq.200.4.gi2 <- quantile(as.vector(pout200eq4gi2$qf[,3]), 0.95))
(mt.eq.200.4.gi2 <- quantile(as.vector(pout200eq4gi2$mult[,3]), 0.95))

(slod.st.200.4.gi1 <- quantile(as.vector(pout200st4gi1$slod[,3]), 0.95))
(mlod.st.200.4.gi1 <- quantile(as.vector(pout200st4gi1$mlod[,3]), 0.95))
(ss.st.200.4.gi1 <- quantile(as.vector(pout200st4gi1$ss[,3]), 0.95))
(qf.st.200.4.gi1 <- quantile(as.vector(pout200st4gi1$qf[,3]), 0.95))
(mt.st.200.4.gi1 <- quantile(as.vector(pout200st4gi1$mult[,3]), 0.95))

(slod.st.200.4.gi2 <- quantile(as.vector(pout200st4gi2$slod[,3]), 0.95))
(mlod.st.200.4.gi2 <- quantile(as.vector(pout200st4gi2$mlod[,3]), 0.95))
(ss.st.200.4.gi2 <- quantile(as.vector(pout200st4gi2$ss[,3]), 0.95))
(qf.st.200.4.gi2 <- quantile(as.vector(pout200st4gi2$qf[,3]), 0.95))
(mt.st.200.4.gi2 <- quantile(as.vector(pout200st4gi2$mult[,3]), 0.95))




# get power rmse for each case
(pout.100.au.1.s <- prtout(out.100.au.1$slod, slod.aut.100.1, 7))
(pout.100.au.1.m <- prtout(out.100.au.1$mlod, mlod.aut.100.1, 7))
(pout.100.au.1.ss <- prtout(out.100.au.1$ss, ss.aut.100.1, 7))
(pout.100.au.1.qf <- prtout(out.100.au.1$qf, qf.aut.100.1, 7))
(pout.100.au.1.mt <- prtout(out.100.au.1$mult, mt.aut.100.1, 7))

(pout.100.au.2.s <- prtout(out.100.au.2$slod, slod.aut.100.2, 7))
(pout.100.au.2.m <- prtout(out.100.au.2$mlod, mlod.aut.100.2, 7))
(pout.100.au.2.ss <- prtout(out.100.au.2$ss, ss.aut.100.2, 7))
(pout.100.au.2.qf <- prtout(out.100.au.2$qf, qf.aut.100.2, 7))
(pout.100.au.2.mt <- prtout(out.100.au.2$mult, mt.aut.100.2, 7))

(pout.100.au.3.s <- prtout(out.100.au.3$slod, slod.aut.100.3, 7))
(pout.100.au.3.m <- prtout(out.100.au.3$mlod, mlod.aut.100.3, 7))
(pout.100.au.3.ss <- prtout(out.100.au.3$ss, ss.aut.100.3, 7))
(pout.100.au.3.qf <- prtout(out.100.au.3$qf, qf.aut.100.3, 7))
(pout.100.au.3.mt <- prtout(out.100.au.3$mult, mt.aut.100.3, 7))

(pout.100.au.4.s <- prtout(out.100.au.4$slod, slod.aut.100.4, 7))
(pout.100.au.4.m <- prtout(out.100.au.4$mlod, mlod.aut.100.4, 7))
(pout.100.au.4.ss <- prtout(out.100.au.4$ss, ss.aut.100.4, 7))
(pout.100.au.4.qf <- prtout(out.100.au.4$qf, qf.aut.100.4, 7))
(pout.100.au.4.mt <- prtout(out.100.au.4$mult, mt.aut.100.4, 7))




(pout.100.eq.1.s <- prtout(out.100.eq.1$slod, slod.eq.100.1, 7))
(pout.100.eq.1.m <- prtout(out.100.eq.1$mlod, mlod.eq.100.1, 7))
(pout.100.eq.1.ss <- prtout(out.100.eq.1$ss, ss.eq.100.1, 7))
(pout.100.eq.1.qf <- prtout(out.100.eq.1$qf, qf.eq.100.1, 7))
(pout.100.eq.1.mt <- prtout(out.100.eq.1$mult, mt.eq.100.1, 7))

(pout.100.eq.2.s <- prtout(out.100.eq.2$slod, slod.eq.100.2, 7))
(pout.100.eq.2.m <- prtout(out.100.eq.2$mlod, mlod.eq.100.2, 7))
(pout.100.eq.2.ss <- prtout(out.100.eq.2$ss, ss.eq.100.2, 7))
(pout.100.eq.2.qf <- prtout(out.100.eq.2$qf, qf.eq.100.2, 7))
(pout.100.eq.2.mt <- prtout(out.100.eq.2$mult, mt.eq.100.2, 7))

(pout.100.eq.3.s <- prtout(out.100.eq.3$slod, slod.eq.100.3, 7))
(pout.100.eq.3.m <- prtout(out.100.eq.3$mlod, mlod.eq.100.3, 7))
(pout.100.eq.3.ss <- prtout(out.100.eq.3$ss, ss.eq.100.3, 7))
(pout.100.eq.3.qf <- prtout(out.100.eq.3$qf, qf.eq.100.3, 7))
(pout.100.eq.3.mt <- prtout(out.100.eq.3$mult, mt.eq.100.3, 7))

(pout.100.eq.4.s <- prtout(out.100.eq.4$slod, slod.eq.100.4, 7))
(pout.100.eq.4.m <- prtout(out.100.eq.4$mlod, mlod.eq.100.4, 7))
(pout.100.eq.4.ss <- prtout(out.100.eq.4$ss, ss.eq.100.4, 7))
(pout.100.eq.4.qf <- prtout(out.100.eq.4$qf, qf.eq.100.4, 7))
(pout.100.eq.4.mt <- prtout(out.100.eq.4$mult, mt.eq.100.4, 7))








(pout.100.st.1.s <- prtout(out.100.st.1$slod, slod.st.100.1, 7))
(pout.100.st.1.m <- prtout(out.100.st.1$mlod, mlod.st.100.1, 7))
(pout.100.st.1.ss <- prtout(out.100.st.1$ss, ss.st.100.1, 7))
(pout.100.st.1.qf <- prtout(out.100.st.1$qf, qf.st.100.1, 7))
(pout.100.st.1.mt <- prtout(out.100.st.1$mult, mt.st.100.1, 7))

(pout.100.st.2.s <- prtout(out.100.st.2$slod, slod.st.100.2, 7))
(pout.100.st.2.m <- prtout(out.100.st.2$mlod, mlod.st.100.2, 7))
(pout.100.st.2.ss <- prtout(out.100.st.2$ss, ss.st.100.2, 7))
(pout.100.st.2.qf <- prtout(out.100.st.2$qf, qf.st.100.2, 7))
(pout.100.st.2.mt <- prtout(out.100.st.2$mult, mt.st.100.2, 7))

(pout.100.st.3.s <- prtout(out.100.st.3$slod, slod.st.100.3, 7))
(pout.100.st.3.m <- prtout(out.100.st.3$mlod, mlod.st.100.3, 7))
(pout.100.st.3.ss <- prtout(out.100.st.3$ss, ss.st.100.3, 7))
(pout.100.st.3.qf <- prtout(out.100.st.3$qf, qf.st.100.3, 7))
(pout.100.st.3.mt <- prtout(out.100.st.3$mult, mt.st.100.3, 7))

(pout.100.st.4.s <- prtout(out.100.st.4$slod, slod.st.100.4, 7))
(pout.100.st.4.m <- prtout(out.100.st.4$mlod, mlod.st.100.4, 7))
(pout.100.st.4.ss <- prtout(out.100.st.4$ss, ss.st.100.4, 7))
(pout.100.st.4.qf <- prtout(out.100.st.4$qf, qf.st.100.4, 7))
(pout.100.st.4.mt <- prtout(out.100.st.4$mult, mt.st.100.4, 7))




(pout.400.au.1.s <- prtout(out.400.au.1$slod, slod.aut.400.1, 7))
(pout.400.au.1.m <- prtout(out.400.au.1$mlod, mlod.aut.400.1, 7))
(pout.400.au.1.ss <- prtout(out.400.au.1$ss, ss.aut.400.1, 7))
(pout.400.au.1.qf <- prtout(out.400.au.1$qf, qf.aut.400.1, 7))
(pout.400.au.1.mt <- prtout(out.400.au.1$mult, mt.aut.400.1, 7))

(pout.400.au.2.s <- prtout(out.400.au.2$slod, slod.aut.400.2, 7))
(pout.400.au.2.m <- prtout(out.400.au.2$mlod, mlod.aut.400.2, 7))
(pout.400.au.2.ss <- prtout(out.400.au.2$ss, ss.aut.400.2, 7))
(pout.400.au.2.qf <- prtout(out.400.au.2$qf, qf.aut.400.2, 7))
(pout.400.au.2.mt <- prtout(out.400.au.2$mult, mt.aut.400.2, 7))

(pout.400.au.3.s <- prtout(out.400.au.3$slod, slod.aut.400.3, 7))
(pout.400.au.3.m <- prtout(out.400.au.3$mlod, mlod.aut.400.3, 7))
(pout.400.au.3.ss <- prtout(out.400.au.3$ss, ss.aut.400.3, 7))
(pout.400.au.3.qf <- prtout(out.400.au.3$qf, qf.aut.400.3, 7))
(pout.400.au.3.mt <- prtout(out.400.au.3$mult, mt.aut.400.3, 7))

(pout.400.au.4.s <- prtout(out.400.au.4$slod, slod.aut.400.4, 7))
(pout.400.au.4.m <- prtout(out.400.au.4$mlod, mlod.aut.400.4, 7))
(pout.400.au.4.ss <- prtout(out.400.au.4$ss, ss.aut.400.4, 7))
(pout.400.au.4.qf <- prtout(out.400.au.4$qf, qf.aut.400.4, 7))
(pout.400.au.4.mt <- prtout(out.400.au.4$mult, mt.aut.400.4, 7))





(pout.400.eq.1.s <- prtout(out.400.eq.1$slod, slod.eq.400.1, 7))
(pout.400.eq.1.m <- prtout(out.400.eq.1$mlod, mlod.eq.400.1, 7))
(pout.400.eq.1.ss <- prtout(out.400.eq.1$ss, ss.eq.400.1, 7))
(pout.400.eq.1.qf <- prtout(out.400.eq.1$qf, qf.eq.400.1, 7))
(pout.400.eq.1.mt <- prtout(out.400.eq.1$mult, mt.eq.400.1, 7))

(pout.400.eq.2.s <- prtout(out.400.eq.2$slod, slod.eq.400.2, 7))
(pout.400.eq.2.m <- prtout(out.400.eq.2$mlod, mlod.eq.400.2, 7))
(pout.400.eq.2.ss <- prtout(out.400.eq.2$ss, ss.eq.400.2, 7))
(pout.400.eq.2.qf <- prtout(out.400.eq.2$qf, qf.eq.400.2, 7))
(pout.400.eq.2.mt <- prtout(out.400.eq.2$mult, mt.eq.400.2, 7))

(pout.400.eq.3.s <- prtout(out.400.eq.3$slod, slod.eq.400.3, 7))
(pout.400.eq.3.m <- prtout(out.400.eq.3$mlod, mlod.eq.400.3, 7))
(pout.400.eq.3.ss <- prtout(out.400.eq.3$ss, ss.eq.400.3, 7))
(pout.400.eq.3.qf <- prtout(out.400.eq.3$qf, qf.eq.400.3, 7))
(pout.400.eq.3.mt <- prtout(out.400.eq.3$mult, mt.eq.400.3, 7))

(pout.400.eq.4.s <- prtout(out.400.eq.4$slod, slod.eq.400.4, 7))
(pout.400.eq.4.m <- prtout(out.400.eq.4$mlod, mlod.eq.400.4, 7))
(pout.400.eq.4.ss <- prtout(out.400.eq.4$ss, ss.eq.400.4, 7))
(pout.400.eq.4.qf <- prtout(out.400.eq.4$qf, qf.eq.400.4, 7))
(pout.400.eq.4.mt <- prtout(out.400.eq.4$mult, mt.eq.400.4, 7))



(pout.400.st.1.s <- prtout(out.400.st.1$slod, slod.st.400.1, 7))
(pout.400.st.1.m <- prtout(out.400.st.1$mlod, mlod.st.400.1, 7))
(pout.400.st.1.ss <- prtout(out.400.st.1$ss, ss.st.400.1, 7))
(pout.400.st.1.qf <- prtout(out.400.st.1$qf, qf.st.400.1, 7))
(pout.400.st.1.mt <- prtout(out.400.st.1$mult, mt.st.400.1, 7))

(pout.400.st.2.s <- prtout(out.400.st.2$slod, slod.st.400.2, 7))
(pout.400.st.2.m <- prtout(out.400.st.2$mlod, mlod.st.400.2, 7))
(pout.400.st.2.ss <- prtout(out.400.st.2$ss, ss.st.400.2, 7))
(pout.400.st.2.qf <- prtout(out.400.st.2$qf, qf.st.400.2, 7))
(pout.400.st.2.mt <- prtout(out.400.st.2$mult, mt.st.400.2, 7))

(pout.400.st.3.s <- prtout(out.400.st.3$slod, slod.st.400.3, 7))
(pout.400.st.3.m <- prtout(out.400.st.3$mlod, mlod.st.400.3, 7))
(pout.400.st.3.ss <- prtout(out.400.st.3$ss, ss.st.400.3, 7))
(pout.400.st.3.qf <- prtout(out.400.st.3$qf, qf.st.400.3, 7))
(pout.400.st.3.mt <- prtout(out.400.st.3$mult, mt.st.400.3, 7))

(pout.400.st.4.s <- prtout(out.400.st.4$slod, slod.st.400.4, 7))
(pout.400.st.4.m <- prtout(out.400.st.4$mlod, mlod.st.400.4, 7))
(pout.400.st.4.ss <- prtout(out.400.st.4$ss, ss.st.400.4, 7))
(pout.400.st.4.qf <- prtout(out.400.st.4$qf, qf.st.400.4, 7))
(pout.400.st.4.mt <- prtout(out.400.st.4$mult, mt.st.400.4, 7))





(pout.200.au.1.s <- prtout(out.200.au.1$slod, slod.aut.200.1, 7))
(pout.200.au.1.m <- prtout(out.200.au.1$mlod, mlod.aut.200.1, 7))
(pout.200.au.1.ss <- prtout(out.200.au.1$ss, ss.aut.200.1, 7))
(pout.200.au.1.qf <- prtout(out.200.au.1$qf, qf.aut.200.1, 7))
(pout.200.au.1.mt <- prtout(out.200.au.1$mult, mt.aut.200.1, 7))

(pout.200.au.2.s <- prtout(out.200.au.2$slod, slod.aut.200.2, 7))
(pout.200.au.2.m <- prtout(out.200.au.2$mlod, mlod.aut.200.2, 7))
(pout.200.au.2.ss <- prtout(out.200.au.2$ss, ss.aut.200.2, 7))
(pout.200.au.2.qf <- prtout(out.200.au.2$qf, qf.aut.200.2, 7))
(pout.200.au.2.mt <- prtout(out.200.au.2$mult, mt.aut.200.2, 7))

(pout.200.au.3.s <- prtout(out.200.au.3$slod, slod.aut.200.3, 7))
(pout.200.au.3.m <- prtout(out.200.au.3$mlod, mlod.aut.200.3, 7))
(pout.200.au.3.ss <- prtout(out.200.au.3$ss, ss.aut.200.3, 7))
(pout.200.au.3.qf <- prtout(out.200.au.3$qf, qf.aut.200.3, 7))
(pout.200.au.3.mt <- prtout(out.200.au.3$mult, mt.aut.200.3, 7))

(pout.200.au.4.s <- prtout(out.200.au.4$slod, slod.aut.200.4, 7))
(pout.200.au.4.m <- prtout(out.200.au.4$mlod, mlod.aut.200.4, 7))
(pout.200.au.4.ss <- prtout(out.200.au.4$ss, ss.aut.200.4, 7))
(pout.200.au.4.qf <- prtout(out.200.au.4$qf, qf.aut.200.4, 7))
(pout.200.au.4.mt <- prtout(out.200.au.4$mult, mt.aut.200.4, 7))




(pout.200.eq.1.s <- prtout(out.200.eq.1$slod, slod.eq.200.1, 7))
(pout.200.eq.1.m <- prtout(out.200.eq.1$mlod, mlod.eq.200.1, 7))
(pout.200.eq.1.ss <- prtout(out.200.eq.1$ss, ss.eq.200.1, 7))
(pout.200.eq.1.qf <- prtout(out.200.eq.1$qf, qf.eq.200.1, 7))
(pout.200.eq.1.mt <- prtout(out.200.eq.1$mult, mt.eq.200.1, 7))

(pout.200.eq.2.s <- prtout(out.200.eq.2$slod, slod.eq.200.2, 7))
(pout.200.eq.2.m <- prtout(out.200.eq.2$mlod, mlod.eq.200.2, 7))
(pout.200.eq.2.ss <- prtout(out.200.eq.2$ss, ss.eq.200.2, 7))
(pout.200.eq.2.qf <- prtout(out.200.eq.2$qf, qf.eq.200.2, 7))
(pout.200.eq.2.mt <- prtout(out.200.eq.2$mult, mt.eq.200.2, 7))

(pout.200.eq.3.s <- prtout(out.200.eq.3$slod, slod.eq.200.3, 7))
(pout.200.eq.3.m <- prtout(out.200.eq.3$mlod, mlod.eq.200.3, 7))
(pout.200.eq.3.ss <- prtout(out.200.eq.3$ss, ss.eq.200.3, 7))
(pout.200.eq.3.qf <- prtout(out.200.eq.3$qf, qf.eq.200.3, 7))
(pout.200.eq.3.mt <- prtout(out.200.eq.3$mult, mt.eq.200.3, 7))

(pout.200.eq.4.s <- prtout(out.200.eq.4$slod, slod.eq.200.4, 7))
(pout.200.eq.4.m <- prtout(out.200.eq.4$mlod, mlod.eq.200.4, 7))
(pout.200.eq.4.ss <- prtout(out.200.eq.4$ss, ss.eq.200.4, 7))
(pout.200.eq.4.qf <- prtout(out.200.eq.4$qf, qf.eq.200.4, 7))
(pout.200.eq.4.mt <- prtout(out.200.eq.4$mult, mt.eq.200.4, 7))








(pout.200.st.1.s <- prtout(out.200.st.1$slod, slod.st.200.1, 7))
(pout.200.st.1.m <- prtout(out.200.st.1$mlod, mlod.st.200.1, 7))
(pout.200.st.1.ss <- prtout(out.200.st.1$ss, ss.st.200.1, 7))
(pout.200.st.1.qf <- prtout(out.200.st.1$qf, qf.st.200.1, 7))
(pout.200.st.1.mt <- prtout(out.200.st.1$mult, mt.st.200.1, 7))

(pout.200.st.2.s <- prtout(out.200.st.2$slod, slod.st.200.2, 7))
(pout.200.st.2.m <- prtout(out.200.st.2$mlod, mlod.st.200.2, 7))
(pout.200.st.2.ss <- prtout(out.200.st.2$ss, ss.st.200.2, 7))
(pout.200.st.2.qf <- prtout(out.200.st.2$qf, qf.st.200.2, 7))
(pout.200.st.2.mt <- prtout(out.200.st.2$mult, mt.st.200.2, 7))

(pout.200.st.3.s <- prtout(out.200.st.3$slod, slod.st.200.3, 7))
(pout.200.st.3.m <- prtout(out.200.st.3$mlod, mlod.st.200.3, 7))
(pout.200.st.3.ss <- prtout(out.200.st.3$ss, ss.st.200.3, 7))
(pout.200.st.3.qf <- prtout(out.200.st.3$qf, qf.st.200.3, 7))
(pout.200.st.3.mt <- prtout(out.200.st.3$mult, mt.st.200.3, 7))

(pout.200.st.4.s <- prtout(out.200.st.4$slod, slod.st.200.4, 7))
(pout.200.st.4.m <- prtout(out.200.st.4$mlod, mlod.st.200.4, 7))
(pout.200.st.4.ss <- prtout(out.200.st.4$ss, ss.st.200.4, 7))
(pout.200.st.4.qf <- prtout(out.200.st.4$qf, qf.st.200.4, 7))
(pout.200.st.4.mt <- prtout(out.200.st.4$mult, mt.st.200.4, 7))





(pout.200.au.1.s.gi1 <- prtout(out.200.au.1.gi1$slod, slod.aut.200.1.gi1, 7))
(pout.200.au.1.m.gi1 <- prtout(out.200.au.1.gi1$mlod, mlod.aut.200.1.gi1, 7))
(pout.200.au.1.ss.gi1 <- prtout(out.200.au.1.gi1$ss, ss.aut.200.1.gi1, 7))
(pout.200.au.1.qf.gi1 <- prtout(out.200.au.1.gi1$qf, qf.aut.200.1.gi1, 7))
(pout.200.au.1.mt.gi1 <- prtout(out.200.au.1.gi1$mult, mt.aut.200.1.gi1, 7))

(pout.200.au.1.s.gi2 <- prtout(out.200.au.1.gi2$slod, slod.aut.200.1.gi2, 7))
(pout.200.au.1.m.gi2 <- prtout(out.200.au.1.gi2$mlod, mlod.aut.200.1.gi2, 7))
(pout.200.au.1.ss.gi2 <- prtout(out.200.au.1.gi2$ss, ss.aut.200.1.gi2, 7))
(pout.200.au.1.qf.gi2 <- prtout(out.200.au.1.gi2$qf, qf.aut.200.1.gi2, 7))
(pout.200.au.1.mt.gi2 <- prtout(out.200.au.1.gi2$mult, mt.aut.200.1.gi2, 7))

(pout.200.eq.1.s.gi1 <- prtout(out.200.eq.1.gi1$slod, slod.eq.200.1.gi1, 7))
(pout.200.eq.1.m.gi1 <- prtout(out.200.eq.1.gi1$mlod, mlod.eq.200.1.gi1, 7))
(pout.200.eq.1.ss.gi1 <- prtout(out.200.eq.1.gi1$ss, ss.eq.200.1.gi1, 7))
(pout.200.eq.1.qf.gi1 <- prtout(out.200.eq.1.gi1$qf, qf.eq.200.1.gi1, 7))
(pout.200.eq.1.mt.gi1 <- prtout(out.200.eq.1.gi1$mult, mt.eq.200.1.gi1, 7))

(pout.200.eq.1.s.gi2 <- prtout(out.200.eq.1.gi2$slod, slod.eq.200.1.gi2, 7))
(pout.200.eq.1.m.gi2 <- prtout(out.200.eq.1.gi2$mlod, mlod.eq.200.1.gi2, 7))
(pout.200.eq.1.ss.gi2 <- prtout(out.200.eq.1.gi2$ss, ss.eq.200.1.gi2, 7))
(pout.200.eq.1.qf.gi2 <- prtout(out.200.eq.1.gi2$qf, qf.eq.200.1.gi2, 7))
(pout.200.eq.1.mt.gi2 <- prtout(out.200.eq.1.gi2$mult, mt.eq.200.1.gi2, 7))

(pout.200.st.1.s.gi1 <- prtout(out.200.st.1.gi1$slod, slod.st.200.1.gi1, 7))
(pout.200.st.1.m.gi1 <- prtout(out.200.st.1.gi1$mlod, mlod.st.200.1.gi1, 7))
(pout.200.st.1.ss.gi1 <- prtout(out.200.st.1.gi1$ss, ss.st.200.1.gi1, 7))
(pout.200.st.1.qf.gi1 <- prtout(out.200.st.1.gi1$qf, qf.st.200.1.gi1, 7))
(pout.200.st.1.mt.gi1 <- prtout(out.200.st.1.gi1$mult, mt.st.200.1.gi1, 7))

(pout.200.st.1.s.gi2 <- prtout(out.200.st.1.gi2$slod, slod.st.200.1.gi2, 7))
(pout.200.st.1.m.gi2 <- prtout(out.200.st.1.gi2$mlod, mlod.st.200.1.gi2, 7))
(pout.200.st.1.ss.gi2 <- prtout(out.200.st.1.gi2$ss, ss.st.200.1.gi2, 7))
(pout.200.st.1.qf.gi2 <- prtout(out.200.st.1.gi2$qf, qf.st.200.1.gi2, 7))
(pout.200.st.1.mt.gi2 <- prtout(out.200.st.1.gi2$mult, mt.st.200.1.gi2, 7))




(pout.200.au.2.s.gi1 <- prtout(out.200.au.2.gi1$slod, slod.aut.200.2.gi1, 7))
(pout.200.au.2.m.gi1 <- prtout(out.200.au.2.gi1$mlod, mlod.aut.200.2.gi1, 7))
(pout.200.au.2.ss.gi1 <- prtout(out.200.au.2.gi1$ss, ss.aut.200.2.gi1, 7))
(pout.200.au.2.qf.gi1 <- prtout(out.200.au.2.gi1$qf, qf.aut.200.2.gi1, 7))
(pout.200.au.2.mt.gi1 <- prtout(out.200.au.2.gi1$mult, mt.aut.200.2.gi1, 7))

(pout.200.au.2.s.gi2 <- prtout(out.200.au.2.gi2$slod, slod.aut.200.2.gi2, 7))
(pout.200.au.2.m.gi2 <- prtout(out.200.au.2.gi2$mlod, mlod.aut.200.2.gi2, 7))
(pout.200.au.2.ss.gi2 <- prtout(out.200.au.2.gi2$ss, ss.aut.200.2.gi2, 7))
(pout.200.au.2.qf.gi2 <- prtout(out.200.au.2.gi2$qf, qf.aut.200.2.gi2, 7))
(pout.200.au.2.mt.gi2 <- prtout(out.200.au.2.gi2$mult, mt.aut.200.2.gi2, 7))

(pout.200.eq.2.s.gi1 <- prtout(out.200.eq.2.gi1$slod, slod.eq.200.2.gi1, 7))
(pout.200.eq.2.m.gi1 <- prtout(out.200.eq.2.gi1$mlod, mlod.eq.200.2.gi1, 7))
(pout.200.eq.2.ss.gi1 <- prtout(out.200.eq.2.gi1$ss, ss.eq.200.2.gi1, 7))
(pout.200.eq.2.qf.gi1 <- prtout(out.200.eq.2.gi1$qf, qf.eq.200.2.gi1, 7))
(pout.200.eq.2.mt.gi1 <- prtout(out.200.eq.2.gi1$mult, mt.eq.200.2.gi1, 7))

(pout.200.eq.2.s.gi2 <- prtout(out.200.eq.2.gi2$slod, slod.eq.200.2.gi2, 7))
(pout.200.eq.2.m.gi2 <- prtout(out.200.eq.2.gi2$mlod, mlod.eq.200.2.gi2, 7))
(pout.200.eq.2.ss.gi2 <- prtout(out.200.eq.2.gi2$ss, ss.eq.200.2.gi2, 7))
(pout.200.eq.2.qf.gi2 <- prtout(out.200.eq.2.gi2$qf, qf.eq.200.2.gi2, 7))
(pout.200.eq.2.mt.gi2 <- prtout(out.200.eq.2.gi2$mult, mt.eq.200.2.gi2, 7))

(pout.200.st.2.s.gi1 <- prtout(out.200.st.2.gi1$slod, slod.st.200.2.gi1, 7))
(pout.200.st.2.m.gi1 <- prtout(out.200.st.2.gi1$mlod, mlod.st.200.2.gi1, 7))
(pout.200.st.2.ss.gi1 <- prtout(out.200.st.2.gi1$ss, ss.st.200.2.gi1, 7))
(pout.200.st.2.qf.gi1 <- prtout(out.200.st.2.gi1$qf, qf.st.200.2.gi1, 7))
(pout.200.st.2.mt.gi1 <- prtout(out.200.st.2.gi1$mult, mt.st.200.2.gi1, 7))

(pout.200.st.2.s.gi2 <- prtout(out.200.st.2.gi2$slod, slod.st.200.2.gi2, 7))
(pout.200.st.2.m.gi2 <- prtout(out.200.st.2.gi2$mlod, mlod.st.200.2.gi2, 7))
(pout.200.st.2.ss.gi2 <- prtout(out.200.st.2.gi2$ss, ss.st.200.2.gi2, 7))
(pout.200.st.2.qf.gi2 <- prtout(out.200.st.2.gi2$qf, qf.st.200.2.gi2, 7))
(pout.200.st.2.mt.gi2 <- prtout(out.200.st.2.gi2$mult, mt.st.200.2.gi2, 7))




(pout.200.au.3.s.gi1 <- prtout(out.200.au.3.gi1$slod, slod.aut.200.3.gi1, 7))
(pout.200.au.3.m.gi1 <- prtout(out.200.au.3.gi1$mlod, mlod.aut.200.3.gi1, 7))
(pout.200.au.3.ss.gi1 <- prtout(out.200.au.3.gi1$ss, ss.aut.200.3.gi1, 7))
(pout.200.au.3.qf.gi1 <- prtout(out.200.au.3.gi1$qf, qf.aut.200.3.gi1, 7))
(pout.200.au.3.mt.gi1 <- prtout(out.200.au.3.gi1$mult, mt.aut.200.3.gi1, 7))

(pout.200.au.3.s.gi2 <- prtout(out.200.au.3.gi2$slod, slod.aut.200.3.gi2, 7))
(pout.200.au.3.m.gi2 <- prtout(out.200.au.3.gi2$mlod, mlod.aut.200.3.gi2, 7))
(pout.200.au.3.ss.gi2 <- prtout(out.200.au.3.gi2$ss, ss.aut.200.3.gi2, 7))
(pout.200.au.3.qf.gi2 <- prtout(out.200.au.3.gi2$qf, qf.aut.200.3.gi2, 7))
(pout.200.au.3.mt.gi2 <- prtout(out.200.au.3.gi2$mult, mt.aut.200.3.gi2, 7))

(pout.200.eq.3.s.gi1 <- prtout(out.200.eq.3.gi1$slod, slod.eq.200.3.gi1, 7))
(pout.200.eq.3.m.gi1 <- prtout(out.200.eq.3.gi1$mlod, mlod.eq.200.3.gi1, 7))
(pout.200.eq.3.ss.gi1 <- prtout(out.200.eq.3.gi1$ss, ss.eq.200.3.gi1, 7))
(pout.200.eq.3.qf.gi1 <- prtout(out.200.eq.3.gi1$qf, qf.eq.200.3.gi1, 7))
(pout.200.eq.3.mt.gi1 <- prtout(out.200.eq.3.gi1$mult, mt.eq.200.3.gi1, 7))

(pout.200.eq.3.s.gi2 <- prtout(out.200.eq.3.gi2$slod, slod.eq.200.3.gi2, 7))
(pout.200.eq.3.m.gi2 <- prtout(out.200.eq.3.gi2$mlod, mlod.eq.200.3.gi2, 7))
(pout.200.eq.3.ss.gi2 <- prtout(out.200.eq.3.gi2$ss, ss.eq.200.3.gi2, 7))
(pout.200.eq.3.qf.gi2 <- prtout(out.200.eq.3.gi2$qf, qf.eq.200.3.gi2, 7))
(pout.200.eq.3.mt.gi2 <- prtout(out.200.eq.3.gi2$mult, mt.eq.200.3.gi2, 7))

(pout.200.st.3.s.gi1 <- prtout(out.200.st.3.gi1$slod, slod.st.200.3.gi1, 7))
(pout.200.st.3.m.gi1 <- prtout(out.200.st.3.gi1$mlod, mlod.st.200.3.gi1, 7))
(pout.200.st.3.ss.gi1 <- prtout(out.200.st.3.gi1$ss, ss.st.200.3.gi1, 7))
(pout.200.st.3.qf.gi1 <- prtout(out.200.st.3.gi1$qf, qf.st.200.3.gi1, 7))
(pout.200.st.3.mt.gi1 <- prtout(out.200.st.3.gi1$mult, mt.st.200.3.gi1, 7))

(pout.200.st.3.s.gi2 <- prtout(out.200.st.3.gi2$slod, slod.st.200.3.gi2, 7))
(pout.200.st.3.m.gi2 <- prtout(out.200.st.3.gi2$mlod, mlod.st.200.3.gi2, 7))
(pout.200.st.3.ss.gi2 <- prtout(out.200.st.3.gi2$ss, ss.st.200.3.gi2, 7))
(pout.200.st.3.qf.gi2 <- prtout(out.200.st.3.gi2$qf, qf.st.200.3.gi2, 7))
(pout.200.st.3.mt.gi2 <- prtout(out.200.st.3.gi2$mult, mt.st.200.3.gi2, 7))




(pout.200.au.4.s.gi1 <- prtout(out.200.au.4.gi1$slod, slod.aut.200.4.gi1, 7))
(pout.200.au.4.m.gi1 <- prtout(out.200.au.4.gi1$mlod, mlod.aut.200.4.gi1, 7))
(pout.200.au.4.ss.gi1 <- prtout(out.200.au.4.gi1$ss, ss.aut.200.4.gi1, 7))
(pout.200.au.4.qf.gi1 <- prtout(out.200.au.4.gi1$qf, qf.aut.200.4.gi1, 7))
(pout.200.au.4.mt.gi1 <- prtout(out.200.au.4.gi1$mult, mt.aut.200.4.gi1, 7))

(pout.200.au.4.s.gi2 <- prtout(out.200.au.4.gi2$slod, slod.aut.200.4.gi2, 7))
(pout.200.au.4.m.gi2 <- prtout(out.200.au.4.gi2$mlod, mlod.aut.200.4.gi2, 7))
(pout.200.au.4.ss.gi2 <- prtout(out.200.au.4.gi2$ss, ss.aut.200.4.gi2, 7))
(pout.200.au.4.qf.gi2 <- prtout(out.200.au.4.gi2$qf, qf.aut.200.4.gi2, 7))
(pout.200.au.4.mt.gi2 <- prtout(out.200.au.4.gi2$mult, mt.aut.200.4.gi2, 7))

(pout.200.eq.4.s.gi1 <- prtout(out.200.eq.4.gi1$slod, slod.eq.200.4.gi1, 7))
(pout.200.eq.4.m.gi1 <- prtout(out.200.eq.4.gi1$mlod, mlod.eq.200.4.gi1, 7))
(pout.200.eq.4.ss.gi1 <- prtout(out.200.eq.4.gi1$ss, ss.eq.200.4.gi1, 7))
(pout.200.eq.4.qf.gi1 <- prtout(out.200.eq.4.gi1$qf, qf.eq.200.4.gi1, 7))
(pout.200.eq.4.mt.gi1 <- prtout(out.200.eq.4.gi1$mult, mt.eq.200.4.gi1, 7))

(pout.200.eq.4.s.gi2 <- prtout(out.200.eq.4.gi2$slod, slod.eq.200.4.gi2, 7))
(pout.200.eq.4.m.gi2 <- prtout(out.200.eq.4.gi2$mlod, mlod.eq.200.4.gi2, 7))
(pout.200.eq.4.ss.gi2 <- prtout(out.200.eq.4.gi2$ss, ss.eq.200.4.gi2, 7))
(pout.200.eq.4.qf.gi2 <- prtout(out.200.eq.4.gi2$qf, qf.eq.200.4.gi2, 7))
(pout.200.eq.4.mt.gi2 <- prtout(out.200.eq.4.gi2$mult, mt.eq.200.4.gi2, 7))

(pout.200.st.4.s.gi1 <- prtout(out.200.st.4.gi1$slod, slod.st.200.4.gi1, 7))
(pout.200.st.4.m.gi1 <- prtout(out.200.st.4.gi1$mlod, mlod.st.200.4.gi1, 7))
(pout.200.st.4.ss.gi1 <- prtout(out.200.st.4.gi1$ss, ss.st.200.4.gi1, 7))
(pout.200.st.4.qf.gi1 <- prtout(out.200.st.4.gi1$qf, qf.st.200.4.gi1, 7))
(pout.200.st.4.mt.gi1 <- prtout(out.200.st.4.gi1$mult, mt.st.200.4.gi1, 7))

(pout.200.st.4.s.gi2 <- prtout(out.200.st.4.gi2$slod, slod.st.200.4.gi2, 7))
(pout.200.st.4.m.gi2 <- prtout(out.200.st.4.gi2$mlod, mlod.st.200.4.gi2, 7))
(pout.200.st.4.ss.gi2 <- prtout(out.200.st.4.gi2$ss, ss.st.200.4.gi2, 7))
(pout.200.st.4.qf.gi2 <- prtout(out.200.st.4.gi2$qf, qf.st.200.4.gi2, 7))
(pout.200.st.4.mt.gi2 <- prtout(out.200.st.4.gi2$mult, mt.st.200.4.gi2, 7))






#### 100 - autocorr

aut100rmsea <- cbind(c(pout.100.au.1.s$rmse, pout.100.au.1.m$rmse, pout.100.au.1.ss$rmse, pout.100.au.1.qf$rmse, pout.100.au.1.mt$rmse),
                     c(pout.100.au.2.s$rmse, pout.100.au.2.m$rmse, pout.100.au.2.ss$rmse, pout.100.au.2.qf$rmse, pout.100.au.2.mt$rmse),
                     c(pout.100.au.3.s$rmse, pout.100.au.3.m$rmse, pout.100.au.3.ss$rmse, pout.100.au.3.qf$rmse, pout.100.au.3.mt$rmse),
                     c(pout.100.au.4.s$rmse, pout.100.au.4.m$rmse, pout.100.au.4.ss$rmse, pout.100.au.4.qf$rmse, pout.100.au.4.mt$rmse)
                     )

aut100powersa <- cbind(c(pout.100.au.1.s$P, pout.100.au.1.m$P, pout.100.au.1.ss$P, pout.100.au.1.qf$P, pout.100.au.1.mt$P),
                       c(pout.100.au.2.s$P, pout.100.au.2.m$P, pout.100.au.2.ss$P, pout.100.au.2.qf$P, pout.100.au.2.mt$P),
                      c(pout.100.au.3.s$P, pout.100.au.3.m$P, pout.100.au.3.ss$P, pout.100.au.3.qf$P, pout.100.au.3.mt$P),
                      c(pout.100.au.4.s$P, pout.100.au.4.m$P, pout.100.au.4.ss$P, pout.100.au.4.qf$P, pout.100.au.4.mt$P)
                      ) / 10000

### 100 - equicorr

aut100rmsee <- cbind(c(pout.100.eq.1.s$rmse, pout.100.eq.1.m$rmse, pout.100.eq.1.ss$rmse, pout.100.eq.1.qf$rmse, pout.100.eq.1.mt$rmse),
                    c(pout.100.eq.2.s$rmse, pout.100.eq.2.m$rmse, pout.100.eq.2.ss$rmse, pout.100.eq.2.qf$rmse, pout.100.eq.2.mt$rmse),
                    c(pout.100.eq.3.s$rmse, pout.100.eq.3.m$rmse, pout.100.eq.3.ss$rmse, pout.100.eq.3.qf$rmse, pout.100.eq.3.mt$rmse),
                    c(pout.100.eq.4.s$rmse, pout.100.eq.4.m$rmse, pout.100.eq.4.ss$rmse, pout.100.eq.4.qf$rmse, pout.100.eq.4.mt$rmse)
                    )

aut100powerse <- cbind(c(pout.100.eq.1.s$P, pout.100.eq.1.m$P, pout.100.eq.1.ss$P, pout.100.eq.1.qf$P, pout.100.eq.1.mt$P),
                      c(pout.100.eq.2.s$P, pout.100.eq.2.m$P, pout.100.eq.2.ss$P, pout.100.eq.2.qf$P, pout.100.eq.2.mt$P),
                      c(pout.100.eq.3.s$P, pout.100.eq.3.m$P, pout.100.eq.3.ss$P, pout.100.eq.3.qf$P, pout.100.eq.3.mt$P),
                      c(pout.100.eq.4.s$P, pout.100.eq.4.m$P, pout.100.eq.4.ss$P, pout.100.eq.4.qf$P, pout.100.eq.4.mt$P)
                      ) / 10000


### 100 - structured

aut100rmses <- cbind(c(pout.100.st.4.s$rmse, pout.100.st.4.m$rmse, pout.100.st.4.ss$rmse, pout.100.st.4.qf$rmse, pout.100.st.4.mt$rmse),
                    c(pout.100.st.1.s$rmse, pout.100.st.1.m$rmse, pout.100.st.1.ss$rmse, pout.100.st.1.qf$rmse, pout.100.st.1.mt$rmse),
                    c(pout.100.st.2.s$rmse, pout.100.st.2.m$rmse, pout.100.st.2.ss$rmse, pout.100.st.2.qf$rmse, pout.100.st.2.mt$rmse),
                    c(pout.100.st.3.s$rmse, pout.100.st.3.m$rmse, pout.100.st.3.ss$rmse, pout.100.st.3.qf$rmse, pout.100.st.3.mt$rmse)
                    )

aut100powerss <- cbind(c(pout.100.st.4.s$P, pout.100.st.4.m$P, pout.100.st.4.ss$P, pout.100.st.4.qf$P, pout.100.st.4.mt$P),
                      c(pout.100.st.1.s$P, pout.100.st.1.m$P, pout.100.st.1.ss$P, pout.100.st.1.qf$P, pout.100.st.1.mt$P),
                      c(pout.100.st.2.s$P, pout.100.st.2.m$P, pout.100.st.2.ss$P, pout.100.st.2.qf$P, pout.100.st.2.mt$P),
                      c(pout.100.st.3.s$P, pout.100.st.3.m$P, pout.100.st.3.ss$P, pout.100.st.3.qf$P, pout.100.st.3.mt$P)
                      ) / 10000




#### 200 - autocorr

aut200rmsea <- cbind(c(pout.200.au.1.s$rmse, pout.200.au.1.m$rmse, pout.200.au.1.ss$rmse, pout.200.au.1.qf$rmse, pout.200.au.1.mt$rmse),
                     c(pout.200.au.2.s$rmse, pout.200.au.2.m$rmse, pout.200.au.2.ss$rmse, pout.200.au.2.qf$rmse, pout.200.au.2.mt$rmse),
                     c(pout.200.au.3.s$rmse, pout.200.au.3.m$rmse, pout.200.au.3.ss$rmse, pout.200.au.3.qf$rmse, pout.200.au.3.mt$rmse),
                     c(pout.200.au.4.s$rmse, pout.200.au.4.m$rmse, pout.200.au.4.ss$rmse, pout.200.au.4.qf$rmse, pout.200.au.4.mt$rmse)
                     )

aut200powersa <- cbind(c(pout.200.au.1.s$P, pout.200.au.1.m$P, pout.200.au.1.ss$P, pout.200.au.1.qf$P, pout.200.au.1.mt$P),
                       c(pout.200.au.2.s$P, pout.200.au.2.m$P, pout.200.au.2.ss$P, pout.200.au.2.qf$P, pout.200.au.2.mt$P),
                      c(pout.200.au.3.s$P, pout.200.au.3.m$P, pout.200.au.3.ss$P, pout.200.au.3.qf$P, pout.200.au.3.mt$P),
                      c(pout.200.au.4.s$P, pout.200.au.4.m$P, pout.200.au.4.ss$P, pout.200.au.4.qf$P, pout.200.au.4.mt$P)
                      ) / 10000

### 200 - equicorr

aut200rmsee <- cbind(c(pout.200.eq.1.s$rmse, pout.200.eq.1.m$rmse, pout.200.eq.1.ss$rmse, pout.200.eq.1.qf$rmse, pout.200.eq.1.mt$rmse),
                    c(pout.200.eq.2.s$rmse, pout.200.eq.2.m$rmse, pout.200.eq.2.ss$rmse, pout.200.eq.2.qf$rmse, pout.200.eq.2.mt$rmse),
                    c(pout.200.eq.3.s$rmse, pout.200.eq.3.m$rmse, pout.200.eq.3.ss$rmse, pout.200.eq.3.qf$rmse, pout.200.eq.3.mt$rmse),
                    c(pout.200.eq.4.s$rmse, pout.200.eq.4.m$rmse, pout.200.eq.4.ss$rmse, pout.200.eq.4.qf$rmse, pout.200.eq.4.mt$rmse)
                    )

aut200powerse <- cbind(c(pout.200.eq.1.s$P, pout.200.eq.1.m$P, pout.200.eq.1.ss$P, pout.200.eq.1.qf$P, pout.200.eq.1.mt$P),
                      c(pout.200.eq.2.s$P, pout.200.eq.2.m$P, pout.200.eq.2.ss$P, pout.200.eq.2.qf$P, pout.200.eq.2.mt$P),
                      c(pout.200.eq.3.s$P, pout.200.eq.3.m$P, pout.200.eq.3.ss$P, pout.200.eq.3.qf$P, pout.200.eq.3.mt$P),
                      c(pout.200.eq.4.s$P, pout.200.eq.4.m$P, pout.200.eq.4.ss$P, pout.200.eq.4.qf$P, pout.200.eq.4.mt$P)
                      ) / 10000


### 200 - structured

aut200rmses <- cbind(c(pout.200.st.4.s$rmse, pout.200.st.4.m$rmse, pout.200.st.4.ss$rmse, pout.200.st.4.qf$rmse, pout.200.st.4.mt$rmse),
                    c(pout.200.st.1.s$rmse, pout.200.st.1.m$rmse, pout.200.st.1.ss$rmse, pout.200.st.1.qf$rmse, pout.200.st.1.mt$rmse),
                    c(pout.200.st.2.s$rmse, pout.200.st.2.m$rmse, pout.200.st.2.ss$rmse, pout.200.st.2.qf$rmse, pout.200.st.2.mt$rmse),
                    c(pout.200.st.3.s$rmse, pout.200.st.3.m$rmse, pout.200.st.3.ss$rmse, pout.200.st.3.qf$rmse, pout.200.st.3.mt$rmse)
                    )

aut200powerss <- cbind(c(pout.200.st.4.s$P, pout.200.st.4.m$P, pout.200.st.4.ss$P, pout.200.st.4.qf$P, pout.200.st.4.mt$P),
                      c(pout.200.st.1.s$P, pout.200.st.1.m$P, pout.200.st.1.ss$P, pout.200.st.1.qf$P, pout.200.st.1.mt$P),
                      c(pout.200.st.2.s$P, pout.200.st.2.m$P, pout.200.st.2.ss$P, pout.200.st.2.qf$P, pout.200.st.2.mt$P),
                      c(pout.200.st.3.s$P, pout.200.st.3.m$P, pout.200.st.3.ss$P, pout.200.st.3.qf$P, pout.200.st.3.mt$P)
                      ) / 10000



### 400 - autocorr

aut400rmsea <- cbind(c(pout.400.au.1.s$rmse, pout.400.au.1.m$rmse, pout.400.au.1.ss$rmse, pout.400.au.1.qf$rmse, pout.400.au.1.mt$rmse),
                     c(pout.400.au.2.s$rmse, pout.400.au.2.m$rmse, pout.400.au.2.ss$rmse, pout.400.au.2.qf$rmse, pout.400.au.2.mt$rmse),
                     c(pout.400.au.3.s$rmse, pout.400.au.3.m$rmse, pout.400.au.3.ss$rmse, pout.400.au.3.qf$rmse, pout.400.au.3.mt$rmse),
                     c(pout.400.au.4.s$rmse, pout.400.au.4.m$rmse, pout.400.au.4.ss$rmse, pout.400.au.4.qf$rmse, pout.400.au.4.mt$rmse)
                    )

aut400powersa <- cbind(c(pout.400.au.1.s$P, pout.400.au.1.m$P, pout.400.au.1.ss$P, pout.400.au.1.qf$P, pout.400.au.1.mt$P),
                      c(pout.400.au.2.s$P, pout.400.au.2.m$P, pout.400.au.2.ss$P, pout.400.au.2.qf$P, pout.400.au.2.mt$P),
                      c(pout.400.au.3.s$P, pout.400.au.3.m$P, pout.400.au.3.ss$P, pout.400.au.3.qf$P, pout.400.au.3.mt$P),
                      c(pout.400.au.4.s$P, pout.400.au.4.m$P, pout.400.au.4.ss$P, pout.400.au.4.qf$P, pout.400.au.4.mt$P)
                      ) / 10000

### 400 - equicorr

aut400rmsee <- cbind(c(pout.400.eq.1.s$rmse, pout.400.eq.1.m$rmse, pout.400.eq.1.ss$rmse, pout.400.eq.1.qf$rmse, pout.400.eq.1.mt$rmse),
                    c(pout.400.eq.2.s$rmse, pout.400.eq.2.m$rmse, pout.400.eq.2.ss$rmse, pout.400.eq.2.qf$rmse, pout.400.eq.2.mt$rmse),
                    c(pout.400.eq.3.s$rmse, pout.400.eq.3.m$rmse, pout.400.eq.3.ss$rmse, pout.400.eq.3.qf$rmse, pout.400.eq.3.mt$rmse),
                    c(pout.400.eq.4.s$rmse, pout.400.eq.4.m$rmse, pout.400.eq.4.ss$rmse, pout.400.eq.4.qf$rmse, pout.400.eq.4.mt$rmse)
                    )

aut400powerse <- cbind(c(pout.400.eq.1.s$P, pout.400.eq.1.m$P, pout.400.eq.1.ss$P, pout.400.eq.1.qf$P, pout.400.eq.1.mt$P),
                      c(pout.400.eq.2.s$P, pout.400.eq.2.m$P, pout.400.eq.2.ss$P, pout.400.eq.2.qf$P, pout.400.eq.2.mt$P),
                      c(pout.400.eq.3.s$P, pout.400.eq.3.m$P, pout.400.eq.3.ss$P, pout.400.eq.3.qf$P, pout.400.eq.3.mt$P),
                      c(pout.400.eq.4.s$P, pout.400.eq.4.m$P, pout.400.eq.4.ss$P, pout.400.eq.4.qf$P, pout.400.eq.4.mt$P)
                      ) / 10000


### 400 - structured

aut400rmses <- cbind(c(pout.400.st.4.s$rmse, pout.400.st.4.m$rmse, pout.400.st.4.ss$rmse, pout.400.st.4.qf$rmse, pout.400.st.4.mt$rmse),
                    c(pout.400.st.1.s$rmse, pout.400.st.1.m$rmse, pout.400.st.1.ss$rmse, pout.400.st.1.qf$rmse, pout.400.st.1.mt$rmse),
                    c(pout.400.st.2.s$rmse, pout.400.st.2.m$rmse, pout.400.st.2.ss$rmse, pout.400.st.2.qf$rmse, pout.400.st.2.mt$rmse),
                    c(pout.400.st.3.s$rmse, pout.400.st.3.m$rmse, pout.400.st.3.ss$rmse, pout.400.st.3.qf$rmse, pout.400.st.3.mt$rmse)
                    )

aut400powerss <- cbind(c(pout.400.st.4.s$P, pout.400.st.4.m$P, pout.400.st.4.ss$P, pout.400.st.4.qf$P, pout.400.st.4.mt$P),
                      c(pout.400.st.1.s$P, pout.400.st.1.m$P, pout.400.st.1.ss$P, pout.400.st.1.qf$P, pout.400.st.1.mt$P),
                      c(pout.400.st.2.s$P, pout.400.st.2.m$P, pout.400.st.2.ss$P, pout.400.st.2.qf$P, pout.400.st.2.mt$P),
                      c(pout.400.st.3.s$P, pout.400.st.3.m$P, pout.400.st.3.ss$P, pout.400.st.3.qf$P, pout.400.st.3.mt$P)
                      ) / 10000




#### 200 gi - autocorr

aut200rmseagi <- cbind(c(pout.200.au.2.s$rmse, pout.200.au.2.m$rmse, pout.200.au.2.ss$rmse, pout.200.au.2.qf$rmse, pout.200.au.2.mt$rmse),
                     c(pout.200.au.2.s.gi1$rmse, pout.200.au.2.m.gi1$rmse, pout.200.au.2.ss.gi1$rmse, pout.200.au.2.qf.gi1$rmse, pout.200.au.2.mt.gi1$rmse),
                     c(pout.200.au.2.s.gi2$rmse, pout.200.au.2.m.gi2$rmse, pout.200.au.2.ss.gi2$rmse, pout.200.au.2.qf.gi2$rmse, pout.200.au.2.mt.gi2$rmse)
                     )

aut200poweragi <- cbind(c(pout.200.au.2.s$P, pout.200.au.2.m$P, pout.200.au.2.ss$P, pout.200.au.2.qf$P, pout.200.au.2.mt$P),
                      c(pout.200.au.2.s.gi1$P, pout.200.au.2.m.gi1$P, pout.200.au.2.ss.gi1$P, pout.200.au.2.qf.gi1$P, pout.200.au.2.mt.gi1$P),
                      c(pout.200.au.2.s.gi2$P, pout.200.au.2.m.gi2$P, pout.200.au.2.ss.gi2$P, pout.200.au.2.qf.gi2$P, pout.200.au.2.mt.gi2$P)
                      ) / 10000

### 200 gi - equicorr


aut200rmseegi <- cbind(c(pout.200.eq.2.s$rmse, pout.200.eq.2.m$rmse, pout.200.eq.2.ss$rmse, pout.200.eq.2.qf$rmse, pout.200.eq.2.mt$rmse),
                     c(pout.200.eq.2.s.gi1$rmse, pout.200.eq.2.m.gi1$rmse, pout.200.eq.2.ss.gi1$rmse, pout.200.eq.2.qf.gi1$rmse, pout.200.eq.2.mt.gi1$rmse),
                     c(pout.200.eq.2.s.gi2$rmse, pout.200.eq.2.m.gi2$rmse, pout.200.eq.2.ss.gi2$rmse, pout.200.eq.2.qf.gi2$rmse, pout.200.eq.2.mt.gi2$rmse)
                     )

aut200poweregi <- cbind(c(pout.200.eq.2.s$P, pout.200.eq.2.m$P, pout.200.eq.2.ss$P, pout.200.eq.2.qf$P, pout.200.eq.2.mt$P),
                      c(pout.200.eq.2.s.gi1$P, pout.200.eq.2.m.gi1$P, pout.200.eq.2.ss.gi1$P, pout.200.eq.2.qf.gi1$P, pout.200.eq.2.mt.gi1$P),
                      c(pout.200.eq.2.s.gi2$P, pout.200.eq.2.m.gi2$P, pout.200.eq.2.ss.gi2$P, pout.200.eq.2.qf.gi2$P, pout.200.eq.2.mt.gi2$P)
                      ) / 10000




### 200 gi - structured


aut200rmsesgi <- cbind(c(pout.200.st.2.s$rmse, pout.200.st.2.m$rmse, pout.200.st.2.ss$rmse, pout.200.st.2.qf$rmse, pout.200.st.2.mt$rmse),
                     c(pout.200.st.2.s.gi1$rmse, pout.200.st.2.m.gi1$rmse, pout.200.st.2.ss.gi1$rmse, pout.200.st.2.qf.gi1$rmse, pout.200.st.2.mt.gi1$rmse),
                     c(pout.200.st.2.s.gi2$rmse, pout.200.st.2.m.gi2$rmse, pout.200.st.2.ss.gi2$rmse, pout.200.st.2.qf.gi2$rmse, pout.200.st.2.mt.gi2$rmse)
                     )

aut200powersgi <- cbind(c(pout.200.st.2.s$P, pout.200.st.2.m$P, pout.200.st.2.ss$P, pout.200.st.2.qf$P, pout.200.st.2.mt$P),
                      c(pout.200.st.2.s.gi1$P, pout.200.st.2.m.gi1$P, pout.200.st.2.ss.gi1$P, pout.200.st.2.qf.gi1$P, pout.200.st.2.mt.gi1$P),
                      c(pout.200.st.2.s.gi2$P, pout.200.st.2.m.gi2$P, pout.200.st.2.ss.gi2$P, pout.200.st.2.qf.gi2$P, pout.200.st.2.mt.gi2$P)
                      ) / 10000





#### 200 gi1 - autocorr

aut200rmseagi1 <- cbind(c(pout.200.au.1.s.gi1$rmse, pout.200.au.1.m.gi1$rmse, pout.200.au.1.ss.gi1$rmse, pout.200.au.1.qf.gi1$rmse, pout.200.au.1.mt.gi1$rmse),
                     c(pout.200.au.2.s.gi1$rmse, pout.200.au.2.m.gi1$rmse, pout.200.au.2.ss.gi1$rmse, pout.200.au.2.qf.gi1$rmse, pout.200.au.2.mt.gi1$rmse),
                     c(pout.200.au.3.s.gi1$rmse, pout.200.au.3.m.gi1$rmse, pout.200.au.3.ss.gi1$rmse, pout.200.au.3.qf.gi1$rmse, pout.200.au.3.mt.gi1$rmse),
                     c(pout.200.au.4.s.gi1$rmse, pout.200.au.4.m.gi1$rmse, pout.200.au.4.ss.gi1$rmse, pout.200.au.4.qf.gi1$rmse, pout.200.au.4.mt.gi1$rmse)
                     )

aut200powersagi1 <- cbind(c(pout.200.au.1.s.gi1$P, pout.200.au.1.m.gi1$P, pout.200.au.1.ss.gi1$P, pout.200.au.1.qf.gi1$P, pout.200.au.1.mt.gi1$P),
                       c(pout.200.au.2.s.gi1$P, pout.200.au.2.m.gi1$P, pout.200.au.2.ss.gi1$P, pout.200.au.2.qf.gi1$P, pout.200.au.2.mt.gi1$P),
                      c(pout.200.au.3.s.gi1$P, pout.200.au.3.m.gi1$P, pout.200.au.3.ss.gi1$P, pout.200.au.3.qf.gi1$P, pout.200.au.3.mt.gi1$P),
                      c(pout.200.au.4.s.gi1$P, pout.200.au.4.m.gi1$P, pout.200.au.4.ss.gi1$P, pout.200.au.4.qf.gi1$P, pout.200.au.4.mt.gi1$P)
                      ) / 10000

### 200 gi1- equicorr

aut200rmseegi1 <- cbind(c(pout.200.eq.1.s.gi1$rmse, pout.200.eq.1.m.gi1$rmse, pout.200.eq.1.ss.gi1$rmse, pout.200.eq.1.qf.gi1$rmse, pout.200.eq.1.mt.gi1$rmse),
                    c(pout.200.eq.2.s.gi1$rmse, pout.200.eq.2.m.gi1$rmse, pout.200.eq.2.ss.gi1$rmse, pout.200.eq.2.qf.gi1$rmse, pout.200.eq.2.mt.gi1$rmse),
                    c(pout.200.eq.3.s.gi1$rmse, pout.200.eq.3.m.gi1$rmse, pout.200.eq.3.ss.gi1$rmse, pout.200.eq.3.qf.gi1$rmse, pout.200.eq.3.mt.gi1$rmse),
                    c(pout.200.eq.4.s.gi1$rmse, pout.200.eq.4.m.gi1$rmse, pout.200.eq.4.ss.gi1$rmse, pout.200.eq.4.qf.gi1$rmse, pout.200.eq.4.mt.gi1$rmse)
                    )

aut200powersegi1 <- cbind(c(pout.200.eq.1.s.gi1$P, pout.200.eq.1.m.gi1$P, pout.200.eq.1.ss.gi1$P, pout.200.eq.1.qf.gi1$P, pout.200.eq.1.mt.gi1$P),
                      c(pout.200.eq.2.s.gi1$P, pout.200.eq.2.m.gi1$P, pout.200.eq.2.ss.gi1$P, pout.200.eq.2.qf.gi1$P, pout.200.eq.2.mt.gi1$P),
                      c(pout.200.eq.3.s.gi1$P, pout.200.eq.3.m.gi1$P, pout.200.eq.3.ss.gi1$P, pout.200.eq.3.qf.gi1$P, pout.200.eq.3.mt.gi1$P),
                      c(pout.200.eq.4.s.gi1$P, pout.200.eq.4.m.gi1$P, pout.200.eq.4.ss.gi1$P, pout.200.eq.4.qf.gi1$P, pout.200.eq.4.mt.gi1$P)
                      ) / 10000


### 200 gi1- structured

aut200rmsesgi1 <- cbind(c(pout.200.st.4.s.gi1$rmse, pout.200.st.4.m.gi1$rmse, pout.200.st.4.ss.gi1$rmse, pout.200.st.4.qf.gi1$rmse, pout.200.st.4.mt.gi1$rmse),
                    c(pout.200.st.1.s.gi1$rmse, pout.200.st.1.m.gi1$rmse, pout.200.st.1.ss.gi1$rmse, pout.200.st.1.qf.gi1$rmse, pout.200.st.1.mt.gi1$rmse),
                    c(pout.200.st.2.s.gi1$rmse, pout.200.st.2.m.gi1$rmse, pout.200.st.2.ss.gi1$rmse, pout.200.st.2.qf.gi1$rmse, pout.200.st.2.mt.gi1$rmse),
                    c(pout.200.st.3.s.gi1$rmse, pout.200.st.3.m.gi1$rmse, pout.200.st.3.ss.gi1$rmse, pout.200.st.3.qf.gi1$rmse, pout.200.st.3.mt.gi1$rmse)
                    )

aut200powerssgi1 <- cbind(c(pout.200.st.4.s.gi1$P, pout.200.st.4.m.gi1$P, pout.200.st.4.ss.gi1$P, pout.200.st.4.qf.gi1$P, pout.200.st.4.mt.gi1$P),
                      c(pout.200.st.1.s.gi1$P, pout.200.st.1.m.gi1$P, pout.200.st.1.ss.gi1$P, pout.200.st.1.qf.gi1$P, pout.200.st.1.mt.gi1$P),
                      c(pout.200.st.2.s.gi1$P, pout.200.st.2.m.gi1$P, pout.200.st.2.ss.gi1$P, pout.200.st.2.qf.gi1$P, pout.200.st.2.mt.gi1$P),
                      c(pout.200.st.3.s.gi1$P, pout.200.st.3.m.gi1$P, pout.200.st.3.ss.gi1$P, pout.200.st.3.qf.gi1$P, pout.200.st.3.mt.gi1$P)
                      ) / 10000




#### 200 gi2 - autocorr

aut200rmseagi2 <- cbind(c(pout.200.au.1.s.gi2$rmse, pout.200.au.1.m.gi2$rmse, pout.200.au.1.ss.gi2$rmse, pout.200.au.1.qf.gi2$rmse, pout.200.au.1.mt.gi2$rmse),
                     c(pout.200.au.2.s.gi2$rmse, pout.200.au.2.m.gi2$rmse, pout.200.au.2.ss.gi2$rmse, pout.200.au.2.qf.gi2$rmse, pout.200.au.2.mt.gi2$rmse),
                     c(pout.200.au.3.s.gi2$rmse, pout.200.au.3.m.gi2$rmse, pout.200.au.3.ss.gi2$rmse, pout.200.au.3.qf.gi2$rmse, pout.200.au.3.mt.gi2$rmse),
                     c(pout.200.au.4.s.gi2$rmse, pout.200.au.4.m.gi2$rmse, pout.200.au.4.ss.gi2$rmse, pout.200.au.4.qf.gi2$rmse, pout.200.au.4.mt.gi2$rmse)
                     )

aut200powersagi2 <- cbind(c(pout.200.au.1.s.gi2$P, pout.200.au.1.m.gi2$P, pout.200.au.1.ss.gi2$P, pout.200.au.1.qf.gi2$P, pout.200.au.1.mt.gi2$P),
                       c(pout.200.au.2.s.gi2$P, pout.200.au.2.m.gi2$P, pout.200.au.2.ss.gi2$P, pout.200.au.2.qf.gi2$P, pout.200.au.2.mt.gi2$P),
                      c(pout.200.au.3.s.gi2$P, pout.200.au.3.m.gi2$P, pout.200.au.3.ss.gi2$P, pout.200.au.3.qf.gi2$P, pout.200.au.3.mt.gi2$P),
                      c(pout.200.au.4.s.gi2$P, pout.200.au.4.m.gi2$P, pout.200.au.4.ss.gi2$P, pout.200.au.4.qf.gi2$P, pout.200.au.4.mt.gi2$P)
                      ) / 10000

### 200 gi2- equicorr

aut200rmseegi2 <- cbind(c(pout.200.eq.1.s.gi2$rmse, pout.200.eq.1.m.gi2$rmse, pout.200.eq.1.ss.gi2$rmse, pout.200.eq.1.qf.gi2$rmse, pout.200.eq.1.mt.gi2$rmse),
                    c(pout.200.eq.2.s.gi2$rmse, pout.200.eq.2.m.gi2$rmse, pout.200.eq.2.ss.gi2$rmse, pout.200.eq.2.qf.gi2$rmse, pout.200.eq.2.mt.gi2$rmse),
                    c(pout.200.eq.3.s.gi2$rmse, pout.200.eq.3.m.gi2$rmse, pout.200.eq.3.ss.gi2$rmse, pout.200.eq.3.qf.gi2$rmse, pout.200.eq.3.mt.gi2$rmse),
                    c(pout.200.eq.4.s.gi2$rmse, pout.200.eq.4.m.gi2$rmse, pout.200.eq.4.ss.gi2$rmse, pout.200.eq.4.qf.gi2$rmse, pout.200.eq.4.mt.gi2$rmse)
                    )

aut200powersegi2 <- cbind(c(pout.200.eq.1.s.gi2$P, pout.200.eq.1.m.gi2$P, pout.200.eq.1.ss.gi2$P, pout.200.eq.1.qf.gi2$P, pout.200.eq.1.mt.gi2$P),
                      c(pout.200.eq.2.s.gi2$P, pout.200.eq.2.m.gi2$P, pout.200.eq.2.ss.gi2$P, pout.200.eq.2.qf.gi2$P, pout.200.eq.2.mt.gi2$P),
                      c(pout.200.eq.3.s.gi2$P, pout.200.eq.3.m.gi2$P, pout.200.eq.3.ss.gi2$P, pout.200.eq.3.qf.gi2$P, pout.200.eq.3.mt.gi2$P),
                      c(pout.200.eq.4.s.gi2$P, pout.200.eq.4.m.gi2$P, pout.200.eq.4.ss.gi2$P, pout.200.eq.4.qf.gi2$P, pout.200.eq.4.mt.gi2$P)
                      ) / 10000


### 200 gi2- structured

aut200rmsesgi2 <- cbind(c(pout.200.st.4.s.gi2$rmse, pout.200.st.4.m.gi2$rmse, pout.200.st.4.ss.gi2$rmse, pout.200.st.4.qf.gi2$rmse, pout.200.st.4.mt.gi2$rmse),
                    c(pout.200.st.1.s.gi2$rmse, pout.200.st.1.m.gi2$rmse, pout.200.st.1.ss.gi2$rmse, pout.200.st.1.qf.gi2$rmse, pout.200.st.1.mt.gi2$rmse),
                    c(pout.200.st.2.s.gi2$rmse, pout.200.st.2.m.gi2$rmse, pout.200.st.2.ss.gi2$rmse, pout.200.st.2.qf.gi2$rmse, pout.200.st.2.mt.gi2$rmse),
                    c(pout.200.st.3.s.gi2$rmse, pout.200.st.3.m.gi2$rmse, pout.200.st.3.ss.gi2$rmse, pout.200.st.3.qf.gi2$rmse, pout.200.st.3.mt.gi2$rmse)
                    )

aut200powerssgi2 <- cbind(c(pout.200.st.4.s.gi2$P, pout.200.st.4.m.gi2$P, pout.200.st.4.ss.gi2$P, pout.200.st.4.qf.gi2$P, pout.200.st.4.mt.gi2$P),
                      c(pout.200.st.1.s.gi2$P, pout.200.st.1.m.gi2$P, pout.200.st.1.ss.gi2$P, pout.200.st.1.qf.gi2$P, pout.200.st.1.mt.gi2$P),
                      c(pout.200.st.2.s.gi2$P, pout.200.st.2.m.gi2$P, pout.200.st.2.ss.gi2$P, pout.200.st.2.qf.gi2$P, pout.200.st.2.mt.gi2$P),
                      c(pout.200.st.3.s.gi2$P, pout.200.st.3.m.gi2$P, pout.200.st.3.ss.gi2$P, pout.200.st.3.qf.gi2$P, pout.200.st.3.mt.gi2$P)
                      ) / 10000



save(aut100powersa, aut100powerse, aut100powerss, aut200powersa, aut200powerse,
     aut200powerss, aut400powersa, aut400powerse, aut400powerss,
     aut100rmsea, aut100rmsee, aut100rmses, aut200rmsea, aut200rmsee,
     aut200rmses, aut400rmsea, aut400rmsee, aut400rmses,
     aut200rmseagi, aut200rmseegi, aut200rmsesgi,
     aut200poweragi, aut200poweregi, aut200powersgi, file = "newrmsepower.RData")
#load("newrmsepower.RData")


save(aut200powersagi1, aut200powersegi1, aut200powerssgi1,
     aut200powersagi2, aut200powersegi2, aut200powerssgi2,
     aut200rmseagi1, aut200rmseegi1, aut200rmsesgi1,
     aut200rmseagi2, aut200rmseegi2, aut200rmsesgi2, file = "newrmsepowermore.RData")
#load("newrmsepowermore.RData")

