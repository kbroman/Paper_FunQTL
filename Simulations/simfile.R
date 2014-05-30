## to get the simulation result described in Fig5 and 6

library(qtl)
library(fda)
source("fr.R")
source("logistic.R")
source("comparison_yap.R")
library(funqtl)
library(grofit)
source("forsim.R")


beta.coef <- rbind( c(29, 7, .7),
                   c(28.5, 6.5, .73),
                   c(27.5, 5, .75)
                   )

out100aut1 <- compsim3(10000,100, "autocorr", beta.coef, 1)
# get 10000 simulation results of each methods(slod, mlod, sens(ss), sens(qf) and multi-trait qtl mapping) for the data set with 100 individual, "autocorr" variance structure, with beta.coef coefficients with c = 1

out100eq1 <- compsim3(10000,100, "equicorr", beta.coef, 1)
out100st1 <- compsim3(10000,100, "structured", beta.coef, 1)

out100aut2 <- compsim3(10000,100, "autocorr", beta.coef, 2)
out100eq2 <- compsim3(10000,100, "equicorr", beta.coef, 2)
out100st2 <- compsim3(10000,100, "structured", beta.coef, 2)

out100aut3 <- compsim3(10000,100, "autocorr", beta.coef, 3)
out100eq3 <- compsim3(10000,100, "equicorr", beta.coef, 3)
out100st3 <- compsim3(10000,100, "structured", beta.coef, 3)

out100aut4 <- compsim3(10000,100, "autocorr", beta.coef, 6)
out100eq4 <- compsim3(10000,100, "equicorr", beta.coef, 6)
out100st4 <- compsim3(10000,100, "structured", beta.coef, .5)


out200aut1 <- compsim3(10000,200, "autocorr", beta.coef, 1)
out200eq1 <- compsim3(10000,200, "equicorr", beta.coef, 1)
out200st1 <- compsim3(10000,200, "structured", beta.coef, 1)

out200aut2 <- compsim3(10000,200, "autocorr", beta.coef, 2)
out200eq2 <- compsim3(10000,200, "equicorr", beta.coef, 2)
out200st2 <- compsim3(10000,200, "structured", beta.coef, 2)

out200aut3 <- compsim3(10000,200, "autocorr", beta.coef, 3)
out200eq3 <- compsim3(10000,200, "equicorr", beta.coef, 3)
out200st3 <- compsim3(10000,200, "structured", beta.coef, 3)

out200aut4 <- compsim3(10000,200, "autocorr", beta.coef, 6)
out200eq4 <- compsim3(10000,200, "equicorr", beta.coef, 6)
out200st4 <- compsim3(10000,200, "structured", beta.coef, .5)


out400aut1 <- compsim3(10000,400, "autocorr", beta.coef, 1)
out400eq1 <- compsim3(10000,400, "equicorr", beta.coef, 1)
out400st1 <- compsim3(10000,400, "structured", beta.coef, 1)

out400aut2 <- compsim3(10000,400, "autocorr", beta.coef, 2)
out400eq2 <- compsim3(10000,400, "equicorr", beta.coef, 2)
out400st2 <- compsim3(10000,400, "structured", beta.coef, 2)

out400aut3 <- compsim3(10000,400, "autocorr", beta.coef, 3)
out400eq3 <- compsim3(10000,400, "equicorr", beta.coef, 3)
out400st3 <- compsim3(10000,400, "structured", beta.coef, 3)

out400aut4 <- compsim3(10000,400, "autocorr", beta.coef, 6)
out400eq4 <- compsim3(10000,400, "equicorr", beta.coef, 6)
out400st4 <- compsim3(10000,400, "structured", beta.coef, .5)



pout100aut1 <- permsim3(10000,100, "autocorr", beta.coef, 1)
# get a list of permutation results of 10000 iteration with 100 individual data, "autocorr" variance structure, with beta.coef beta coefficient and c = 1

pout100eq1 <- permsim3(10000,100, "equicorr", beta.coef, 1)
pout100st1 <- permsim3(10000,100, "structured", beta.coef, 1)

pout100aut2 <- permsim3(10000,100, "autocorr", beta.coef, 2)
pout100eq2 <- permsim3(10000,100, "equicorr", beta.coef, 2)
pout100st2 <- permsim3(10000,100, "structured", beta.coef, 2)

pout100aut3 <- permsim3(10000,100, "autocorr", beta.coef, 3)
pout100eq3 <- permsim3(10000,100, "equicorr", beta.coef, 3)
pout100st3 <- permsim3(10000,100, "structured", beta.coef, 3)

pout100aut4 <- permsim3(10000,100, "autocorr", beta.coef, 6)
pout100eq4 <- permsim3(10000,100, "equicorr", beta.coef, 6)
pout100st4 <- permsim3(10000,100, "structured", beta.coef, .5)



pout200aut1 <- permsim3(10000,200, "autocorr", beta.coef, 1)
pout200eq1 <- permsim3(10000,200, "equicorr", beta.coef, 1)
pout200st1 <- permsim3(10000,200, "structured", beta.coef, 1)

pout200aut2 <- permsim3(10000,200, "autocorr", beta.coef, 2)
pout200eq2 <- permsim3(10000,200, "equicorr", beta.coef, 2)
pout200st2 <- permsim3(10000,200, "structured", beta.coef, 2)

pout200aut3 <- permsim3(10000,200, "autocorr", beta.coef, 3)
pout200eq3 <- permsim3(10000,200, "equicorr", beta.coef, 3)
pout200st3 <- permsim3(10000,200, "structured", beta.coef, 3)

pout200aut4 <- permsim3(10000,200, "autocorr", beta.coef, 6)
pout200eq4 <- permsim3(10000,200, "equicorr", beta.coef, 6)
pout200st4 <- permsim3(10000,200, "structured", beta.coef, .5)



pout400aut1 <- permsim3(10000,400, "autocorr", beta.coef, 1)
pout400eq1 <- permsim3(10000,400, "equicorr", beta.coef, 1)
pout400st1 <- permsim3(10000,400, "structured", beta.coef, 1)

pout400aut2 <- permsim3(10000,400, "autocorr", beta.coef, 2)
pout400eq2 <- permsim3(10000,400, "equicorr", beta.coef, 2)
pout400st2 <- permsim3(10000,400, "structured", beta.coef, 2)

pout400aut3 <- permsim3(10000,400, "autocorr", beta.coef, 3)
pout400eq3 <- permsim3(10000,400, "equicorr", beta.coef, 3)
pout400st3 <- permsim3(10000,400, "structured", beta.coef, 3)

pout400aut4 <- permsim3(10000,400, "autocorr", beta.coef, 6)
pout400eq4 <- permsim3(10000,400, "equicorr", beta.coef, 6)
pout400st4 <- permsim3(10000,400, "structured", beta.coef, .5)



out200aut1gi1 <- compsim3(10000,200, "autocorr", beta.coef, 1, gi = 1)
out200eq1gi1 <- compsim3(10000,200, "equicorr", beta.coef, 1, gi = 1)
out200st1gi1 <- compsim3(10000,200, "structured", beta.coef, 1, gi = 1)
out200aut1gi2 <- compsim3(10000,200, "autocorr", beta.coef, 1, gi = 2)
out200eq1gi2 <- compsim3(10000,200, "equicorr", beta.coef, 1, gi = 2)
out200st1gi2 <- compsim3(10000,200, "structured", beta.coef, 1, gi = 2)

out200aut2gi1 <- compsim3(10000,200, "autocorr", beta.coef, 2, gi = 1)
out200eq2gi1 <- compsim3(10000,200, "equicorr", beta.coef, 2, gi = 1)
out200st2gi1 <- compsim3(10000,200, "structured", beta.coef, 2, gi = 1)
out200aut2gi2 <- compsim3(10000,200, "autocorr", beta.coef, 2, gi = 2)
out200eq2gi2 <- compsim3(10000,200, "equicorr", beta.coef, 2, gi = 2)
out200st2gi2 <- compsim3(10000,200, "structured", beta.coef, 2, gi = 2)

out200aut3gi1 <- compsim3(10000,200, "autocorr", beta.coef, 3, gi = 1)
out200eq3gi1 <- compsim3(10000,200, "equicorr", beta.coef, 3, gi = 1)
out200st3gi1 <- compsim3(10000,200, "structured", beta.coef, 3, gi = 1)
out200aut3gi2 <- compsim3(10000,200, "autocorr", beta.coef, 3, gi = 2)
out200eq3gi2 <- compsim3(10000,200, "equicorr", beta.coef, 3, gi = 2)
out200st3gi2 <- compsim3(10000,200, "structured", beta.coef, 3, gi = 2)

out200aut4gi1 <- compsim3(10000,200, "autocorr", beta.coef, 6, gi = 1)
out200eq4gi1 <- compsim3(10000,200, "equicorr", beta.coef, 6, gi = 1)
out200st4gi1 <- compsim3(10000,200, "structured", beta.coef, .5, gi = 1)
out200aut4gi2 <- compsim3(10000,200, "autocorr", beta.coef, 6, gi = 2)
out200eq4gi2 <- compsim3(10000,200, "equicorr", beta.coef, 6, gi = 2)
out200st4gi2 <- compsim3(10000,200, "structured", beta.coef, .5, gi = 2)


pout200aut1gi1 <- permsim3(10000,200, "autocorr", beta.coef, 1, gi = 1)
pout200eq1gi1 <- permsim3(10000,200, "equicorr", beta.coef, 1, gi = 1)
pout200st1gi1 <- permsim3(10000,200, "structured", beta.coef, 1, gi = 1)
pout200aut1gi2 <- permsim3(10000,200, "autocorr", beta.coef, 1, gi = 2)
pout200eq1gi2 <- permsim3(10000,200, "equicorr", beta.coef, 1, gi = 2)
pout200st1gi2 <- permsim3(10000,200, "structured", beta.coef, 1, gi = 2)

pout200aut2gi1 <- permsim3(10000,200, "autocorr", beta.coef, 2, gi = 1)
pout200eq2gi1 <- permsim3(10000,200, "equicorr", beta.coef, 2, gi = 1)
pout200st2gi1 <- permsim3(10000,200, "structured", beta.coef, 2, gi = 1)
pout200aut2gi2 <- permsim3(10000,200, "autocorr", beta.coef, 2, gi = 2)
pout200eq2gi2 <- permsim3(10000,200, "equicorr", beta.coef, 2, gi = 2)
pout200st2gi2 <- permsim3(10000,200, "structured", beta.coef, 2, gi = 2)

pout200aut3gi1 <- permsim3(10000,200, "autocorr", beta.coef, 3, gi = 1)
pout200eq3gi1 <- permsim3(10000,200, "equicorr", beta.coef, 3, gi = 1)
pout200st3gi1 <- permsim3(10000,200, "structured", beta.coef, 3, gi = 1)
pout200aut3gi2 <- permsim3(10000,200, "autocorr", beta.coef, 3, gi = 2)
pout200eq3gi2 <- permsim3(10000,200, "equicorr", beta.coef, 3, gi = 2)
pout200st3gi2 <- permsim3(10000,200, "structured", beta.coef, 3, gi = 2)

pout200aut4gi1 <- permsim3(10000,200, "autocorr", beta.coef, 6, gi = 1)
pout200eq4gi1 <- permsim3(10000,200, "equicorr", beta.coef, 6, gi = 1)
pout200st4gi1 <- permsim3(10000,200, "structured", beta.coef, .5, gi = 1)
pout200aut4gi2 <- permsim3(10000,200, "autocorr", beta.coef, 6, gi = 2)
pout200eq4gi2 <- permsim3(10000,200, "equicorr", beta.coef, 6, gi = 2)
pout200st4gi2 <- permsim3(10000,200, "structured", beta.coef, .5, gi = 2)
