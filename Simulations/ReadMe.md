## Simulation codes for "A simple regression-based method to map quantitative trait loci underlying function-valued phenotypes"

### R Packages needed

- [qtl](http://cran.r-project.org/web/packages/qtl/)
- [funqtl](http://github.com/ikwak2/funqtl)
- [splines](http://cran.r-project.org/src/contrib/Archive/splines/)
- [Matrix](http://cran.r-project.org/web/packages/Matrix/)
- [corpcor](http://cran.r-project.org/web/packages/corpcor/)
- [geoR](http://cran.r-project.org/web/packages/geoR/)
- [fda](http://cran.r-project.org/web/packages/fda/)
- [grofit](http://cran.r-project.org/web/packages/grofit/)

All of the packages except funqtl] can be downloaded from [CRAN](http://cran.r-project.org).

To install [funqtl](https://github.com/ikwak2/funqtl) from github, do
the following:

    install.packages("devtools")
    library(devtools)
    install_github("ikwak2/funqtl")

### R Code



- [`comparison_Yap.R`](comparison_Yap.R), [`fr.R`](fr.R),
  [`logistic.R`](logistic.R) &mdash; Code for [Xiong et al (2011)]'s method, downloaded from [here](http://www.epibiostat.ucsf.edu/biostat/sen/functionalMapping/)
   
- [`forsim.R`](forsim.R) &mdash; Functions that needed for the 1st simulation.

- [`simfile.R`](simfile.R) &mdash; Simulation file for the 1st simulation. Each line takes a long time.
   The result of this big simulation saved as `outputresults.RData`.

- [`simul1.R`](simul1.R) &mdash; Calculates power and RMSEs for
   simulated results in the first data set. The results are saved in
   [`../RData/newrmsepower.RData`](https://github.com/kbroman/Paper_FunQTL/blob/master/RDatas/newrmsepower.RData)
   and
   [`../RData/newrmsepowermore.RData`](https://github.com/kbroman/Paper_FunQTL/blob/master/RDatas/newrmsepowermore.RData).
   Used in Figures 5, 6, S5, and S6.


- [`estherit.R`](estherit.R) &mdash; 
   Function to get heritability values. Result saved in
   [`../RData/herit.RData`](https://github.com/kbroman/Paper_FunQTL/blob/master/RDatas/herit.RData). Used
   in Figure S4.

- [`forsim2.R`](forsim2.R) &mdash;
   Some functions needed for the 2nd simulation.

- [`cansim.R`](cansim.R) &mdash;
   Code for the 2nd simulation (threshold and stepwiseqtl search).
   The result is saved in [`QTLs.RData`](QTLs.RData)

- [`simul2res.R`](simul2res.R) &mdash;
   Calculation for Table 1
