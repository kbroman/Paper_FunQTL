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

## R Code

comparison_Yap.R, fr.R, logistic.R :
```S
   Codes for Xiong et all(2011)'s method.
   Downloaded from "http://www.epibiostat.ucsf.edu/biostat/sen/functionalMapping/"
```
forsim.R :
```S
   This file contains some functions that needed for the 1st simulation.
```

simfile.R :
```S
   simulation file for the 1st simulation. each line takes a long time.
   The result of this big simulation saved as "outpoutresults.RData"
```

simul1.R :
```S
   This file produce powers, rmses of simulated data set.
   The results are saved in "../RData/newrmsepower.RData" and "../RData/newrmsepowermore.RData".
   ../R/fig5.R , ../R/fig6.R ../R/figS5.R and ../R/figS6.R functins produce  Fig5, Fig6, FigS5 and FigS6.
```

estherit.R :
```S
   Function to get heritability scores.
   Result saved in "../RData/herit.RData"
   ../R/figS4 produce FigS4.
```

forsim2.R :
```S
   This file contains some functions that needed for the 2nd simulation.
```

cansim.R :
```S
   Codes for 2nd simulation.
   Threshold and stepwiseqtl search.
   The result saved in "QTLs.RData"
```

simul2res.R :
```S
   Calculation for Table 1
```

