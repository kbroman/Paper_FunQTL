all: funqtl.pdf funqtl_supp.pdf funqtl_combined.pdf

funqtl.pdf: funqtl.tex Figs/fig1.eps Figs/fig2.eps Figs/fig3.eps Figs/fig4.eps Figs/fig5.eps Figs/fig6.eps funqtl.bib
	pdflatex funqtl
	bibtex funqtl
	pdflatex funqtl
	pdflatex funqtl
	pdflatex funqtl

RDatas/spal_data.RData: R/prepData.R Datas/input_rev.csv
	cd R;R CMD BATCH prepData.R

Figs/fig1.eps: R/fig1.R R/plotlod.R RDatas/spal_data.RData
	cd R;R CMD BATCH fig1.R

Figs/fig2.eps: R/fig2.R RDatas/spal_data.RData
	cd R;R CMD BATCH fig2.R

Figs/fig3.eps: R/fig3.R
	cd R;R CMD BATCH fig3.R

Figs/fig4.eps: R/fig4.R RDatas/spal_data.RData
	cd R;R CMD BATCH fig4.R

Figs/fig5.eps: R/fig5.R
	cd R;R CMD BATCH fig5.R

Figs/fig6.eps: R/fig6.R
	cd R;R CMD BATCH fig6.R

Figs/figS1.eps: R/figS1.R
	cd R;R CMD BATCH figS1.R

Figs/figS2.eps: R/figS2.R
	cd R;R CMD BATCH figS2.R

Figs/figS3.eps: R/figS3.R
	cd R;R CMD BATCH figS3.R

Figs/figS4.eps: R/figS4.R
	cd R;R CMD BATCH figS4.R

Figs/figS5.eps: R/figS5.R
	cd R;R CMD BATCH figS5.R

Figs/figS6.eps: R/figS6.R
	cd R;R CMD BATCH figS6.R

Figs/figS7.eps: R/figS7.R
	cd R;R CMD BATCH figS7.R

funqtl_supp.pdf: funqtl_supp.tex Figs/figS1.eps Figs/figS2.eps Figs/figS3.eps Figs/figS4.eps Figs/figS5.eps Figs/figS6.eps Figs/figS7.eps
	xelatex funqtl_supp

funqtl_combined.pdf: funqtl.pdf funqtl_supp.pdf
	combinepdf -f funqtl.pdf funqtl_supp.pdf funqtl_combined.pdf

clean:
	\rm -f *.aux *.bbl *.blg *.log *.bak *~ *.Rout */*~ */*.Rout */*.aux */*.log

cleanall:
	\rm -f *.aux *.bbl *.blg *.log *.pdf *.bak *~ *.Rout */*~ */*.Rout */*.pdf */*.aux */*.log
