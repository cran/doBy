whoami=$(shell whoami)

all: 	
	make doBy.pdf
	make popMeans.pdf

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.toc *.Rnw~ 

copy:
	cp *.Rnw ../gRbase/inst/doc/
	cp *.pdf ../gRbase/inst/doc/

setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif
RPROG=	$(R_HOME)/bin/R
RSCRIPT=$(R_HOME)/bin/Rscript

doBy.pdf:
	$(RPROG) CMD Sweave --encoding=latin1 doBy.Rnw
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'doBy.tex', pdf = TRUE, clean = FALSE )"
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'doBy.tex', pdf = TRUE, clean = FALSE )"

popMeans.pdf:
	$(RPROG) CMD Sweave --encoding=latin1 popMeans.Rnw
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'popMeans.tex', pdf = TRUE, clean = FALSE )"
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'popMeans.tex', pdf = TRUE, clean = FALSE )"
