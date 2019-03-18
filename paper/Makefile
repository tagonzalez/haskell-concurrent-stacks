all: ./chapters/**/*.tex
	pdflatex -shell-escape tesis.tex
	biber tesis
	pdflatex -shell-escape tesis.tex
	pdflatex -shell-escape tesis.tex
	open tesis.pdf

.PHONY: clean

clean:
	rm -rf ./**/*.aux _minted-tesis ./**/*.toc ./**/*.log ./**/*.fdb_latexmk ./**/*.fls *.aux *.log *.toc *.bbl *.bcf *.blg *.out *.xml