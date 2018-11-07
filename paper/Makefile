all: tesis.pdf
	pdflatex -shell-escape tesis.tex && pdflatex -shell-escape tesis.tex
	open tesis.pdf

.PHONY: clean

clean:
	rm -rf ./**/*.aux _minted-tesis ./**/*.toc ./**/*.log ./**/*.fdb_latexmk ./**/*.cls ./**/*.fls *.aux *.cls *.log *.toc