## Markdown extension (e.g. md, markdown, mdown).
MEXT = md

## All markdown files in the working directory
SRC = $(wildcard *.$(MEXT))

## Location of Pandoc support files.
PREFIX = /home/chris/.pandoc

## Location of your working bibliography file
BIB = ./article.bib

## CSL stylesheet (located in the csl folder of the PREFIX directory).
CSL = apsa

PDFS=$(SRC:.md=.pdf)
TEX=$(SRC:.md=.tex)

all:	$(PDFS) $(HTML) $(TEX) $(RTF) $(DOCX)

pdf:	clean $(PDFS)
tex:	clean $(TEX)

%.tex:	%.md
	pandoc -r markdown+simple_tables+table_captions+yaml_metadata_block -w latex -s -S --latex-engine=xelatex --filter pandoc-citeproc --bibliography=$(BIB) -o $@ $<

%.pdf:	%.md
	pandoc --latex-engine=xelatex --template=./default.latex -o $@ $<

clean:
	rm -f *.pdf *.tex
