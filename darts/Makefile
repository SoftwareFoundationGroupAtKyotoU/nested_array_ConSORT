TEX = darts-artifact
PDF = $(TEX).pdf

.PHONY: all clean

all: $(PDF)

$(PDF): $(TEX).tex lipics-v2021.cls
	pdflatex $(TEX)
	pdflatex $(TEX)

clean:
	rm -f $(TEX).pdf $(TEX).aux $(TEX).log $(TEX).out $(TEX).bbl $(TEX).blg
