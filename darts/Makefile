TEX = darts-artifact
PDF = $(TEX).pdf
ARTIFACT_DIR = ../artifact-package

.PHONY: all clean

all: $(PDF)
	mkdir -p $(ARTIFACT_DIR)
	cp $(PDF) $(ARTIFACT_DIR)/

$(PDF): $(TEX).tex references.bib lipics-v2021.cls
	pdflatex $(TEX)
	bibtex $(TEX)
	pdflatex $(TEX)
	pdflatex $(TEX)

clean:
	rm -f $(TEX).pdf $(TEX).aux $(TEX).log $(TEX).out $(TEX).bbl $(TEX).blg
