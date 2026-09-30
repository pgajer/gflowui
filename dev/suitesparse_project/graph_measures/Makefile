# The desktop LaTeX editor also compiles this standalone source directly.
OUT ?= build
CITATION_CHECK ?= $(HOME)/.codex/notes/agent_instructions/reports/scripts/check_citation_verification.py
.PHONY: pdf citation-check
pdf:
	mkdir -p $(OUT)
	pdflatex -interaction=nonstopmode -halt-on-error -output-directory=$(OUT) graph_measures.tex
	pdflatex -interaction=nonstopmode -halt-on-error -output-directory=$(OUT) graph_measures.tex
citation-check:
	python3 $(CITATION_CHECK) --tex graph_measures.tex --bib references.bib --html citation_verification.html --log $(OUT)/graph_measures.log
