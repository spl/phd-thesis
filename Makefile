# Disable implicit and built-in rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

#-------------------------------------------------------------------------------
# Variables

# Directories
OUTDIR := out
SRCDIR := src
FMTDIR := fmt
HSDIR  := hs
STYDIR := sty
BIBDIR := bib

# Files
TEXFILES := $(patsubst $(SRCDIR)/%.lhs, $(OUTDIR)/%.tex, $(wildcard $(SRCDIR)/*.lhs))
FMTFILES := $(OUTDIR)/gen.fmt $(patsubst $(FMTDIR)/%, $(OUTDIR)/%, $(wildcard $(FMTDIR)/*.fmt))
STYFILES := $(patsubst $(STYDIR)/%, $(OUTDIR)/%, $(wildcard $(STYDIR)/*.sty))
BIBFILES := $(patsubst $(BIBDIR)/%, $(OUTDIR)/%, $(wildcard $(BIBDIR)/*.bib))

# latexmk
LATEXMK	     := latexmk
LATEXMKFLAGS := -silent -halt-on-error -file-line-error -bibtex -xelatex -pv

# lhs2TeX
LHS2TEX      := lhs2TeX
LHS2TEXFLAGS := -v --poly

#-------------------------------------------------------------------------------
# Phony Rules

.PHONY: default clean

default: $(OUTDIR)/main.pdf

clean:
	$(RM) -r $(OUTDIR)

#-------------------------------------------------------------------------------
# Specific Rules

$(OUTDIR)/main.pdf: $(TEXFILES) $(STYFILES) $(BIBFILES)
	(cd $(OUTDIR); $(LATEXMK) $(LATEXMKFLAGS) main.tex)
	touch $@

$(OUTDIR):
	mkdir $@

$(OUTDIR)/gen.fmt: $(HSDIR)/dist/build/fmt/fmt
	$< > $@

$(HSDIR)/dist/build/fmt/fmt: $(HSDIR)/Format.hs $(HSDIR)/Main.hs $(HSDIR)/fmt.cabal
	(cd $(HSDIR); cabal configure; cabal build)

# The output directory is an order-only prerequisite.
$(TEXFILES) $(FMTFILES) $(STYFILES) $(BIBFILES): | $(OUTDIR)

#-------------------------------------------------------------------------------
# Pattern Rules

$(OUTDIR)/%.tex: $(OUTDIR)/%.lhs $(FMTFILES)
	(cd $(OUTDIR); $(LHS2TEX) $(LHS2TEXFLAGS) -o $(notdir $@) $(notdir $<))

$(OUTDIR)/%.lhs: $(SRCDIR)/%.lhs
	cp $< $@

$(OUTDIR)/%.fmt: $(FMTDIR)/%.fmt
	cp $< $@

$(OUTDIR)/%.sty: $(STYDIR)/%.sty
	cp $< $@

$(OUTDIR)/%.bib: $(BIBDIR)/%.bib
	cp $< $@

