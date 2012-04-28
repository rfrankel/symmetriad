OUTDIR = out
SRCDIR = src

GCLFILES = $(filter-out $(SRCDIR)/init-conformal.gcl $(SRCDIR)/init-virtual.gcl,$(wildcard $(SRCDIR)/*.gcl))
PPMFILES = $(patsubst $(SRCDIR)/%.gcl,$(OUTDIR)/%.ppm,$(GCLFILES))
PSFILES = $(patsubst $(SRCDIR)/%.gcl,$(OUTDIR)/%.ps,$(GCLFILES))
PNGFILES = $(patsubst $(SRCDIR)/%.gcl,$(OUTDIR)/%.png,$(GCLFILES))

all : $(PNGFILES)

psfiles : $(PSFILES)

$(PPMFILES) : $(OUTDIR)/%.ppm : $(SRCDIR)/%.gcl | $(OUTDIR)
	{ echo '(snapshot "Conf" $@ ppm)'; echo '(quit)'; } | cat src/init-conformal.gcl $< - | geomview -c - -wins 0 -nopanels

$(PSFILES) : $(OUTDIR)/%.ps : $(SRCDIR)/%.gcl | $(OUTDIR)
	{ echo '(snapshot "Conf" $@ ps)'; echo '(quit)'; } | cat src/init-conformal.gcl $< - | geomview -c - -wins 0 -nopanels

$(PNGFILES) : %.png : %.ppm
	pnmtopng $< > $@

$(OUTDIR) :
	mkdir -p $(OUTDIR)
