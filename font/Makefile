FONTDIR = /usr/X11R6/lib/X11/fonts/misc

EMACS = emacs

PCFS = bitmap12.pcf bitmap14.pcf \
	bitmap16.pcf bitmap20.pcf bitmap24.pcf

all: bdf $(PCFS)

clean::
	rm -f $(PCFS)

install: all
	for f in $(PCFS); do \
	  install -m 644 $$f $(FONTDIR); \
	  gzip -f $(FONTDIR)/$$f; \
	done
	(cd $(FONTDIR); mkfontdir)
	xset fp rehash

install-nogzip: all
	for f in $(PCFS); do \
	  install -m 644 $$f $(FONTDIR); \
	done
	(cd $(FONTDIR); mkfontdir)
	xset fp rehash

bdf: bitmap16.bdf bitmap12.bdf bitmap14.bdf bitmap20.bdf bitmap24.bdf

bitmap16.bdf:
	$(EMACS) -batch -q -no-site-file \
	  -l bitmap16.el -f generate-bitmap-bdf-font

bitmap12.bdf: bitmap16.bdf
	bdfresize -f 12/16 bitmap16.bdf | sed "s/8x16/6x12/g" > bitmap12.bdf

bitmap14.bdf: bitmap16.bdf
	bdfresize -f 14/16 bitmap16.bdf | sed "s/8x16/7x14/g" > bitmap14.bdf

bitmap20.bdf: bitmap16.bdf
	bdfresize -f 20/16 bitmap16.bdf | sed "s/8x16/10x20/g" > bitmap20.bdf

bitmap24.bdf: bitmap16.bdf
	bdfresize -f 24/16 bitmap16.bdf | sed "s/8x16/12x24/g" > bitmap24.bdf

%.pcf: %.bdf
	bdftopcf $< > $@
