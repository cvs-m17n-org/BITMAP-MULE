FONTDIR = /usr/X11R6/lib/X11/fonts/misc

PCFS = bmp12-etl.pcf bmp14-etl.pcf \
	bmp16-etl.pcf bmp20-etl.pcf bmp24-etl.pcf

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

bdf: bmp12-etl.bdf bmp14-etl.bdf \
	bmp20-etl.bdf bmp24-etl.bdf


bmp12-etl.bdf: bmp16-etl.bdf
	bdfresize -f 12/16 bmp16-etl.bdf \
	  | sed "s/8x16/6x12/g" > bmp12-etl.bdf

bmp14-etl.bdf: bmp16-etl.bdf
	bdfresize -f 14/16 bmp16-etl.bdf \
	  | sed "s/8x16/7x14/g" > bmp14-etl.bdf

bmp20-etl.bdf: bmp16-etl.bdf
	bdfresize -f 20/16 bmp16-etl.bdf \
	  | sed "s/8x16/10x20/g" > bmp20-etl.bdf

bmp24-etl.bdf: bmp16-etl.bdf
	bdfresize -f 24/16 bmp16-etl.bdf \
	  | sed "s/8x16/12x24/g" > bmp24-etl.bdf

%.pcf: %.bdf
	bdftopcf $< > $@
