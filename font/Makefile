FONTDIR = /usr/X11R6/lib/X11/fonts/misc

PCFS = etl6x12-bitmap.pcf etl7x14-bitmap.pcf \
	etl8x16-bitmap.pcf etl12x24-bitmap.pcf

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

bdf: etl6x12-bitmap.bdf etl7x14-bitmap.bdf etl12x24-bitmap.bdf

etl6x12-bitmap.bdf: etl8x16-bitmap.bdf
	bdfresize -f 12/16 etl8x16-bitmap.bdf \
	  | sed "s/8x16/6x12/g" > etl6x12-bitmap.bdf

etl7x14-bitmap.bdf: etl8x16-bitmap.bdf
	bdfresize -f 14/16 etl8x16-bitmap.bdf \
	  | sed "s/8x16/7x14/g" > etl7x14-bitmap.bdf

etl12x24-bitmap.bdf: etl8x16-bitmap.bdf
	bdfresize -f 24/16 etl8x16-bitmap.bdf \
	  | sed "s/8x16/12x24/g" > etl12x24-bitmap.bdf

%.pcf: %.bdf
	bdftopcf $< > $@
