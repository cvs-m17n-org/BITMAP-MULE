FONTDIR = /usr/X11R6/lib/X11/fonts/misc/

PCFS = etl6x12-bitmap.pcf.gz etl7x14-bitmap.pcf.gz \
	etl8x16-bitmap.pcf.gz etl12x24-bitmap.pcf.gz

all: bdf $(PCFS)

install: all
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

%.pcf.gz: %.bdf
	bdftopcf $< | gzip > $@

clean::
	rm -f $(PCFS)
