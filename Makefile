#
# $Id: Makefile,v 1.1 1996/11/27 15:38:38 morioka Exp morioka $
#

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l BITMAP-MK

PREFIX =

BITMAP_FILES =	bitmap-mule/Makefile bitmap-mule/BITMAP-* \
		bitmap-mule/*.el bitmap-mule/*.bdf bitmap-mule/README.en

MU_FILES =	mu/MU-ELS mu/*.el mu/ChangeLog

TL_FILES =	tl/README.en tl/Makefile tl/mk-tl tl/TL-ELS \
		tl/*.el tl/doc/*.ol tl/doc/*.tex tl/doc/*.texi tl/ChangeLog

EMU_FILES =	emu/EMU-ELS emu/*.el emu/ChangeLog

FILES =		$(BITMAP_FILES) $(MU_FILES) $(TL_FILES) $(EMU_FILES)

TARFILE = bitmap-mule-7.15.tar


elc:
	$(EMACS) $(FLAGS) -f compile-bitmap

install:	elc
	$(EMACS) $(FLAGS) -f install-bitmap $(PREFIX)


clean:
	-rm *.elc


tar:
	cd ..; tar cvf $(TARFILE) $(FILES); gzip -best $(TARFILE)
