#
# $Id: Makefile,v 3.1 1997/11/13 20:00:01 morioka Exp $
#

VERSION	= 7.18

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l BITMAP-MK

PREFIX	= NONE
LISPDIR = NONE

TAR	= tar

#TL_FILES =	tl/README.en tl/Makefile tl/mk-tl tl/TL-ELS \
#		tl/*.el tl/doc/*.ol tl/doc/*.tex tl/doc/*.texi tl/ChangeLog

#EMU_FILES =	emu/EMU-ELS emu/*.el emu/ChangeLog

#FILES =		$(BITMAP_FILES) $(MU_FILES) $(TL_FILES) $(EMU_FILES)

#TARFILE = bitmap-mule-7.15.tar


elc:
	$(EMACS) $(FLAGS) -f compile-bitmap $(PREFIX) $(LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-bitmap $(PREFIX) $(LISPDIR)


clean:
	-rm *.elc


tar:
	cvs commit
	sh -c 'cvs tag -RF bitmap-mule-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; cvs export -d bitmap-mule-$(VERSION) \
		-r bitmap-mule-`echo $(VERSION) \
			| sed s/\\\\./_/ | sed s/\\\\./_/` SEMI/bitmap-mule'
	cd /tmp; $(TAR) cvzf bitmap-mule-$(VERSION).tar.gz \
		bitmap-mule-$(VERSION)
	cd /tmp; $(RM) -r bitmap-mule-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp
#	cd ..; tar cvf $(TARFILE) $(FILES); gzip -best $(TARFILE)

release:
	-$(RM) /pub/GNU/elisp/bitmap-mule/bitmap-mule-$(VERSION).tar.gz
	mv /tmp/bitmap-mule-$(VERSION).tar.gz /pub/GNU/elisp/bitmap-mule/
	cd /pub/GNU/elisp/mime/alpha/ ; \
		ln -s ../../bitmap-mule/bitmap-mule-$(VERSION).tar.gz .
