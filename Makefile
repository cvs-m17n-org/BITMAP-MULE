#
# Makefile for bitmap-mule.
#

PACKAGE = bitmap-mule
API	= 8
RELEASE = 1

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l BITMAP-MK

PREFIX	= NONE
LISPDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc \
	  *.cp *.cps *.ky *.kys *.fn *.fns *.vr *.vrs \
	  *.pg *.pgs *.tp *.tps *.toc *.aux *.log
FILES	= README.?? Makefile BITMAP-MK BITMAP-CFG BITMAP-ELS *.el \
	  etl8x16-bitmap.bdf ChangeLog

VERSION	= $(API).$(RELEASE)
ARC_DIR = /pub/elisp/bitmap

elc:
	$(EMACS) $(FLAGS) -f compile-bitmap $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-bitmap $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

clean:
	-$(RM) $(GOMI)

tar:
	cvs commit
	sh -c 'cvs tag -RF $(PACKAGE)-`echo $(VERSION) | tr . _`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) | tr . _` \
		mu'
	cd /tmp; $(RM) $(PACKAGE)-$(VERSION)/ftp.in ; \
		$(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed -e "s/VERSION/$(VERSION)/" -e "s/API/$(API)/" \
		-e "s/PACKAGE/$(PACKAGE)/" < ftp.in > ftp

invoice:
	sed -e "s/VERSION/$(VERSION)/" -e "s/API/$(API)/" \
		-e "s/PACKAGE/$(PACKAGE)/" < ftp.in > ftp

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
