#
# Makefile for BITMAP-MULE.
#

PACKAGE	= bitmap-mule
API	= 8
RELEASE	= 5
BITMAP_FONTS	= bitmap-fonts-1.0

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
MKDIR	= /bin/mkdir

EMACS	= emacs
FLAGS	= -batch -q -no-site-file -l BITMAP-MK

PREFIX	= NONE
LISPDIR	= NONE
VERSION_SPECIFIC_LISPDIR	= NONE

GOMI	= *.elc \
	  *.cp *.cps *.ky *.kys *.fn *.fns *.vr *.vrs \
	  *.pg *.pgs *.tp *.tps *.toc *.aux *.log

VERSION	= $(API).$(RELEASE)
ARC_DIR	= /pub/elisp/bitmap

elc:
	$(EMACS) $(FLAGS) -f compile-bitmap $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-bitmap $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

clean:
	-$(RM) $(GOMI)

tar:
#	cvs commit
#	sh -c 'cvs tag -RF $(PACKAGE)-`echo $(VERSION) | tr . _`; \
#	cd /tmp; \
#	cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/root \
#		export -d $(PACKAGE)-$(VERSION) \
#		-r $(PACKAGE)-`echo $(VERSION) | tr . _` \
#		bitmap-mule'
	$(RM) -r /tmp/$(PACKAGE)-$(VERSION); \
		$(MKDIR) /tmp/$(PACKAGE)-$(VERSION); \
		$(TAR) cf - . | $(TAR) xf - -C /tmp/$(PACKAGE)-$(VERSION); \
		cd /tmp/$(PACKAGE)-$(VERSION); \
		$(RM) -r CVS font/CVS
	cd /tmp; $(RM) $(PACKAGE)-$(VERSION)/ftp.in; \
		GZIP=-9; export GZIP; \
		$(TAR) czf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; cd $(PACKAGE)-$(VERSION)/font; \
		make bdf; \
		GZIP=-9; export GZIP; \
		$(TAR) czf /tmp/$(BITMAP_FONTS).tar.gz *.bdf
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed -e "s/VERSION/$(VERSION)/" -e "s/API/$(API)/" \
		-e "s/PACKAGE/$(PACKAGE)/" < ftp.in

invoice:
	sed -e "s/VERSION/$(VERSION)/" -e "s/API/$(API)/" \
		-e "s/PACKAGE/$(PACKAGE)/" < ftp.in

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
