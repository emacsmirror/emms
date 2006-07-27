EMACS=emacs
SITEFLAG=--no-site-file
GZIP=gzip
ALLSOURCE=$(wildcard *.el)
ALLCOMPILED=$(wildcard *.elc)
SPECIAL=emms-auto.el emms-maint.el
SOURCE=$(filter-out $(SPECIAL),$(ALLSOURCE))
TARGET=$(patsubst %.el,%.elc,$(SOURCE))
MAN1PAGES=emms-print-metadata.1

DESTDIR=
PREFIX=$(DESTDIR)/usr/local
INFODIR=$(PREFIX)/info
MAN1DIR=$(PREFIX)/share/man/man1
SITELISP=$(PREFIX)/share/emacs/site-lisp/emms

INSTALLINFO = /usr/sbin/install-info --info-dir=$(INFODIR)

.PHONY: all install deb-install clean
.PRECIOUS: %.elc %.info %.html
all: $(TARGET) emms-auto.el emms.info

emms-auto.el: emms-auto.in $(SOURCE)
	cp emms-auto.in emms-auto.el
	-rm -f emms-auto.elc
	@$(EMACS) -q $(SITEFLAG) -batch \
		-l emms-maint.el \
		-l emms-auto.el \
		-f generate-autoloads \
		$(shell pwd)/emms-auto.el .

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch \
		-l emms-maint.el \
		-f batch-byte-compile $<

%.info: %.texinfo
	makeinfo --no-split $<

%.html: %.texinfo
	makeinfo --html --no-split $<

emms-print-metadata: emms-print-metadata.c
	$(CC) -o $@ $< -I/usr/include/taglib -L/usr/lib -ltag_c

install:
	test -d $(SITELISP) || mkdir -p $(SITELISP)
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 644 $(ALLSOURCE) $(SITELISP)
	install -m 644 $(ALLCOMPILED) $(SITELISP)
	install -m 0644 emms.info $(INFODIR)/emms
	for p in $(MAN1PAGES) ; do $(GZIP) -9c $$p > $(MAN1DIR)/$$p.gz ; done
	$(INSTALLINFO) emms.info

remove-info:
	$(INSTALLINFO) --remove emms.info

deb-install:
	install -m 644 $(ALLSOURCE) $(SITELISP)

ChangeLog:
	darcs changes > $@

clean:
	-rm -f *~ *.elc emms-auto.el emms.info emms.html emms-print-metadata
