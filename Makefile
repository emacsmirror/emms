DESTDIR=
EMACS=emacs
ALLSOURCE=$(wildcard *.el)
SPECIAL=emms-auto.el emms-maint.el
SOURCE=$(filter-out $(SPECIAL),$(ALLSOURCE))
TARGET=$(patsubst %.el,%.elc,$(SOURCE))
DESTDIR=/usr/share/emacs/site-lisp/emms
INSTALLINFO=/usr/sbin/install-info

.PHONY: all install deb-install clean
.PRECIOUS: %.elc %.info %.html
all: $(TARGET) emms-auto.el emms.info

emms-auto.el: emms-auto.in $(SOURCE)
	cp emms-auto.in emms-auto.el
	-rm -f emms-auto.elc
	@$(EMACS) --no-init-file --no-site-file -batch \
		-l emms-maint.el \
		-l emms-auto.el \
		-f generate-autoloads \
		$(shell pwd)/emms-auto.el .

%.elc: %.el
	@$(EMACS) --no-init-file --no-site-file -batch \
		-l emms-maint.el \
		-f batch-byte-compile $<

%.info: %.texinfo
	makeinfo $<

%.html: %.texinfo
	makeinfo --html --no-split $<

emms-print-metadata: emms-print-metadata.c
	$(CC) -o $@ $< -I/usr/include/taglib -L/usr/lib -ltag_c

install:
	test -d $(DESTDIR) || mkdir -p $(DESTDIR)
	install -m 644 $(ALLSOURCE) $(DESTDIR)/usr/share/emacs/site-lisp/emms/
	$(INSTALLINFO) --infodir=$(DESTDIR)/usr/share/info/ emms.info

deb-install:
	install -m 644 $(ALLSOURCE) $(DESTDIR)/usr/share/emacs/site-lisp/emms/

ChangeLog:
	darcs changes > $@

clean:
	-rm -f *~ *.elc emms-auto.el emms.info emms.html ChangeLog \
		emms-print-metadata
