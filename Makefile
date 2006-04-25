EMACS=emacs
SITEFLAG=--no-site-file
ALLSOURCE=$(wildcard *.el)
ALLCOMPILED=$(wildcard *.elc)
SPECIAL=emms-auto.el emms-maint.el
SOURCE=$(filter-out $(SPECIAL),$(ALLSOURCE))
TARGET=$(patsubst %.el,%.elc,$(SOURCE))
PREFIX=/usr/local
INFODIR=$(PREFIX)/info
DESTDIR=$(PREFIX)/share/emacs/site-lisp/emms


INSTALLINFO = /usr/sbin/install-info --info-dir=$(INFODIR)

# If you're using Debian, uncomment the following line and comment out
# the above line.
#INSTALLINFO = /usr/sbin/install-info --section "Emacs" "emacs" --info-dir=$(INFODIR)

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
	makeinfo $<

%.html: %.texinfo
	makeinfo --html --no-split $<

emms-print-metadata: emms-print-metadata.c
	$(CC) -o $@ $< -I/usr/include/taglib -L/usr/lib -ltag_c

install:
	test -d $(DESTDIR) || mkdir -p $(DESTDIR)
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 644 $(ALLSOURCE) $(DESTDIR)
	install -m 644 $(ALLCOMPILED) $(DESTDIR)
	install -m 0644 emms.info $(INFODIR)/emms
	$(INSTALLINFO) emms.info

remove-info:
	$(INSTALLINFO) --remove emms.info

deb-install:
	install -m 644 $(ALLSOURCE) $(DESTDIR)

ChangeLog:
	darcs changes > $@

clean:
	-rm -f *~ *.elc emms-auto.el emms.info emms.html ChangeLog \
		emms-print-metadata
