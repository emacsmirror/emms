## Process this file with automake to produce Makefile.in

SOURCES = emms-bookmarks.el emms-browser.el emms-cache.el	\
emms-compat.el emms-cue.el emms-history.el emms-i18n.el		\
emms-info-libtag.el emms-info-metaflac.el			\
emms-info-mp3info.el emms-info-ogg.el emms-info-ogginfo.el	\
emms-info.el emms-last-played.el emms-lastfm-client.el		\
emms-lyrics.el emms-mark.el emms-metaplaylist-mode.el		\
emms-mode-line-icon.el emms-mode-line.el emms-player-mpd.el	\
emms-player-mpg321-remote.el emms-player-mplayer.el		\
emms-player-simple.el emms-player-vlc.el emms-player-xine.el	\
emms-playing-time.el emms-playlist-limit.el			\
emms-playlist-mode.el emms-playlist-sort.el emms-score.el	\
emms-setup.el emms-source-file.el emms-source-playlist.el	\
emms-stream-info.el emms-streams.el emms-tag-editor.el		\
emms-url.el emms-volume-amixer.el emms-volume.el emms.el	\
jack.el later-do.el ogg-comment.el tq.el
pkglispdir = $(lispdir)/$(PACKAGE)
dist_pkglisp_LISP=$(SOURCES) emms-autoloads.el
MAINTAINERCLEANFILES=emms-autoloads.el

emms-autoloads.el: $(SOURCES)
	$(EMACS) --batch --eval "(setq generated-autoload-file \"$(abs_builddir)/$@\")" -f batch-update-autoloads $(srcdir)
