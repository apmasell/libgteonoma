ACLOCAL_AMFLAGS = -I m4
NULL =

all: version.h

CLEANFILES = \
	gteonoma.h \
	$(libgteonoma_la_SOURCES:.vala=.c) \
	libgteonoma.vala.stamp \
	$(LIB_NAME).pc \
	$(LIB_NAME).vapi \
	vala-doc \
	.version \
	version.h \
	$(NULL)

DISTCLEANFILES = \
	lib.pc \
	$(NULL)

EXTRA_DIST = \
	lib.pc.in \
	.version \
	build-aux/git-version-gen \
	build-aux/gitlog-to-changelog \
	$(libgteonoma_la_SOURCES) \
	$(NULL)

$(LIB_NAME).pc: lib.pc
	cp lib.pc $(LIB_NAME).pc

lib_LTLIBRARIES = libgteonoma.la

valaincludedir = $(includedir)/$(LIB_NAME)
valainclude_HEADERS = gteonoma.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(LIB_NAME).pc

vapidir = $(datadir)/vala/vapi
dist_vapi_DATA = $(LIB_NAME).vapi $(LIB_NAME).deps

girdir = $(datadir)/gir-1.0
dist_gir_DATA = $(LIB_NAME).gir

VALAFLAGS = --vapidir=$(top_srcdir) -g -C

libgteonoma_la_VALAPKGS = \
	gee-$(GEE_VER) \
	$(NULL)

libgteonoma_la_CPPFLAGS = \
	$(GLIB_CFLAGS) \
	$(GEE_CFLAGS) \
	$(NULL)

libgteonoma_la_LIBADD = \
	$(GLIB_LIBS) \
	$(GEE_LIBS) \
	$(NULL)


libgteonoma_la_VALASOURCES = \
	base.vala \
	rules.vala \
	utils.vala \
	$(NULL)

libgteonoma_la_SOURCES = \
	libgteonoma.vala.stamp \
	$(libgteonoma_la_VALASOURCES:.vala=.c) \
	$(NULL)

$(libgteonoma_la_SOURCES) $(LIB_NAME).vapi $(LIB_NAME).deps $(LIB_NAME).gir gteonoma.h: $(libgteonoma_la_VALASOURCES)
	$(VALAC) $(VALAFLAGS) --library=$(LIB_NAME) --gir=$(LIB_NAME).gir -H gteonoma.h $^ `for each in $(libgteonoma_la_VALAPKGS); do echo --pkg $$each; done`
	@touch libgteonoma.vala.stamp
	echo $(libgteonoma_la_VALAPKGS) > $(LIB_NAME).deps

if ENABLE_VALADOC
VALADOCFLAGS = --package-name=$(LIB_NAME) -o doc --vapidir=$(top_srcdir)
vala-doc: $(libgteonoma_la_VALASOURCES)
	test ! -d doc || rm -r doc
	$(VALADOC) $(VALADOCFLAGS) $^ `for each in $(libgteonoma_la_VALAPKGS); do echo --pkg $$each; done` && touch vala-doc
endif

version.h: $(top_srcdir)/.version
	echo '#define BUILD_VERSION "'`cat $(top_srcdir)/.version`'"' > $@-t && mv $@-t $@

$(top_srcdir)/.version: gen-version
	@true
.PHONY: gen-version
gen-version:
	@V=`$(top_srcdir)/build-aux/git-version-gen $(top_srcdir)/.tarball-version` &&			\
	if [ -e $(top_srcdir)/.version ] && [ "x`cat $(top_srcdir)/.version`" = "x$$V" ]; then		\
	  true;												\
	else												\
	  echo "$$V" > $(top_srcdir)/.version;								\
	fi
dist-hook: gen-ChangeLog
	echo $(VERSION) > $(distdir)/.tarball-version

.PHONY: gen-ChangeLog
gen-ChangeLog:
	if test -d .git; then						\
	  $(top_srcdir)/build-aux/gitlog-to-changelog			\
	     > $(distdir)/cl-t;		\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;			\
	fi
