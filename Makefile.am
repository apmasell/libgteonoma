ACLOCAL_AMFLAGS = -I m4
NULL =

CLEANFILES = \
	gteonoma.h \
	$(LIB_NAME).pc \
	$(LIB_NAME).vapi \
	vala-doc \
	$(NULL)

DISTCLEANFILES = \
	lib.pc \
	$(NULL)

EXTRA_DIST = \
	lib.pc.in \
	build-aux/git-version-gen \
	build-aux/gitlog-to-changelog \
	$(libgteonoma_la_SOURCES) \
	$(NULL)

lib_LTLIBRARIES = libgteonoma.la

pkginclude_HEADERS = gteonoma.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = $(LIB_NAME).pc

vapidir = $(datadir)/vala/vapi
dist_vapi_DATA = $(LIB_NAME).vapi $(LIB_NAME).deps

girdir = $(datadir)/gir-1.0
dist_gir_DATA = $(LIB_NAME).gir

libgteonoma_la_COMMONVALAFLAGS =\
	--pkg gee-$(GEE_VER) \
	--vapidir=$(top_srcdir) \
	$(NULL)

libgteonoma_la_VALAFLAGS = \
	-g \
	--gir $(LIB_NAME).gir \
	-H gteonoma.h \
	--library=$(LIB_NAME) \
	$(libgteonoma_la_COMMONVALAFLAGS)

libgteonoma_la_CPPFLAGS = -g \
	$(GLIB_CFLAGS) \
	$(GEE_CFLAGS) \
	$(NULL)

libgteonoma_la_LIBADD = \
	$(GLIB_LIBS) \
	$(GEE_LIBS) \
	$(NULL)

libgteonoma_la_SOURCES = \
	base.vala \
	rules.vala \
	utils.vala \
	$(NULL)

if ENABLE_VALADOC
VALADOCFLAGS = --package-name=$(LIB_NAME) -o doc --vapidir=$(top_srcdir)
vala-doc: $(libgteonoma_la_SOURCES)
	test ! -d doc || rm -r doc
	$(VALADOC) $(VALADOCFLAGS) $(libgteonoma_la_COMMONVALAFLAGS) $^ && touch vala-doc
endif

