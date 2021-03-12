AC_PREREQ([2.69])
AC_INIT([Guile-zlib], [0.1], [guile-user@gnu.org], [guile-zlib],
  [https://notabug.org/guile-zlib/guile-zlib])

AC_SUBST(HVERSION, "\"0.1\"")
AC_SUBST(AUTHOR, "\"Mathieu Othacehe\"")
AC_SUBST(COPYRIGHT, "'(2020)")
AC_SUBST(LICENSE, gpl3+)
AC_CONFIG_SRCDIR(zlib.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile zlib/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your GNU Guile installation.])
fi

if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

PKG_CHECK_MODULES([LIBZ], [zlib])
AC_MSG_CHECKING([libz library directory])
PKG_CHECK_VAR([LIBZ_LIBDIR], [zlib], [libdir])
AC_MSG_RESULT([$LIBZ_LIBDIR])
AS_IF([test "x$LIBZ_LIBDIR" = "x"], [
  AC_MSG_FAILURE([Unable to identify libz lib path.])
])
AC_SUBST([LIBZ_LIBDIR])


dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
