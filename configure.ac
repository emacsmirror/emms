# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([emms], [4.0HEAD], [emms-help@gnu.org])
AM_INIT_AUTOMAKE([-Wall -Werror dist-zip])
AC_PROG_CC
AM_PATH_LISPDIR
AC_CHECK_LIB([tag_c], [taglib_file_new])
AC_CHECK_HEADER([taglib/tag_c.h], [], [AC_MSG_ERROR([taglib/tag_c.h, please install the C taglib library, or put taglib/tag_c.h somewhere it can be found]);])
AC_CHECK_HEADERS([stdlib.h])
AC_CONFIG_FILES([Makefile
                 doc/Makefile
                 lisp/Makefile
                 src/Makefile])
AC_OUTPUT
