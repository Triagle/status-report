AC_INIT(status-report, 1.0)

AM_INIT_AUTOMAKE([foreign])

AC_PREREQ(2.12)

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_CHECK_PROG(CSC, csc, YES)

AC_CHECK_PROG(CHICKEN, chicken, YES)

AC_SUBST(CHICKENFLAGS, "-R tcp6 -R medea -R socket -R shell -R fmt -R fmt-color")

AC_OUTPUT(Makefile)
