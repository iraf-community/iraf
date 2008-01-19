/*
 * IRAF.H -- Index to the IRAF/C include files and to the major IRAF
 * directories.  All directory references in the IRAF system are relative
 * to the root directories defined in this file.  The UNIX version of IRAF
 * scans this file at run time to get the logical directory definitions.
 */

#ifndef D_iraf
#define D_iraf

#ifndef PREFIX
#define PREFIX ""
#endif

/* ### Start of run time definitions */
#define	HOST		PREFIX "/iraf/iraf/unix/"
#define	IRAF		PREFIX "/iraf/iraf/"
#define	TMP		"/tmp/"
/* ### End of run time definitions */

/* Compile time definitions (for C #ifdefs).
 */
/* #define BSDUNIX */
/* #define SUNOS4 */

#endif

/* Include any header files specified in "#define import_XXX" statements
 * before we were called.
 */
#ifdef import_libc
#include <iraf/libc.h>
#undef import_libc
#endif

#ifdef import_spp
#include <iraf/spp.h>
#undef import_spp
#endif

#ifdef import_main
#include <iraf/main.h>
#undef import_main
#endif

#ifdef import_stdio
#include <iraf/stdio.h>
#undef import_stdio
#endif

#ifdef import_error
#include <iraf/error.h>
#undef import_error
#endif

#ifdef import_ctype
#include <iraf/ctype.h>
#undef import_ctype
#endif

#ifdef import_finfo
#include <iraf/finfo.h>
#undef import_finfo
#endif

#ifdef import_fset
#include <iraf/fset.h>
#undef import_fset
#endif

#ifdef import_fpoll
#include <iraf/fpoll.h>
#undef import_fpoll
#endif

#ifdef import_kernel
#include <iraf/kernel.h>
#undef import_kernel
#endif

#ifdef import_xnames
#include <iraf/xnames.h>
#undef import_xnames
#endif

#ifdef import_knames
#include <iraf/knames.h>
#undef import_knames
#endif

#ifdef import_setjmp
#include <iraf/setjmp.h>
#undef import_setjmp
#endif

#ifdef import_xwhen
#include <iraf/xwhen.h>
#undef import_xwhen
#endif

#ifdef import_protect
#include <iraf/protect.h>
#undef import_protect
#endif

#ifdef import_prtype
#include <iraf/prtype.h>
#undef import_prtype
#endif

#ifdef import_zfstat
#include <iraf/zfstat.h>
#undef import_zfstat
#endif

#ifdef import_alloc
#include <iraf/alloc.h>
#undef import_alloc
#endif

#ifdef import_math
#include <iraf/math.h>
#undef import_math
#endif

#ifdef import_prstat
#include <iraf/prstat.h>
#undef import_prstat
#endif

#ifdef import_lexnum
#include <iraf/lexnum.h>
#undef import_lexnum
#endif

#ifdef import_ttset
#include <iraf/ttset.h>
#undef import_ttset
#endif

#ifdef import_varargs
#include <iraf/varargs.h>
#undef import_varargs
#endif

#ifdef import_stdarg
#include <iraf/stdarg.h>
#undef import_stdarg
#endif

#ifdef import_endian
#include <iraf/endian.h>
#undef import_endian
#endif

