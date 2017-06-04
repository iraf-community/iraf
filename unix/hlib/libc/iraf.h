/*
 * IRAF.H -- Index to the IRAF/C include files and to the major IRAF
 * directories.  All directory references in the IRAF system are relative
 * to the root directories defined in this file.  The UNIX version of IRAF
 * scans this file at run time to get the logical directory definitions.
 */

#ifndef D_iraf

/* Compile time definitions (for C #ifdefs).
 */

#define D_iraf
#endif

/* Include any header files specified in "#define import_XXX" statements
 * before we were called.
 */
#ifdef import_libc
#ifndef D_libc
#include "iraf/libc.h"
#endif
#undef import_libc
#endif

#ifdef import_spp
#ifndef D_spp
#include "iraf/spp.h"
#endif
#undef import_spp
#endif

#ifdef import_main
#ifndef D_main
#include "iraf/main.h"
#endif
#undef import_main
#endif

#ifdef import_stdio
#ifndef D_stdio
#include "iraf/stdio.h"
#endif
#undef import_stdio
#endif

#ifdef import_error
#ifndef D_error
#include "iraf/error.h"
#endif
#undef import_error
#endif

#ifdef import_ctype
#ifndef D_ctype
#include "iraf/ctype.h"
#endif
#undef import_ctype
#endif

#ifdef import_finfo
#ifndef D_finfo
#include "iraf/finfo.h"
#endif
#undef import_finfo
#endif

#ifdef import_fset
#ifndef D_fset
#include "iraf/fset.h"
#endif
#undef import_fset
#endif

#ifdef import_fpoll
#ifndef D_fpoll
#include "iraf/fpoll.h"
#endif
#undef import_fpoll
#endif

#ifdef import_kernel
#ifndef D_kernel
#include "iraf/kernel.h"
#endif
#undef import_kernel
#endif

#ifdef import_xnames
#ifndef D_xnames
#include "iraf/xnames.h"
#endif
#undef import_xnames
#endif

#ifdef import_knames
#ifndef D_knames
#include "iraf/knames.h"
#endif
#undef import_knames
#endif

#ifdef import_kproto
#ifndef D_kproto
#include "iraf/kproto.h"
#endif
#undef import_kproto
#endif

#ifdef import_setjmp
#ifndef D_setjmp
#include "iraf/setjmp.h"
#endif
#undef import_setjmp
#endif

#ifdef import_xwhen
#ifndef D_xwhen
#include "iraf/xwhen.h"
#endif
#undef import_xwhen
#endif

#ifdef import_protect
#ifndef D_protect
#include "iraf/protect.h"
#endif
#undef import_protect
#endif

#ifdef import_prtype
#ifndef D_prtype
#include "iraf/prtype.h"
#endif
#undef import_prtype
#endif

#ifdef import_zfstat
#ifndef D_zfstat
#include "iraf/zfstat.h"
#endif
#undef import_zfstat
#endif

#ifdef import_alloc
#ifndef D_alloc
#include "iraf/alloc.h"
#endif
#undef import_alloc
#endif

#ifdef import_math
#ifndef D_math
#include "iraf/math.h"
#endif
#undef import_math
#endif

#ifdef import_prstat
#ifndef D_prstat
#include "iraf/prstat.h"
#endif
#undef import_prstat
#endif

#ifdef import_lexnum
#ifndef D_lexnum
#include "iraf/lexnum.h"
#endif
#undef import_lexnum
#endif

#ifdef import_ttset
#ifndef D_ttset
#include "iraf/ttset.h"
#endif
#undef import_ttset
#endif

#ifdef import_stdarg
#ifndef D_stdarg
#include "iraf/stdarg.h"
#endif
#undef import_stdarg
#endif
