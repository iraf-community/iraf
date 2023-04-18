/*
 * IRAF.H -- Index to the IRAF/C include files and to the major IRAF
 * directories.  All directory references in the IRAF system are relative
 * to the root directories defined in this file.  The UNIX version of IRAF
 * scans this file at run time to get the logical directory definitions.
 */

#ifndef D_iraf
#define D_iraf
#endif

/* Compile time definitions (for C #ifdefs).
 */

/* Include any header files specified in "#define import_XXX" statements
 * before we were called.
 */
#ifdef import_libc
#include "iraf_libc.h"
#undef import_libc
#endif

#ifdef import_spp
#include "iraf_spp.h"
#undef import_spp
#endif

#ifdef import_main
#include "iraf_main.h"
#undef import_main
#endif

#ifdef import_stdio
#include "iraf_stdio.h"
#undef import_stdio
#endif

#ifdef import_error
#include "iraf_error.h"
#undef import_error
#endif

#ifdef import_finfo
#include "iraf_finfo.h"
#undef import_finfo
#endif

#ifdef import_fset
#include "iraf_fset.h"
#undef import_fset
#endif

#ifdef import_fpoll
#include "iraf_fpoll.h"
#undef import_fpoll
#endif

#ifdef import_kernel
#include "iraf_kernel.h"
#undef import_kernel
#endif

#ifdef import_xnames
#include "iraf_xnames.h"
#undef import_xnames
#endif

#ifdef import_vosproto
#include "iraf_vosproto.h"
#undef import_vosproto
#endif

#ifdef import_knames
#include "iraf_knames.h"
#undef import_knames
#endif

#ifdef import_kproto
#include "iraf_kproto.h"
#undef import_kproto
#endif

#ifdef import_setjmp
#include "iraf_setjmp.h"
#undef import_setjmp
#endif

#ifdef import_xwhen
#include "iraf_xwhen.h"
#undef import_xwhen
#endif

#ifdef import_protect
#include "iraf_protect.h"
#undef import_protect
#endif

#ifdef import_prtype
#include "iraf_prtype.h"
#undef import_prtype
#endif

#ifdef import_zfstat
#include "iraf_zfstat.h"
#undef import_zfstat
#endif

#ifdef import_alloc
#include "iraf_alloc.h"
#undef import_alloc
#endif

#ifdef import_math
#include "iraf_math.h"
#undef import_math
#endif

#ifdef import_prstat
#include "iraf_prstat.h"
#undef import_prstat
#endif

#ifdef import_lexnum
#include "iraf_lexnum.h"
#undef import_lexnum
#endif

#ifdef import_ttset
#include "iraf_ttset.h"
#undef import_ttset
#endif
