/* VARARGS.H -- Pick up the local UNIX definitions for dealing with a variable
 * number of arguments.  This is done via indirection through this file so
 * that any problems can be dealt with by changing only this file.
 */
#ifndef D_varargs

#ifdef SUNOS

typedef char *va_list;
#if defined(sparc)
# define va_alist __builtin_va_alist
#endif
# define va_dcl int va_alist;
# define va_start(list) list = (char *) &va_alist
# define va_end(list)
# if defined(__BUILTIN_VA_ARG_INCR) && !defined(lint)
#    define va_arg(list,mode) ((mode*)__builtin_va_arg_incr((mode *)list))[0]
# else
#    define va_arg(list,mode) ((mode *)(list += sizeof(mode)))[-1]
# endif

#else

#ifdef LINUX
/* Linux has one of these but it is self-referential and causes an infinite
 * loop.
 */
#include <varargs-bsd.h>
#else
#include "/usr/include/varargs.h"
#endif

#endif

#define	D_varargs
#endif
