/* STDARG.H -- Interface to the Unix variable argument-list interface.
 * This version replaces <varargs.h>, but is NOT backwards compatible.
 *
 * We pick up the local UNIX definitions for dealing with a variable
 * number of arguments.  This is done via indirection through this file so
 * that any problems can be dealt with by changing only this file.
 *
 * Usage: Include this file using import_stdarg.  If USE_STDARG is
 * defined use the stdarg interface, otherwise use the varargs
 * interface (ifdef the code accordingly).  Old code which uses
 * varargs directly is unaffected.
 */
#ifndef D_stdarg


#ifdef __GNUC__
#include "stdarg-gcc.h"
#else
#include <stdarg.h>
#endif


#define	D_stdarg
#endif
