/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_xnames
#define	import_knames
#define	import_libc
#include <iraf.h>


/* CXWHEN -- Post an exception handler.  The exception handler procedure
** is called when an exception occurs, unless the exception has been
** disabled.  If a user exception handler has not been posted and an
** exception has not been disabled, a system default exception handler is
** called when the exception occurs.
**
** Currently only four exceptions are recognized (import_xwhen):
**
**	X_INT		interrupt
**	X_ARITH		arithmetic exception (e.g. divide by zero)
**	X_ACV		access violation (e.g. illegal memory reference)
**	X_IPC		write to IPC with no reader
**
** When an exception occurs the user supplied exception handler is called 
** with the following argument list:
**
**	handler (&exception, &next_handler)
**
** The first argument is the code for the virtual exception calling the user
** handler (the same handler may be posted for more than one exception).
** The second argument is set by the user handler before exiting, and
** must be either the ZLOCPR entry point address of the next exception
** handler to be called or NULL, indicating that normal execution is to
** resume.
**
** For portability reasons, only the virtual exceptions should be used to
** post exception handlers.  For good diagnostic messages when an exception
** occurs it is desirable, however, to have a more precise description of
** the actual host system exception which occurred.  This may be obtained
** by a call to C_XGMES.
*/

#define	SZ_ERRMSG	64
typedef	int (*PFI)();		/* pointer to function returning int	*/


/* C_XWHEN -- Post an exception handler for an exception, or disable the
** exception (not all exceptions can be disabled).
*/
void
c_xwhen (
  int	exception,		/* code for virtual exception		*/
  PFI	new_handler,		/* new exception handler		*/
  PFI	*old_handler		/* old exception handler (output)	*/
)
{
	XINT	excode = exception;
	XINT	epa_new_handler = (XINT)new_handler;
	XINT	epa_old_handler;

	XWHEN (&excode, &epa_new_handler, &epa_old_handler);
	*old_handler = (PFI)epa_old_handler;
}
