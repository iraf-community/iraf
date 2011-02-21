/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_ERRACT -- Take an error action.  Typically called in an IFERR error
** handler to print a warning message or initiate error recovery after
** application specific cleanup actions have been taken.  The actions are
** defined in import_error
*/
void
c_erract (int action)
{
	XINT  x_action = action;

	ERRACT (&x_action);
}
