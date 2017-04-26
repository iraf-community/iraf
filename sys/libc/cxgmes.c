/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_knames
#include <iraf.h>


/* C_XGMES -- Fetch the machine dependent integer code and message string
** for the most recent exception.  The integer code XOK is returned if
** no exception has occurred or if C_XGMES is called more than once after
** a single exception.
*/
void
c_xgmes (
  int	*oscode,		/* os integer code of exception		*/
  char	*oserrmsg,		/* os error message string		*/
  int	maxch
)
{
	PKCHAR	x_oserrmsg[SZ_LINE+1];
	XINT	x_oscode = *oscode, x_maxch = SZ_LINE;

	ZXGMES (&x_oscode, x_oserrmsg, &x_maxch);
	(void) strncpy (oserrmsg, (char *)x_oserrmsg, maxch);
	*oscode = x_oscode;
}
