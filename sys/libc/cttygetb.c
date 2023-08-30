/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYGETB -- Determine if the named capability exists for a device.
** Presence of the capability in the termcap entry for the device results
** in a return value of YES (=1), regardless of the actual datatype of
** the parameter.
*/
int
c_ttygetb (
  XINT	tty,			/* tty descriptor		*/
  char	*cap			/* two char capability name	*/
)
{
	XINT   x_tty = tty;
        XBOOL  bval;
        XBOOL  TTYGETB (XPOINTER *tty, XCHAR *cap);
        XINT   BTOI (XBOOL *boolean_value);

	bval = TTYGETB (&x_tty, c_sppstr(cap));
	return ((int) BTOI (&bval));
}
