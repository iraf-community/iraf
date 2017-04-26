/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_PRINTF -- Formatted print to STDOUT.
*/
int
c_printf (
  char	*format			/* format string */
)
{
	iferr (PRINTF (c_sppstr(format)))
	    return (ERR);
	else
	    return (OK);
}


/* C_FPRINTF -- Formatted print to given file.
*/
int
c_fprintf (
  XINT	fd,			/* output file */
  char	*format			/* format string */
)
{
	XINT  x_fd = fd;

	iferr (FPRINTF (&x_fd, c_sppstr(format)))
	    return (ERR);
	else
	    return (OK);
}


void c_pargb (int    ival) { XINT    x_ival = ival; PARGI (&x_ival); }
void c_pargc (int    ival) { XINT    x_ival = ival; PARGI (&x_ival); }
void c_pargs (short  sval) { XSHORT  x_sval = sval; PARGS (&x_sval); }
void c_pargi (int    ival) { XINT    x_ival = ival; PARGI (&x_ival); }
void c_pargl (long   lval) { XLONG   x_lval = lval; PARGL (&x_lval); }
void c_pargr (float  rval) { XREAL   x_rval = rval; PARGR (&x_rval); }
void c_pargd (double dval) { XDOUBLE x_dval = dval; PARGD (&x_dval); }


void c_pargstr (char *strval)
{
    PARGSTR (c_sppstr(strval)); 
}
