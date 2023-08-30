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
        int PRINTF (XCHAR *format_string);

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
        int   FPRINTF (XINT *fd, XCHAR *format_string);

	iferr (FPRINTF (&x_fd, c_sppstr(format)))
	    return (ERR);
	else
	    return (OK);
}


extern int PARGD (XDOUBLE *dval);
extern int PARGC (XCHAR *cval);
extern int PARGS (XSHORT *sval);
extern int PARGI (XINT *ival);
extern int PARGL (XLONG *lval);
extern int PARGR (XREAL *rval);
extern int PARGB (XBOOL *bval);

void c_pargb (int    ival) { XINT    x_ival = ival; PARGI (&x_ival); }
void c_pargc (int    ival) { XINT    x_ival = ival; PARGI (&x_ival); }
void c_pargs (short  sval) { XSHORT  x_sval = sval; PARGS (&x_sval); }
void c_pargi (int    ival) { XINT    x_ival = ival; PARGI (&x_ival); }
void c_pargl (long   lval) { XLONG   x_lval = lval; PARGL (&x_lval); }
void c_pargr (float  rval) { XREAL   x_rval = rval; PARGR (&x_rval); }
void c_pargd (double dval) { XDOUBLE x_dval = dval; PARGD (&x_dval); }


void c_pargstr (char *strval)
{
    int PARGSTR (XCHAR *str);

    PARGSTR (c_sppstr(strval)); 
}
