/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_PRINTF -- Formatted print to STDOUT.
 */
c_printf (format)
char	*format;		/* format string */
{
	iferr (PRINTF (c_sppstr(format)))
	    return (ERR);
	else
	    return (OK);
}

/* C_FPRINTF -- Formatted print to given file.
 */
c_fprintf (fd, format)
int	fd;			/* output file */
char	*format;		/* format string */
{
	iferr (FPRINTF (&fd, c_sppstr(format)))
	    return (ERR);
	else
	    return (OK);
}

c_pargb (ival) int ival;	 { PARGI (&ival); }
c_pargc (ival) int ival;	 { PARGI (&ival); }
c_pargs (sval) short sval;	 { PARGS (&sval); }
c_pargi (ival) int ival;	 { PARGI (&ival); }
c_pargl (lval) long lval;	 { PARGL (&lval); }
c_pargr (rval) float rval;	 { PARGR (&rval); }
c_pargd (dval) double dval;	 { PARGD (&dval); }
c_pargstr (strval) char	*strval; { PARGSTR (c_sppstr(strval)); }
