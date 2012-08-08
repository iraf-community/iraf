# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include "fxf.h"

# FXF_ADDPAR -- Encode a parameter in FITS format and add it to the FITS format
# IMIO userarea.

procedure fxf_addpar (im, pname, dtype, pval)

pointer	im			#I image descriptor
char	pname[ARB]		#I parameter name
int	dtype			#I SPP datatype of parameter
char	pval[ARB]		#I string encoded parameter value

bool	bval
real	rval
double	dval
short	sval
long	lval
int	ival, ip, junk
int	ctoi(), ctor(), ctod()
errchk	imadds, imaddl, imaddr, imaddd, imastr

begin
	ip  = 1

	switch (dtype) {
	case TY_BOOL:
	    bval = (pval[1] == 'T')
	    call imaddb (im, pname, bval)
	case TY_SHORT:
	    junk = ctoi (pval, ip, ival)
	    sval = ival
	    call imadds (im, pname, sval)
	case TY_INT, TY_LONG:
	    junk = ctoi (pval, ip, ival)
	    lval = ival
	    call imaddl (im, pname, lval)
	case TY_REAL:
	    junk = ctor (pval, ip, rval)
	    call imaddr (im, pname, rval)
	case TY_DOUBLE:
	    junk = ctod (pval, ip, dval)
	    call imaddd (im, pname, dval)
	default:
	    call imastr (im, pname, pval)
	}
end
