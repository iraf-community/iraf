# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<imhdr.h>
include	<mach.h>
include	"idb.h"

# IDB_PUTSTRING -- Set the value of a standard header parameter given the new
# value of the parameter encoded as a string.  If actual type of the parameter
# is non string the value must be decoded.  ERR is returned if the key is not
# a standard header parameter or if the key is known but the value cannot be
# decoded.

int procedure idb_putstring (im, key, strval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned
char	strval[ARB]		# string value of parameter

long	lval
double	dval
bool	numeric
int	ip, axis
int	idb_kwlookup(), idb_naxis(), ctod()
long	clktime()

begin
	ip = 1
	numeric = (ctod (strval, ip, dval) > 0)
	if (numeric) {
	    if (IS_INDEFD (dval))
		lval = INDEFL
	    else if (real(MAX_LONG) < abs(dval))
		lval = INDEFL
	    else
		lval = nint (dval)
	}

	# The keywords "naxis1", "naxis2", etc. are treated as a special case.
	if (idb_naxis (key, axis) == YES)
	    if (axis > 0) {
		if (numeric)
		    IM_LEN(im,axis) = lval
		else
		    return (ERR)
	    }
	    
	# Lookup the keyword in the dictionary and set the value of the
	# header parameter.  If the parameter is string valued copy the
	# string value and return immediately.

	switch (idb_kwlookup (key)) {
	case I_CTIME:
	    if (numeric)
		IM_CTIME(im) = lval
	case I_LIMTIME:
	    if (numeric)
		IM_LIMTIME(im) = lval
	case I_MAXPIXVAL:
	    if (numeric) {
		IM_MAX(im) = dval
		IM_LIMTIME(im) = clktime (long(0))
	    }
	case I_MINPIXVAL:
	    if (numeric) {
		IM_MIN(im) = dval
		IM_LIMTIME(im) = clktime (long(0))
	    }
	case I_MTIME:
	    if (numeric)
		IM_MTIME(im) = lval
	case I_NAXIS:
	    if (numeric)
		IM_NDIM(im) = lval
	case I_PIXFILE:
	    call strcpy (strval, IM_PIXFILE(im), SZ_IMPIXFILE)
	    return (OK)
	case I_PIXTYPE:
	    if (numeric)
		IM_PIXTYPE(im) = lval
	case I_TITLE:
	    call strcpy (strval, IM_TITLE(im), SZ_IMTITLE)
	    return (OK)
	default:
	    return (ERR)
	}

	# We make it here only if the actual keyword is numeric, so return
	# ERR if the keyword value was nonnumeric.

	if (numeric)
	    return (OK)
	else
	    return (ERR)
end
