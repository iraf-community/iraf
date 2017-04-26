# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<imhdr.h>
include	"idb.h"

define	TY_STRING	(-1)

# IDB_GETSTRING -- Get the string value of a standard header parameter.  If the
# actual type of the parameter is not string the value is encoded as a string.
# The length of the string is returned as the function value.  ERR is returned
# if the string cannot be found.

int procedure idb_getstring (im, key, outstr, maxch)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned
char	outstr[ARB]		# output string to receive parameter value
int	maxch

long	lval
real	rval
int	dtype, axis, ip
int	gstrcpy(), idb_kwlookup(), strncmp(), ltoc(), strlen()
define	encode_ 91

begin
	# A standard keyword is recognized with or without the "i_" prefix.
	if (key[1] == 'i' && key[2] == '_')
	    ip = 3
	else
	    ip = 1

	# The keywords "naxis1", "naxis2", etc. are treated as a special case.
	if (strncmp (key[ip], "naxis", 5) == 0)
	    if (IS_DIGIT(key[ip+5]) && key[ip+6] == EOS) {
		dtype = TY_LONG
		axis  = TO_INTEG(key[ip+5])
		lval  = IM_LEN(im,axis)
		goto encode_
	    }
	    
	switch (idb_kwlookup (key[ip])) {
	case I_CTIME:
	    dtype = TY_LONG
	    lval = IM_CTIME(im)
	case I_HISTORY:
	    dtype = TY_STRING
	    return (gstrcpy (IM_HISTORY(im), outstr, maxch))
	case I_LIMTIME:
	    dtype = TY_LONG
	    lval = IM_LIMTIME(im)
	case I_MAXPIXVAL:
	    dtype = TY_REAL
	    rval = IM_MAX(im)
	case I_MINPIXVAL:
	    dtype = TY_REAL
	    rval = IM_MIN(im)
	case I_MTIME:
	    dtype = TY_LONG
	    lval = IM_MTIME(im)
	case I_NAXIS:
	    dtype = TY_LONG
	    lval = IM_NDIM(im)
	case I_PIXFILE:
	    return (gstrcpy (IM_PIXFILE(im), outstr, maxch))
	case I_PIXTYPE:
	    dtype = TY_LONG
	    lval = IM_PIXTYPE(im)
	case I_TITLE:
	    return (gstrcpy (IM_TITLE(im), outstr, maxch))
	default:
	    outstr[1] = EOS
	    return (ERR)
	}

encode_
	if (dtype == TY_LONG)
	    return (ltoc (lval, outstr, maxch))
	else {
	    call sprintf (outstr, maxch, "%g")
		call pargr (rval)
	    return (strlen (outstr))
	}
end
