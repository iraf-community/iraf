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
double	dval
int	dtype, axis
int	gstrcpy(), idb_kwlookup(), idb_naxis(), ltoc(), dtoc()
define	encode_ 91

begin
	# The keywords "naxis1", "naxis2", etc. are treated as a special case.
	if (idb_naxis (key, axis) == YES)
	    if (axis > 0) {
		dtype = TY_LONG
		lval  = IM_LEN(im,axis)
		goto encode_
	    }
	    
	switch (idb_kwlookup (key)) {
	case I_CTIME:
	    dtype = TY_LONG
	    lval = IM_CTIME(im)
	case I_LIMTIME:
	    dtype = TY_LONG
	    lval = IM_LIMTIME(im)
	case I_MAXPIXVAL:
	    dtype = TY_REAL
	    if (IS_INDEFR (IM_MAX(im)))
		dval = INDEFD
	    else
		dval = IM_MAX(im)
	case I_MINPIXVAL:
	    dtype = TY_REAL
	    if (IS_INDEFR (IM_MIN(im)))
		dval = INDEFD
	    else
		dval = IM_MIN(im)
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
	else
	    return (dtoc (dval, outstr, maxch, 15, 'g', maxch))
end
