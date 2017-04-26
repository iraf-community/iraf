# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	<ctype.h>
include	<imhdr.h>
include	"idb.h"

# IDB_PUTSTRING -- Set the value of a standard header parameter given the new
# value of the parameter encoded as a string.  If actual type of the parameter
# is non string the value must be decoded.  ERR is returned if the key is not
# a standard header parameter.  An error action is taken if the key is known
# but the value cannot be decoded.

int procedure idb_putstring (im, key, strval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be returned
char	strval[ARB]		# string value of parameter

double	dval
bool	numeric
int	ip, axis
int	strncmp(), gstrcpy(), idb_kwlookup(), ctod(), strlen()

begin
	# Determine if the given string value is numeric.  This is true if
	# it consists of a single numeric token of a reasonable length.

	ip = 1
	numeric = false
	if (strlen (strval) < MAX_DIGITS)
	    if (ctod (strval, ip, dval) > 0) {
		while (IS_WHITE (strval[ip]) || strval[ip] == '\n')
		    ip = ip + 1
		numeric = (strval[ip] == EOS)
	    }

	# A standard keyword is recognized with or without the "i_" prefix.
	if (key[1] == 'i' && key[2] == '_')
	    ip = 3
	else
	    ip = 1

	# The keywords "naxis1", "naxis2", etc. are treated as a special case.
	if (strncmp (key[ip], "naxis", 5) == 0)
	    if (IS_DIGIT(key[ip+5]) && key[ip+6] == EOS) {
		axis = TO_INTEG(key[ip+5])
		if (numeric && axis >= 1 && axis <= IM_NDIM(im)) {
		    IM_LEN(im,axis) = nint(dval)
		    return (OK)
		} else
		    call syserrs (SYS_IDBTYPE, key)
	    }
	    
	# Lookup the keyword in the dictionary and set the value of the
	# header parameter.  If the parameter is string valued copy the
	# string value and return immediately.

	switch (idb_kwlookup (key[ip])) {
	case I_CTIME:
	    if (numeric)
		IM_CTIME(im) = nint(dval)
	case I_HISTORY:
	    return (gstrcpy (strval, IM_HISTORY(im), SZ_IMHIST))
	case I_LIMTIME:
	    if (numeric)
		IM_LIMTIME(im) = nint(dval)
	case I_MAXPIXVAL:
	    if (numeric)
		IM_MAX(im) = dval
	case I_MINPIXVAL:
	    if (numeric)
		IM_MIN(im) = dval
	case I_MTIME:
	    if (numeric)
		IM_MTIME(im) = nint(dval)
	case I_NAXIS:
	    if (numeric)
		IM_NDIM(im) = nint(dval)
	case I_PIXFILE:
	    return (gstrcpy (strval, IM_PIXFILE(im), SZ_IMPIXFILE))
	case I_PIXTYPE:
	    if (numeric)
		IM_PIXTYPE(im) = nint(dval)
	case I_TITLE:
	    return (gstrcpy (strval, IM_TITLE(im), SZ_IMTITLE))
	default:
	    return (ERR)
	}

	# If we make it through the switch, i.e., do not execute a return
	# statement, then the key was recognized and is of a numeric datatype.
	# If the value was successfully decoded as numeric then all is well,
	# else the value could not be decoded and we have an error.

	if (!numeric)
	    call syserrs (SYS_IDBTYPE, key)
	else
	    return (OK)
end
