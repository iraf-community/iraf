# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	"idb.h"

# IMGFTYPE -- Get the datatype of a particular field of an image header.  Since
# the internal format is FITS, there are four primary datatypes, boolean (T|F),
# string (quoted), integer and real.

int procedure imgftype (im, key)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set

pointer	rp
int	ch, ip
int	idb_findrecord(), idb_kwlookup()
errchk	syserrs

begin
	# Check for a standard header keyword.
	switch (idb_kwlookup (key)) {
	case I_CTIME:
	    return (TY_LONG)
	case I_HISTORY:
	    return (TY_CHAR)
	case I_LIMTIME:
	    return (TY_LONG)
	case I_MAXPIXVAL:
	    return (TY_REAL)
	case I_MINPIXVAL:
	    return (TY_REAL)
	case I_MTIME:
	    return (TY_LONG)
	case I_NAXIS:
	    return (TY_LONG)
	case I_PIXFILE:
	    return (TY_CHAR)
	case I_PIXTYPE:
	    return (TY_LONG)
	case I_TITLE:
	    return (TY_CHAR)
	}

	# If we get here then the named parameter is not a standard header
	# keyword.

	if (idb_findrecord (im, key, rp) > 0) {
	    # Check for quoted string.
	    ch = Memc[rp+IDB_STARTVALUE]
	    if (ch == '\'')
		return (TY_CHAR)

	    # Check for boolean field.
	    ch = Memc[rp+IDB_ENDVALUE-1]
	    if (ch == 'T' || ch == 'F')
		return (TY_BOOL)

	    # If field contains only digits it must be an integer.
	    for (ip=IDB_STARTVALUE;  ip <= IDB_ENDVALUE;  ip=ip+1) {
		ch = Memc[rp+ip-1]
		if (! (IS_DIGIT(ch) || IS_WHITE(ch)))
		    return (TY_REAL)
	    }
	    
	    return (TY_INT)
	}

	call syserrs (SYS_IDBKEYNF, key)
end
