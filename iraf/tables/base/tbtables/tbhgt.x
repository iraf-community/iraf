include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# Get a parameter from the table header.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  9-Mar-1989  Change dtype from char to int.
# Phil Hodge, 22-Jan-1993  Change "== INDEFD" to "IS_INDEFD".
# Phil Hodge, 15-Dec-1994  Allow converting from text string parameter.
# Phil Hodge, 30-Mar-1995  Include keyword name in error message.
# Phil Hodge,  8-Jun-1995  Modify for FITS tables.
# Phil Hodge,  7-Jun-1999  In tbhgtb, check for "yes", "y", "no", "n",
#		"true", "t", "false", "f" if a numerical value was not
#		found and data type is text.

# tbhgtb -- get Boolean header parameter
# Get a parameter from the table header.  This is for data type bool.

bool procedure tbhgtb (tp, keyword)

pointer tp			# i: pointer to table descriptor
char	keyword[ARB]		# i: name of parameter to get
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if keyword was found)
double	dblval			# buffer for reading value from string
bool	bval			# buffer for value
int	nscan()
bool	streq()
errchk	tbhfkr, tbfhgb

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhgb (tp, keyword, bval)
	    return (bval)
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call tbhfkr (tp, keyword, dtype, Memc[par], parnum)	# find keyword
	if (parnum > 0) {
	    dblval = INDEFD
	    call sscan (Memc[par])
		call gargd (dblval)		# read the value as a double
	    if (nscan() < 1 && dtype == TY_CHAR) {
		call strlwr (Memc[par])
		if (streq (Memc[par], "yes") || streq (Memc[par], "y") ||
		    streq (Memc[par], "true") || streq (Memc[par], "t")) {
		    dblval = double(YES)
		} else if (streq (Memc[par], "no") || streq (Memc[par], "n") ||
		    streq (Memc[par], "false") || streq (Memc[par], "f")) {
		    dblval = double(NO)
		}
	    }
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
			    "tbhgtb:  table header parameter `%s' not found")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}
	call sfree (sp)

	if (IS_INDEFD (dblval))
	    return (false)
	else if (nint(dblval) == YES)
	    return (true)
	else
	    return (false)
end


# tbhgtd -- get double header parameter
# Get a parameter from the table header.  This is for data type double.

double procedure tbhgtd (tp, keyword)

pointer tp			# i: pointer to table descriptor
char	keyword[ARB]		# i: name of parameter to get
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if keyword was found)
double	dblval			# buffer for reading value from string
errchk	tbhfkr, tbfhgd

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhgd (tp, keyword, dblval)
	    return (dblval)
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call tbhfkr (tp, keyword, dtype, Memc[par], parnum)	# find keyword
	if (parnum > 0) {
	    dblval = INDEFD
	    call sscan (Memc[par])
		call gargd (dblval)
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
			"tbhgtd:  table header parameter `%s' not found")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}
	call sfree (sp)

	return (dblval)
end


# tbhgti -- get integer header parameter
# Get a parameter from the table header.  This is for data type int.

int procedure tbhgti (tp, keyword)

pointer tp			# i: pointer to table descriptor
char	keyword[ARB]		# i: name of parameter to get
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if keyword was found)
double	dblval			# buffer for reading value from string
int	ival
errchk	tbhfkr, tbfhgi

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhgi (tp, keyword, ival)
	    return (ival)
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call tbhfkr (tp, keyword, dtype, Memc[par], parnum)	# find keyword
	if (parnum > 0) {
	    dblval = INDEFD
	    call sscan (Memc[par])
		call gargd (dblval)		# read the value as a double
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
			"tbhgti:  table header parameter `%s' not found")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}
	call sfree (sp)

	if (IS_INDEFD (dblval))
	    return (INDEFI)
	else
	    return (nint(dblval))
end


# tbhgtr -- get real header parameter
# Get a parameter from the table header.  This is for data type real.

real procedure tbhgtr (tp, keyword)

pointer tp			# i: pointer to table descriptor
char	keyword[ARB]		# i: name of parameter to get
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if keyword was found)
real	realval			# buffer for reading value from string
errchk	tbhfkr, tbfhgr

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhgr (tp, keyword, realval)
	    return (realval)
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call tbhfkr (tp, keyword, dtype, Memc[par], parnum)	# find keyword
	if (parnum > 0) {
	    realval = INDEFR
	    call sscan (Memc[par])
		call gargr (realval)
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
			"tbhgtr:  table header parameter `%s' not found")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}
	call sfree (sp)

	return (realval)
end


# tbhgtt -- get character header parameter
# Get a parameter from the table header.  This is for character data type.

procedure tbhgtt (tp, keyword, text, maxch)

pointer tp			# i: pointer to table descriptor
char	keyword[ARB]		# i: name of parameter
char	text[ARB]		# o: value of parameter
int	maxch			# i: maximum number of characters to get
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if keyword was found)
errchk	tbhfkr, tbfhgt

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhgt (tp, keyword, text, maxch)
	    return
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call tbhfkr (tp, keyword, dtype, Memc[par], parnum)	# find keyword
	if (parnum > 0) {
	    call strcpy (Memc[par], text, maxch)
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
			"tbhgtt:  table header parameter `%s' not found")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}
	call sfree (sp)
end
