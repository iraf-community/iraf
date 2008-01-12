include <ctype.h>		# for IS_WHITE
include <tbset.h>
include "tbtables.h"

# tbhgnp -- get Nth parameter
# Get the keyword and value string of header parameter number parnum.
# The string str should be SZ_PARREC in length, although not that much
# will be used since only the value will be copied to str.
# A keyword may not contain embedded blanks.
# If the parameter has an associated comment string, that string will
# not be returned; use tbhgcm to get the comment.
# Trailing blanks will be trimmed from the parameter value.
#
# Phil Hodge,  9-Mar-1989  Change dtype from char to int.
# Phil Hodge,  9-Mar-1995  Ignore comment; trim trailing blanks from value.
# Phil Hodge, 12-May-1995  Check for both ' and " as string delimiter.
# Phil Hodge,  8-Jun-1995  Modify for FITS tables.
# Phil Hodge, 27-Nov-1995  Add cmt to calling sequence of tbfgnp.

procedure tbhgnp (tp, parnum, keyword, dtype, str)

pointer tp			# i: pointer to table descriptor
int	parnum			# i: number of the parameter to be gotten
char	keyword[SZ_KEYWORD]	# o: keyword for the parameter
int	dtype			# o: data type (TY_CHAR, etc)
char	str[SZ_PARREC]		# o: string containing the value of the param.
#--
pointer sp
pointer par			# buffer for parameter record
pointer cmt			# scratch for comment from FITS record
int	k			# loop index for copying keyword
int	ip			# loop indices for copying value
int	char_type		# data type as a letter (t, b, i, r, d)
int	index			# location of comment in string
int	nchar, ctowrd()
int	strlen()
errchk	tbhrpr

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call smark (sp)
	    call salloc (cmt, SZ_PARREC, TY_CHAR)	# thrown away
	    call tbfgnp (tp, parnum, keyword, dtype, str, Memc[cmt], SZ_PARREC)
	    call sfree (sp)
	    return
	}

	if (parnum < 1 || parnum > TB_NPAR(tp)) {
	    keyword[1] = EOS
	    dtype = 0
	    str[1] = EOS
	    return
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call tbhrpr (tp, parnum, Memc[par])	# read parameter record

	# Copy the keyword to output and append EOS.
	do k = 1, SZ_KEYWORD {
	    if (Memc[par+k-1] == ' ') {		# stop at first blank
		keyword[k] = EOS
		break
	    }
	    keyword[k] = Memc[par+k-1]
	}
	keyword[SZ_KEYWORD+1] = EOS

	char_type = Memc[par+LOCN_DTYPE-1]	# data type
	switch (char_type) {
	case 'r':
	    dtype = TY_REAL
	case 'i':
	    dtype = TY_INT
	case 'd':
	    dtype = TY_DOUBLE
	case 'b':
	    dtype = TY_BOOL
	default:
	    dtype = TY_CHAR
	}

	# Find the comment, if any.
	call tbhfcm (Memc[par], index)

	# If there is a comment, chop it off.
	if (index > 0) {
	    # Backspace over whitespace.
	    while (index > START_OF_VALUE) {
		if (IS_WHITE(Memc[par+index-2]))	# element is [index-1]
		    index = index - 1
		else
		    break
	    }
	    Memc[par+index-1] = EOS
	}

	# Copy the portion of the record containing the value to output.
	ip = START_OF_VALUE
	if (Memc[par+ip-1] == '"' || Memc[par+ip-1] == '\'')
	    nchar = ctowrd (Memc[par], ip, str, SZ_PARREC)
	else
	    call strcpy (Memc[par+ip-1], str, SZ_PARREC)

	# Trim trailing blanks.
	do ip = strlen (str), 1, -1 {
	    if (str[ip] == ' ')
		str[ip] = EOS
	    else
		break
	}

	call sfree (sp)
end
