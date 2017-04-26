include <ctype.h>		# for IS_WHITE
include <tbset.h>
include "tbtables.h"

# tbhpcm -- add a comment to a header parameter
# This adds a comment to a header parameter, or replaces one that is
# already there.  It is an error if the header parameter is not found.
# Nothing is done if the table is of type text; we can't find the
# keyword in the header because there is no header.  If the keyword
# is HISTORY, COMMENT, or blank, this routine returns without adding
# anything and without error.
#
# Phil Hodge,  6-Mar-1995  Subroutine created.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 12-May-1995  Change string delimiter from " to '.
# Phil Hodge, 14-Jun-1995  Modify for FITS tables.
# Phil Hodge,  7-Jun-1999  Handle text tables.

procedure tbhpcm (tp, keyword, comment)

pointer tp			# i: pointer to table descriptor
char	keyword[SZ_KEYWORD]	# i: keyword to be found
char	comment[ARB]		# i: comment string for keyword
#--
pointer sp
pointer str			# scratch for string read from header
pointer value			# scratch for the string value
pointer errmsg			# scratch for possible error message
int	parnum			# number of the parameter
int	ip, nchar, ctowrd()
int	strlen()
bool	tbhisc()
errchk	tbfpcm, tbhfkw, tbhrpr, tbhwpr

begin
	if (comment[1] == EOS)
	    return

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfpcm (tp, keyword, comment)
	    TB_MODIFIED(tp) = true
	    return
	}

	# We don't add a comment to a comment.
	if (tbhisc (keyword))
	    return

	call smark (sp)
	call salloc (str, SZ_PARREC, TY_CHAR)
	call salloc (value, SZ_PARREC, TY_CHAR)

	# Find the keyword in the header.
	call tbhfkw (tp, keyword, parnum)
	if (parnum < 1) {
	    call salloc (errmsg, SZ_FNAME, TY_CHAR)
	    call sprintf (Memc[errmsg], SZ_FNAME,
			"tbhpcm:  keyword `%s' not found in table `%s'")
		call pargstr (keyword)
		call pargstr (TB_NAME(tp))
	    call error (1, Memc[errmsg])
	}

	# Read the string containing keyword, datatype, value.
	call tbhrpr (tp, parnum, Memc[str])

	# If the data type is text, we'll either use ctowrd or take the
	# entire string as the current value, depending on whether it's
	# already enclosed in quotes.
	if (Memc[str+LOCN_DTYPE-1] == 't') {	# type is text string?

	    if (Memc[str+START_OF_VALUE-1] == '"' ||
		Memc[str+START_OF_VALUE-1] == '\'') {

		# It's enclosed in quotes, so use ctowrd to get current value.
		ip = START_OF_VALUE
		nchar = ctowrd (Memc[str], ip, Memc[value], SZ_PARREC)

	    } else {

		# If the value is already so long that we can't even enclose
		# it in quotes, quit now.
		if (strlen (Memc[str]) > SZ_PARREC-2) {
		    call sfree (sp)
		    return
		}

		# Save the value.
		call strcpy (Memc[str+START_OF_VALUE-1], Memc[value], SZ_PARREC)
	   }

	    # Enclose the value in quotes, and copy it and the comment to str.
	    Memc[str+START_OF_VALUE-1] = EOS		# truncate after dtype
	    call strcat ("'", Memc[str], SZ_PARREC)
	    call strcat (Memc[value], Memc[str], SZ_PARREC)
	    call strcat ("' ", Memc[str], SZ_PARREC)
	    call strcat (comment, Memc[str], SZ_PARREC)

	} else {					# numeric datatype

	    # Save the value.
	    ip = START_OF_VALUE
	    nchar = ctowrd (Memc[str], ip, Memc[value], SZ_PARREC)

	    Memc[str+START_OF_VALUE-1] = EOS		# truncate
	    call strcat (Memc[value], Memc[str], SZ_PARREC)
	    call strcat (" ", Memc[str], SZ_PARREC)
	    call strcat (comment, Memc[str], SZ_PARREC)
	}

	# Write the string back into the table.
	call tbhwpr (tp, parnum, Memc[str])

	TB_MODIFIED(tp) = true

	call sfree (sp)
end
