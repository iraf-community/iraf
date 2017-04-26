include <tbset.h>
include "tbtables.h"

# tbhgcm -- get a comment from a header parameter
# This reads the comment from a header parameter.  It is an error if
# the keyword is not found, but there need not be an associated comment.
# Trailing blanks are removed from the comment.
#
# Phil Hodge,  6-Mar-1995  Subroutine created.
# Phil Hodge,  8-Jun-1995  Modify for FITS tables.
# Phil Hodge,  7-Jun-1999  Handle text tables.

procedure tbhgcm (tp, keyword, comment, maxch)

pointer tp			# i: pointer to table descriptor
char	keyword[SZ_KEYWORD]	# i: keyword to be found
char	comment[ARB]		# o: comment string for keyword
int	maxch			# i: max size of comment
#--
pointer sp
pointer errmsg			# scratch for possible error message
pointer str			# scratch for string read from header
int	parnum			# number of the parameter
int	index			# location of comment within string
int	i
int	strlen()
bool	tbhisc()

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfgcm (tp, keyword, comment, maxch)
	    return
	}

	# We don't read a comment from a comment.
	if (tbhisc (keyword)) {
	    comment[1] = EOS
	    return
	}

	call smark (sp)
	call salloc (str, SZ_PARREC, TY_CHAR)

	# Find the keyword in the header.
	call tbhfkw (tp, keyword, parnum)
	if (parnum < 1) {
	    call salloc (errmsg, SZ_FNAME, TY_CHAR)
	    call sprintf (Memc[errmsg], SZ_FNAME,
			"tbhgcm:  keyword `%s' not found in table `%s'")
		call pargstr (keyword)
		call pargstr (TB_NAME(tp))
	    call error (1, Memc[errmsg])
	}

	# Read the string containing keyword, datatype, value.
	call tbhrpr (tp, parnum, Memc[str])

	# Search for a comment.
	call tbhfcm (Memc[str], index)

	if (index > 0)
	    call strcpy (Memc[str+index-1], comment, maxch)
	else
	    comment[1] = EOS				# no comment

	# Trim trailing blanks.
	do i = strlen (comment), 1, -1 {
	    if (comment[i] == ' ')
		comment[i] = EOS
	    else
		break
	}

	call sfree (sp)
end
