include <tbset.h>
include "tbtables.h"

# tbfgcm -- get a comment for a FITS header parameter
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfgcm (tp, keyword, comment, maxch)

pointer tp			# i: pointer to table descriptor
char	keyword[SZ_KEYWORD]	# i: keyword to be found
char	comment[ARB]		# o: comment string for keyword
int	maxch			# i: max size of comment
#--
pointer sp
pointer value			# scratch for value (ignored)
pointer cmt			# scratch for comment; also for error message
int	status			# zero is OK
bool	streq()
errchk	tbferr

begin
	call smark (sp)
	call salloc (value, SZ_LINE, TY_CHAR)
	call salloc (cmt, SZ_LINE, TY_CHAR)

	# Check for history or comment.
	call strcpy (keyword, Memc[value], SZ_LINE)	# temp
	call strupr (Memc[value])
	if (streq (Memc[value], "HISTORY") ||
	    streq (Memc[value], "COMMENT") ||
	    streq (Memc[value], "       ") || keyword[1] == EOS) {

	    comment[1] = EOS
	    call sfree (sp)
	    return
	}

	status = 0

	# Get the value and comment.
	call fsgkey (TB_FILE(tp), keyword, Memc[value], Memc[cmt], status)

	if (status != 0)
	    call tbferr (status)

	call strcpy (Memc[cmt], comment, maxch)

	call sfree (sp)
end
