include "tbtables.h"

# tbfpcm -- add a comment to a FITS header parameter
# This adds a comment to a header parameter, or replaces one that is
# already there.  It is an error if the header parameter is not found.
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfpcm (tp, keyword, comment)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: keyword to be found
char	comment[ARB]	# i: comment string for keyword
#--
int	status		# 0 is OK
errchk	tbferr

begin
	if (comment[1] == EOS)
	    return

	status = 0

	call fsmcom (TB_FILE(tp), keyword, comment, status)
	if (status != 0)
	    call tbferr (status)
end
