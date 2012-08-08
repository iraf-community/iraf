# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# ERRGET -- Return the integer code and descriptive error message string
# of the last error posted.  The error code is set to a positive nonnegative
# integer by a call to ERROR or FATAL, and is cleared (set to OK) whenever
# an IFERR block is entered.  Note that if we are called from within an error
# handler (true part of an IFERR block), xerflg is false, so we cannot test
# xerflg to see if an error occurred.

int procedure errget (outstr, maxch)

char	outstr[maxch]		# error message
int	maxch
include	"error.com"

begin
	call xer_fmterrmsg (xermsg, outstr, maxch)
	return (xercod)
end
