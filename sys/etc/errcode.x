# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# ERRCODE -- Return the integer code of the last error posted.  The error
# code is set to a positive nonnegative integer by a call to ERROR or
# FATAL, and is cleared (set to OK) whenever an IFERR block is entered.
# Note that if we are called from within an error handler (true part of
# an IFERR block), xerflg is false, so we cannot test xerflg to see if
# an error occurred.

int procedure errcode()

include	"error.com"

begin
	return (xercod)
end
