# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# XERPSH -- Push an error handler on the error "stack".  All we really need
# do is keep track of the number of nested handlers.  If an error condition
# already exists when we are called, an error has occurred which was not
# caught, probably because of a missing errchk declaration.

procedure xerpsh()

include	"error.com"

begin
	if (xerflg)					# error not caught
	    call erract (EA_FATAL)
	nhandlers = nhandlers + 1
	xercod = OK
end


# XERPOP -- Pop an error handler, and return the error status flag (true if
# an error occurred).

bool procedure xerpop()

bool	error_status
include	"error.com"

begin
	nhandlers = nhandlers - 1
	error_status = xerflg
	xerflg = false

	return (error_status)
end


# XERPOPI -- Integer version of XERPOP.

int procedure xerpopi()

bool	error_status
include	"error.com"

begin
	nhandlers = nhandlers - 1
	error_status = xerflg
	xerflg = false

	if (error_status)
	    return (1)
	else
	    return (0)
end
