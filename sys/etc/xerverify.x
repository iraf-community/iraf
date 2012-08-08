# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# XER_VERIFY -- The following procedure is called by the iraf main after
# a task completes, to verify that NHANDLERS is zero, indicating that an XERPOP
# was executed for each XERPSH.  Note that a transfer out of an IFERR block
# (a programming error) could prevent XERPOP from being called.

procedure xer_verify()

include	"error.com"

begin
	if (xerflg)
	    call erract (EA_FATAL)
	if (nhandlers != 0) {
	    nhandlers = 0
	    call putline (STDERR, "Warning: Transfer out of IFERR block\n")
	}
end
