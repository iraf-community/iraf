# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# XER_RESET -- Called to initialize error handling.  Used during startup and
# during error recovery (e.g. in an interrupt handler) to reset the state of
# the error handling code.

procedure xer_reset()

include	"error.com"

begin
	xerflg = false
	xercod = OK
	err_restart = NO
	nhandlers = 0
	xermsg[1] = EOS
end
