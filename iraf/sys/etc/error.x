# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# ERROR -- Take an error action.  A call to ERROR does not necessarily
# terminate task execution, i.e., if an IFERR error handler is posted it
# will receive control after the procedure call stack is unwound back
# to the procedure containing the error handler.

procedure error (error_code, message)

int	error_code		# positive error code identifying error
char	message[ARB]		# error message describing error
include	"error.com"

begin
	if (xerflg) {					# error already posted?
	    if (max(error_code,1) == xercod)
		return					# same error again
	    else
		call erract (EA_FATAL)			# too many errors
	}

	call xeract (error_code, message, EA_ERROR)
end


# FATAL -- Called when a fatal (irrecoverable) error occurs.  Fatal errors
# cannot be caught by IFERR handlers.  The calling task is terminated and
# error recovery is initiated in the IRAF Main.

procedure fatal (error_code, message)

int	error_code		# positive error code identifying error
char	message[ARB]		# error message describing error

begin
	call xeract (error_code, message, EA_FATAL)
end


# XERACT -- Take an error action; called by ERROR or FATAL.

procedure xeract (error_code, message, severity)

int	error_code		# positive error code identifying error
char	message[ARB]		# error message describing error
int	severity		# severity of the error
include	"error.com"

begin
	xerflg = true					# post error
	xercod = max (1, error_code)
	call strcpy (message, xermsg, SZ_XERMSG)

	if (nhandlers > 0 && severity == EA_ERROR)	# is a handler posted?
	    return
	else
	    call erract (severity)			# take error action
end
