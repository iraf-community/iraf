# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<error.h>

.help erract
.nf _________________________________________________________________________
ERRACT -- Take error action.  Called by FATAL, and by ERROR if a handler
is not posted.  May be called by a user error handler to pass an error
back up to the handler at the next level, or to change the severity of
an error.  Warning messages are posted to the standard error output, 
whereas fatal errors result in error recovery followed by transmission of
the ERROR statement to the CL.

Error restart consists of the following steps:

    (1) The IRAF main is restarted with the error code as argument.
    (2) The main goes through error recovery.  Error recovery consists
	of cleaning up the files system, i.e., closing open files and
	deleting NEW_FILES and TEMP_FILES, clearing the stack, and calling
	any procedures posted with ONERROR.  
    (3) The ERROR statement is sent to the CL.  An example of the
	error statment is "ERROR (501, "Access Violation")".
    (4) The main either waits for the next command, or if run from the CL
	and the error code is SYS_XINT (a CL kill in response to a keyboard
	interrupt), the main returns, shutting the process down.  Procedures
	posted with ONEXIT are called when the process shuts down.

Any errors occuring during error restart or while executing the ONEXIT
procedures are fatal and result in immediate process termination, usually
with a panic error message.  This is necessary to prevent infinite error
recursion.  Also, if we are killed by the CL we should die and not hang up
trying to send error messages to the CL.
.endhelp ____________________________________________________________________

procedure erract (severity)

int	severity
int	op, jumpbuf[LEN_JUMPBUF]
char	wmsg[SZ_LINE]
int	gstrcpy()
include	"error.com"
common	/JUMPCOM/ jumpbuf

begin
	# Clear error restart condition.  Called by the IRAF Main
	# after successful completion of error recovery.

	if (severity == EA_RESTART) {
	    err_restart = err_restart + 1
	    xerflg = false
	    return
	} else if (severity == OK) {
	    err_restart = 0
	    xerflg = false
	    return
	}

	# Any uncaught errors occuring during error restart are fatal and
	# will result in process termination.  This is necessary to prevent
	# recursion and to ensure that a process killed by the CL dies if
	# it cannot complete cleanup and shutdown without errors.  If error
	# recursion occurs we will be called repeatedly, causing the counter
	# to be incremented until a panic abort occurs.

	if (severity != EA_WARN && err_restart > 2) {
	    call xer_fmterrmsg (xermsg, xermsg, SZ_XERMSG)
	    call sys_panic (xercod, xermsg)
	}

	# If a handler is posted, set flag and return, deferring error
	# recovery to the user handler.  If warning message, merely put
	# message to stderr.  Otherwise initiate error recovery by restarting
	# the IRAF main.  This sounds reentrant, but it is not since it is an
	# error restart using ZDOJMP.  The ERROR statement is not sent to
	# the CL until error recovery has completed.

	if (severity == EA_ERROR && nhandlers > 0)
	    xerflg = true
	else {
	    call xer_fmterrmsg (xermsg, xermsg, SZ_XERMSG)
	    if (severity == EA_WARN) {
		op =      gstrcpy ("Warning: ", wmsg, SZ_LINE) + 1
		op = op + gstrcpy (xermsg, wmsg[op], SZ_LINE - op + 1)
		wmsg[op]   = '\n'
		wmsg[op+1] = EOS
		call xer_putline (STDERR, wmsg)
	    } else {
		err_restart = err_restart + 1
		call zdojmp (jumpbuf, xercod)		# Restart IRAF main.
	    }
	}
end
