# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<knet.h>

# PRSIGNAL -- Send a signal (interrupt) to a child process.  It is an error
# if the pid given is not found in the process table.

procedure prsignal (pid, signal)

int	pid			# process-id of child process
int	signal			# code of signal to be sent (e.g. X_INT)

int	child
int	pr_findproc()
include	"prc.com"
errchk	syserr

begin
	child = pr_findproc (pid)
	if (child != ERR)
	    call zintpr (pid, signal, child)

	if (child == ERR)
	    call syserr (SYS_PRSIGNAL)
end
