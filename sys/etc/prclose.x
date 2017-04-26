# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<prstat.h>

# PRCLOSE -- Close a connected subprocess.  Send the command "bye" to the
# child to initiate process shutdown, then close the IPC channels and
# wait for the child to terminate, returning the termination status to
# our caller.  Note that process shutdown may take an arbitrarily long
# time, depending on the number and nature of ONEXIT procedures posted by
# tasks in the the child process.

int procedure prclose (pid)

int	pid			# process-id of child process

int	child
int	pr_findproc(), prclcpr()
include	"prc.com"
errchk	syserr

begin
	child = pr_findproc (pid)
	if (child == ERR)
	    call syserr (SYS_PRNOTFOUND)

	if (pr_status[child] != P_DONE && pr_status[child] != P_DEAD)
	    call putline (pr_outfd[child], "bye\n")

	return (prclcpr (pid))
end
