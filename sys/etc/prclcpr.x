# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>

# PRCLCPR -- Close a connected subprocess.  Given the PID of the child process
# we get the input and output file descriptors from the process table and
# close both files.  When the second file is closed the process is disconnected
# by the PR_ZCLSPR procedure, which leaves the exit status in the process table
# common.

int procedure prclcpr (pid)

int	pid			# process id of child process

int	child
int	pr_findproc()
include	"prc.com"
errchk	syserr

begin
	# Search process table for the PID of the child process and close it
	# if found.  Return process termination code to parent.

	child = pr_findproc (pid)
	if (child == ERR)
	    call syserr (SYS_PRNOTFOUND)

	call close (pr_infd[child])
	call close (pr_outfd[child])

	return (pr_last_exit_code)
end
