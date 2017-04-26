# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>

# PR_FINDPROC -- Search the process table for the given PID of a child process,
# returning the index of the process if found.

int procedure pr_findproc (pid)

int	pid			# process id of child process
int	pr
include	"prc.com"

begin
	for (pr=1;  pr <= MAX_CHILDPROCS;  pr=pr+1)
	    if (pr_pid[pr] == pid)
		return (pr)
	
	return (ERR)
end
