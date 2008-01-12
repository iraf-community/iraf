# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<prstat.h>

# PRSTATI -- Get the value of a parameter for a connected subprocess.

int procedure prstati (pid, param)

int	pid			# process id of connected subprocess
int	param			# parameter for which status is desired
int	pr
int	pr_findproc()
include	"prc.com"
errchk	syserr

begin
	pr = pr_findproc (pid)
	if (pr == ERR)
	    call syserr (SYS_PRNOTFOUND)

	switch (param) {
	case PR_STATUS:
	    return (pr_status[pr])
	case PR_INCHAN:
	    return (pr_inchan[pr])
	case PR_INFD:
	    return (pr_infd[pr])
	case PR_OUTCHAN:
	    return (pr_outchan[pr])
	case PR_OUTFD:
	    return (pr_outfd[pr])
	case PR_STDIN:
	    return (pr_pstofd[pr,STDIN])
	case PR_STDERR:
	    return (pr_pstofd[pr,STDERR])
	case PR_STDOUT:
	    return (pr_pstofd[pr,STDOUT])
	case PR_STDGRAPH:
	    return (pr_pstofd[pr,STDGRAPH])
	case PR_STDIMAGE:
	    return (pr_pstofd[pr,STDIMAGE])
	case PR_STDPLOT:
	    return (pr_pstofd[pr,STDPLOT])
	default:
	    call syserr (SYS_PRSTAT)
	}
end
