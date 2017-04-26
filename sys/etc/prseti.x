# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<prstat.h>

# PRSETI -- Set the value of a parameter for a connected subprocess.

procedure prseti (pid, param, value)

int	pid			#I process id of connected subprocess
int	param			#I parameter to be set
int	value			#I new parameter value

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
	    pr_status[pr] = value
	case PR_INCHAN:
	    pr_inchan[pr] = value
	case PR_INFD:
	    pr_infd[pr] = value
	case PR_OUTCHAN:
	    pr_outchan[pr] = value
	case PR_OUTFD:
	    pr_outfd[pr] = value
	case PR_STDIN:
	    pr_pstofd[pr,STDIN] = value
	case PR_STDERR:
	    pr_pstofd[pr,STDERR] = value
	case PR_STDOUT:
	    pr_pstofd[pr,STDOUT] = value
	case PR_STDGRAPH:
	    pr_pstofd[pr,STDGRAPH] = value
	case PR_STDIMAGE:
	    pr_pstofd[pr,STDIMAGE] = value
	case PR_STDPLOT:
	    pr_pstofd[pr,STDPLOT] = value
	default:
	    call syserr (SYS_PRSTAT)
	}
end
