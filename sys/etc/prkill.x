# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>

define	NOKILL		NO
define	KILL		YES

# PRKILL -- Kill a detached process.  Control does not return until the process
# has terminated, unless the kill fails for some reason.  If the process exists
# and is terminated by the kill directive, the exit status is left in the
# process table for return by PRCLDPR.

procedure prkill (job)

int	job			# slot number of job in prd.com table
include	"prd.com"
errchk	syserr

begin
	# Kill the process if there is such a process and it is still active.
	# It is an error to try to kill a nonexistent process or a process
	# which has already been killed.

	if (pr_jobcode[job] == NULL)
	    call syserr (SYS_PRBKGNF)
	else if (pr_active[job] == NO)
	    call syserr (SYS_PRBKGNOKILL)
	else {
	    call zcldpr (pr_jobcode[job], KILL, pr_exit_status[job])
	    if (pr_exit_status[job] == ERR)
		call syserr (SYS_PRBKGNOKILL)
	    else
		pr_active[job] = NO
	}

	# Delete the bkgfile if the process has not already done so during
	# shutdown.
	iferr (call delete (Memc[pr_bkgfile[job]]))
	    ;
end
