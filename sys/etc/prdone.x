# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>

# PRDONE -- Determine if a detached process (background job) has completed.
# This is a difficult process to perform portably at the system call level,
# hence the deletion of the bkgfile is used to signal the completion of a
# detached process.  If the detached process fails to delete its bkgfile
# for some reason, PRCLDPR will do so if the process has indeed terminated.

int procedure prdone (job)

int	job			# job number (slot number in job table)
int	access()
include	"prd.com"

begin
	if (pr_jobcode[job] == NULL)
	    call syserr (SYS_PRBKGNF)

	if (access (Memc[pr_bkgfile[job]], 0, 0) == YES)
	    return (NO)
	else
	    return (YES)
end
