# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>

define	NOKILL		NO
define	KILL		YES

# PRCLDPR -- Close a detached process.  Called following process termination
# to obtain the exit status and free up any system resources still allocated
# to the job.  If called prior to job termination execution of the current
# process will be suspended until the bkg job terminates.  PRDONE should be
# called to determine if the job is still running if waiting is not desired.

int procedure prcldpr (job)

int	job			# slot number of job in prd.com table
int	exit_status
include	"prd.com"

begin
	# Wait for process to terminate if it is still active.  If we are
	# interrupted the process table is left unmodified.  If the process
	# has been killed the exit status will have been left in the table
	# and ZCLDPR should not be called again.

	if (pr_jobcode[job] == NULL)
	    call syserr (SYS_PRBKGNF)
	else if (pr_active[job] == YES)
	    call zcldpr (pr_jobcode[job], NOKILL, exit_status)
	else
	    exit_status = pr_exit_status[job]

	# Free all remaining resources allocated to job.  The bkgfile should
	# already have been deleted by the process but if not, e.g., in the
	# event of abnormal process termination, we delete it ourselves.  The
	# buffer for the bkgfile filename is freed as is the slot in the
	# process table.

	iferr (call delete (Memc[pr_bkgfile[job]]))
	    ;
	call mfree (pr_bkgfile[job], TY_CHAR)
	pr_jobcode[job] = NULL

	return (exit_status)
end
