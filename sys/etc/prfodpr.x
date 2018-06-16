# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>

# PRFODPR -- Fork a detached process.  A detached process runs independently of
# and asynchronous with the parent, with no direct communications (i.e., like
# the IPC channels of connected subprocesses).

int procedure prfodpr ()

int	jobcode, pr
errchk	fmapfn, syserrs, malloc
int	zfodpr()
include	"prd.com"

begin
	# First time initialization of the job table.
	if (first_time) {
	    do pr = 1, MAX_BKGJOBS {
		pr_jobcode[pr] = NULL
		pr_bkgfile[pr] = NULL
	    }
	    first_time = false
	}

	# Get job slot.
	for (pr=1;  pr <= MAX_BKGJOBS;  pr=pr+1)
	    if (pr_jobcode[pr] == NULL)
		break
	if (pr > MAX_BKGJOBS)
	    call syserrs (SYS_PRBKGOVFL, "fork")

	# Spawn or enqueue detached process.
	jobcode = zfodpr ()
	if (jobcode == ERR)
	    call syserrs (SYS_PRBKGOPEN, "fork")

        # On child side, return immediately
        if (jobcode == NULL) {
	    call fio_cleanup(ERR) # reset all open files
	    return (NULL)
	}

	# Set up bkg job descriptor.
	pr_jobcode[pr] = jobcode
	pr_active[pr] = YES

	return (pr)
end
