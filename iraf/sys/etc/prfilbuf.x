# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# PRFILBUF -- Fill the FIO buffer from a process.  The function is equivalent
# to the ordinary FIO filbuf with the exception that pseudofile read and
# write directives are intercepted and processed.  Hence, the reader sees a
# stream of application specific commands need not know about pseudofile i/o.

int procedure prfilbuf (fd)

int	fd			# parent's input IPC from child process

int	pr
int	filbuf(), prpsio()
include	"prc.com"

begin
	# Determine which process has the given file as its CLIN stream.
	# If FD not associated with a process call ordinary FILBUF, otherwise
	# call PR_PSIO.  To minimize searches of the process table we keep
	# track of the slot number of the last active pid.

	if (pr_infd[pr_lastio] == fd && pr_pid[pr_lastio] != NULL)
	    pr = pr_lastio
	else {
	    for (pr=1;  pr <= MAX_CHILDPROCS;  pr=pr+1)
		if (pr_pid[pr] != NULL)
		    if (pr_infd[pr] == fd)
			break
	    if (pr > MAX_CHILDPROCS)
		return (filbuf (fd))			# normal file
	    pr_lastio = pr
	}

	return (prpsio (pr_pid[pr], CLIN, FF_READ))
end
