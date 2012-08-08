# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>
include	<fset.h>
include	<xwhen.h>
include	<prstat.h>

# PROPCPR -- Open a connected subprocess, i.e., spawn the subprocess and
# connect the two IPC channels to FIO file descriptors.  No i/o is done on
# the IPC channels, hence this relatively low level command is independent
# of the IPC protocol used.  PROPEN should be used if the standard IPC
# protocol is followed, so that the environment and current working directory
# may be passed to the subprocess.

int procedure propcpr (process, in, out)

char	process[ARB]		# filename of executable process file
int	in			# fd of IPC input from child (output)
int	out			# fd of IPC output to child (output)

bool	first_time
int	pr, loc_onipc
pointer	sp, fname
int	fopnbf()
extern	pr_onipc(), pr_dummy_open(), pr_zclspr()
extern	zardpr(), zawrpr(), zawtpr(), zsttpr()
errchk	xwhen, fmapfn, syserr
include	"prc.com"

define	cleanup1_ 91
define	cleanup2_ 92
define	cleanup3_ 93
data	first_time /true/

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Initialize the process table.  Post exception handler to recover
	# from write to IPC with no reader.

	if (first_time) {
	    do pr = 1, MAX_CHILDPROCS
		pr_pid[pr] = NULL
	    pr_lastio = 1		# any legal slot number will do
	    pr_last_exit_code = OK
	    call zlocpr (pr_onipc, loc_onipc)
	    call xwhen (X_IPC, loc_onipc, pr_oldipc)
	    first_time = false
	}

	# Find empty process slot.
	for (pr=1;  pr <= MAX_CHILDPROCS;  pr=pr+1)
	    if (pr_pid[pr] == NULL)
		break
	if (pr > MAX_CHILDPROCS)
	    call syserr (SYS_PROVFL)
	pr_index = pr

	# Initialize the mapping of pseudofile codes to file descriptor numbers.
	# PS codes begin at 1, corresponding to STDIN.

	pr_pstofd[pr,STDIN]    = STDIN
	pr_pstofd[pr,STDOUT]   = STDOUT
	pr_pstofd[pr,STDERR]   = STDERR
	pr_pstofd[pr,STDGRAPH] = STDGRAPH
	pr_pstofd[pr,STDIMAGE] = STDIMAGE
	pr_pstofd[pr,STDPLOT]  = STDPLOT

	# Spawn process and open IPC channels.
	call fmapfn (process, Memc[fname], SZ_PATHNAME)
	call zopcpr (Memc[fname], pr_inchan[pr], pr_outchan[pr], pr_pid[pr])
	if (pr_pid[pr] == ERR)
	    goto cleanup3_
	pr_nopen[pr] = 2

	# Set up file descriptors for the two IPC channels.

	call strcpy (process, Memc[fname], SZ_PATHNAME)
	call strcat (".in", Memc[fname], SZ_PATHNAME)
	iferr (in = fopnbf (Memc[fname], READ_ONLY,
	    pr_dummy_open, zardpr, zawrpr, zawtpr, zsttpr, pr_zclspr))
	    goto cleanup2_
	pr_infd[pr] = in

	call strcpy (process, Memc[fname], SZ_PATHNAME)
	call strcat (".out", Memc[fname], SZ_PATHNAME)
	iferr (out = fopnbf (Memc[fname], WRITE_ONLY,
	    pr_dummy_open, zardpr, zawrpr, zawtpr, zsttpr, pr_zclspr))
	    goto cleanup1_
	pr_outfd[pr] = out

	pr_status[pr] = P_RUNNING
	call sfree (sp)
	return (pr_pid[pr])

cleanup1_
	iferr (call close (out))
	    ;
cleanup2_
	iferr (call close (in))
	    ;
cleanup3_
	call sfree (sp)
	pr_pid[pr] = NULL
	call syserrs (SYS_PROPEN, process)
end


# PR_DUMMY_OPEN -- Dummy ZOPNPR procedure called by FIO to "open" the IPC
# channel to a subprocess.  Our only function is to return the appropriate
# channel code to FIO, since the channel has already been opened by ZOPCPR.

procedure pr_dummy_open (osfn, mode, chan)

char	osfn[ARB]		# not used
int	mode			# used to select read or write IPC channel
int	chan			# returned to FIO
include	"prc.com"

begin
	if (mode == READ_ONLY)
	    chan = pr_inchan[pr_index]
	else
	    chan = pr_outchan[pr_index]
end


# PR_ZCLSPR -- Dummy "zclspr" routine called by FIO to close an IPC channel
# when CLOSE is called on the corresponding FIO file descriptor.  A subprocess
# opened with PROPCPR is normally closed with PRCLCPR, but if error recovery
# takes place CLOSE will automatically be called by the system to close both
# file descriptors.  We decrement the count of open channels and close the
# process and both IPC channels when the count reaches zero.  Hence, a
# connected subprocess may be closed either by calling PRCLCPR or by closing
# the IN and OUT file descriptors.

procedure pr_zclspr (chan, status)

int	chan			# either inchan or outchan of process
int	status			# OK or ERR on output
int	pr
include	"prc.com"

begin
	do pr = 1, MAX_CHILDPROCS
	    if (pr_pid[pr] != NULL)
		if (pr_inchan[pr] == chan || pr_outchan[pr] == chan) {
		    pr_nopen[pr] = pr_nopen[pr] - 1
		    if (pr_nopen[pr] == 0) {
			call zclcpr (pr_pid[pr], pr_last_exit_code)
			pr_pid[pr] = NULL
		    }
		    status = OK
		    return
		}

	status = ERR
end


# PR_ONIPC -- Exception handler for the X_IPC (write to IPC with no reader)
# exception.  This exception occurs when the child process dies unexpectedly.
# Determine what process was being written to, cancel any output (to prevent
# a cascade of onipcs trying to flush the output buffer), and cause FIO to
# return a file write error.  We cannot do any more than that w/o reentrancy
# problems.

procedure pr_onipc (vex, next_handler)

int	vex		# virtual exception
int	next_handler	# next handler in chain
int	pr, fd
int	fstati()
include	"prc.com"

begin
	# Chain to next exception handler, if any.
	next_handler = pr_oldipc

	# Get the FD of the file being written to at the time that the
	# exception occurred.  We assume that the write operation is still
	# in progress when the exception takes place.

	fd = fstati (0, F_LASTREFFILE)
	for (pr=1;  pr <= MAX_CHILDPROCS;  pr=pr+1)
	    if (pr_pid[pr] != NULL)
		if (pr_outfd[pr] == fd)
		    break
	if (pr > MAX_CHILDPROCS)
	    return

	# Cancel any buffered output and remove write permission to ensure
	# that we will not get a cascade of X_IPC exceptions.

	call fseti (fd, F_CANCEL, ERR)
	call fseti (fd, F_MODE, READ_ONLY)
	pr_status[pr] = P_DEAD
end
