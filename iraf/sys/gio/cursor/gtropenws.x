# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<error.h>
include	<prstat.h>
include	<fset.h>
include	<fio.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_OPENWS -- Called by gtr_control(pr_psio) to connect a kernel to a
# graphics stream and to initialize the datapath to the kernel.
# The workstation is not physically opened until the GKI open workstation
# directive has been sent to the kernel.  There are two types of kernels,
# the builtin (STDGRAPH) kernel, and all external kernels.  The external
# kernels reside in connected subprocesses communicating via the central
# process (the CL process) with the graphics task in another subprocess.

procedure gtr_openws (devspec, mode, stream, source_pid)

char	devspec[ARB]		#I device specification
int	mode			#I access mode
int	stream			#I graphics stream
int	source_pid		#I process which issued the openws directive

int	redir_code, dd[LEN_GKIDD], ip
pointer	sp, op, tr, tty, kernfname, taskname, device

bool	streq()
pointer	ttygdes()
int	pr_getredir(), ttygets(), gtr_connect(), pr_findproc(), locpr()
extern	gtr_reset(), prpsio()

errchk	syserr, syserrs, fseti, ttygdes, ttycdes, pr_redir, stg_close, stg_open
errchk	gtr_connect, gtr_disconnect
include	"gtr.com"

begin
	call smark (sp)
	call salloc (kernfname, SZ_FNAME, TY_CHAR)
	call salloc (taskname, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)

	tr = trdes[stream]

	# Extract the device name field from the device specification.
	op = device
	for (ip=1;  devspec[ip] != EOS;  ip=ip+1)
	    if (devspec[ip] == ',')
		break
	    else {
		Memc[op] = devspec[ip]
		op = op + 1
	    }
	Memc[op] = EOS

	# We only connect up the i/o channels, and do not issue the OPENWS
	# to the gio kernel, so reset the counter to zero to indicate that
	# the workstation has not yet been (logically) opened.

	TR_WSOPEN(tr) = 0

	# If the stream has been redirected into a file, do not connect a
	# kernel.

	redir_code = pr_getredir (source_pid, stream)
	if (redir_code >= FIRST_FD && redir_code <= LAST_FD) {
	    call sfree (sp)
	    return
	}

	# The graphics stream is a spoolfile in this process (the CL process).
	# Spoolfiles are files that are fully buffered in memory and never
	# get written to disk.  Data is written into the spoolfile and then
	# read back out by a different part of the program.

	call fseti (stream, F_TYPE, SPOOL_FILE)
	call fseti (stream, F_CANCEL, OK)

	# If the device is already connected to the stream (or we are
	# appending to a connected device) all we need do is reset the
	# redirection code for the graphics stream.  This code is reset to
	# the default value (the code for the stream itself) by the CL when
	# a task is spawned.

	if (TR_DEVNAME(tr) != EOS && mode == APPEND ||
		streq (devspec, TR_DEVNAME(tr))) {
	    call pr_redir (source_pid, stream, TR_REDIR(tr))
	    call sfree (sp)
	    return
	}

	# Connect the named kernel, i.e., disconnect the old kernel if any
	# and connect the new one.  Set the redirection information for the
	# named stream of the source process.

	iferr {
	    # Close device graphcap descriptor.
	    if (TR_TTY(tr) != NULL)
		call ttycdes (TR_TTY(tr))

	    # Disconnect old kernel.
	    if (streq (TR_KERNFNAME(tr), "cl"))
		call stg_close()
	    else if (TR_DEVNAME(tr) != EOS && TR_KERNFNAME(tr) != EOS) {
		call gtr_disconnect (TR_PID(tr), TR_IN(tr), TR_OUT(tr), stream)
		TR_PID(tr) = NULL
		TR_IN(tr) = NULL
		TR_OUT(tr) = NULL
	    }
	} then {
	    TR_DEVNAME(tr) = EOS
	    call erract (EA_ERROR)
	} else
	    TR_DEVNAME(tr) = EOS

	# Get graphcap entry for the new device.  The special device name
	# "none" indicates that there is no suitable stdgraph device.

	if (streq (devspec, "none")) {
	    switch (stream) {
	    case STDGRAPH:
		call syserr (SYS_GGNONE)
	    case STDIMAGE:
		call syserr (SYS_GINONE)
	    case STDPLOT:
		call syserr (SYS_GPNONE)
	    default:
		call syserr (SYS_GGNONE)
	    }
	} else {
	    tty = ttygdes (Memc[device])
	    TR_TTY(tr) = tty
	}

	# Get the name of the executable file containing the kernel for the
	# device.  The special name "cl" signifies the builtin STDGRAPH kernel.

	if (ttygets (tty, "kf", Memc[kernfname], SZ_FNAME) <= 0) {
	    call ttycdes (tty)
	    call syserrs (SYS_GNOKF, Memc[device])
	} else if (ttygets (tty, "tn", Memc[taskname], SZ_FNAME) <= 0)
	    ;

	# Connect the new kernel.
	call strcpy (Memc[kernfname], TR_KERNFNAME(tr), SZ_KERNFNAME)

	if (streq (Memc[kernfname], "cl")) {
	    # Open the stdgraph kernel.  Connect the referenced GKI stream to
	    # the stdgraph kernel.  Set a negative redirection code value to
	    # flag that GIOTR is to be called to filter graphics output from
	    # the process.

	    call stg_open (devspec, dd, STDIN, STDOUT, 0, 0, 0)
	    call gki_inline_kernel (stream, dd)
	    if (source_pid != NULL)
		call pr_redir (source_pid, stream, -stream)
	    TR_REDIR(tr) = -stream
	    TR_INTERACTIVE(tr) = YES

	} else {
	    # Spawn subprocess and start up kernel task.
	    TR_PID(tr) = gtr_connect (Memc[kernfname], Memc[taskname],
		devspec, stream, TR_IN(tr), TR_OUT(tr))

	    # Encode the process slot number of the kernel process in the
	    # redirection code for the source process (the process which
	    # issued the openws).  If the stream is STDGRAPH or STDIMAGE
	    # make the redirection code negative to flag that graphics
	    # output is to be processed through GIOTR (the workstation
	    # transformation).

	    if (source_pid != NULL) {
		redir_code = (pr_findproc(TR_PID(tr)) * KSHIFT) + stream
		if (stream == STDGRAPH || stream == STDIMAGE)
		    redir_code = -redir_code
		call pr_redir (source_pid, stream, redir_code)
		TR_REDIR(tr) = redir_code

		# Mark the process busy.  This flags it it as busy executing
		# some subprotocol (in this case processing GKI metacode) and
		# prevents commands such as chdir/set from being sent to the
		# process and corrupting the IPC protocol.

		call prseti (TR_PID(tr), PR_STATUS, P_BUSY)
	    }

	    call gki_subkernel (stream, TR_PID(tr), locpr(prpsio))
	    TR_INTERACTIVE(tr) = NO
	}

	# Do not change value of DEVNAME until the new kernel has been
	# successfully connected, since this variable is used to test if
	# the kernel is already connected.

	call strcpy (devspec, TR_DEVNAME(tr), SZ_TRDEVNAME)

	# Post the gtr_reset procedure to be executed upon process shutdown,
	# to close down any connected graphics subkernels in an orderly way.

	call onexit (gtr_reset)

	call sfree (sp)
end
