# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<ttset.h>
include	<fset.h>
include	<knet.h>

# CLOPEN -- "Open" the CL files (initialize CLIO).  Called by the IRAF Main
# upon process startup.  The CL device may be either the CL process,
# communicating with the current process via intertask communications
# (via ZARDCL, ZAWRCL), or a text file or terminal.  "Open" the files CLIN
# and CLOUT, and the standard pseudofiles STDIN, STDOUT, STDERR, STDGRAPH,
# STDIMAGE, STDPLOT, and PSIOCTRL.

procedure clopen (stdin_chan, stdout_chan, stderr_chan, device, devtype)

int	stdin_chan		# OS channel for the process standard input
int	stdout_chan		# OS channel for the process standard output
int	stderr_chan		# OS channel for the standard error output
int	device			# zlocpr EPA of the driver read routine
int	devtype			# device type (text or binary)

int	fd, psmode, chan, devepa
int	fsetfd(), locpr()
extern	zardps(), zardnu(), zgetty(), zgettt()

begin
	if (devtype == BINARY_FILE)
	    psmode = WRITE_ONLY
	else
	    psmode = APPEND

	# Allocate and initialize the standard (predefined) file descriptors.
	# FSETFD performs only the standard initialization.  The remainder
	# of the code initializes the device dependent parameters.

	fd = fsetfd (CLIN,     "CLIN",     READ_ONLY,  devtype)
	fd = fsetfd (CLOUT,    "CLOUT",    psmode,     devtype)
	fd = fsetfd (STDIN,    "STDIN",    READ_ONLY,  devtype)
	fd = fsetfd (STDOUT,   "STDOUT",   psmode,     devtype)
	fd = fsetfd (STDERR,   "STDERR",   psmode,     devtype)
	fd = fsetfd (STDGRAPH, "STDGRAPH", READ_WRITE, BINARY_FILE)
	fd = fsetfd (STDIMAGE, "STDIMAGE", READ_WRITE, BINARY_FILE)
	fd = fsetfd (STDPLOT,  "STDPLOT",  READ_WRITE, BINARY_FILE)
	fd = fsetfd (PSIOCTRL, "PSIOCTRL", READ_WRITE, BINARY_FILE)

	# Set the entry point addresses of the device Z-routines for each
	# of the special files.  If the process channels are text files
	# (character files or a terminal) the pseudofiles are connected to
	# real files (no multiplexing).  Graphics i/o is connected to the
	# null file if the process channels are textual, hence graphics
	# output is discarded (unless redirected) when a task is run stand
	# alone.  If the device we are passed is the kernel terminal driver
	# TY, connect the VOS logical terminal driver TT instead.

	if (device == locpr (zgetty)) {
	    devepa = locpr (zgettt)
	    call zsettt (stdin_chan,  TT_KINCHAN, stdin_chan)
	    call zsettt (stdout_chan, TT_KOUTCHAN, stdout_chan)
	} else
	    devepa = device

	call fseti (CLIN,  F_DEVICE, devepa)
	call fseti (CLOUT, F_DEVICE, devepa)

	if (devtype == TEXT_FILE) {
	    # Set device drivers for the textual pseudofiles.
	    do fd = STDIN, STDERR
		call fseti (fd, F_DEVICE, devepa)
	    
	    # Connect the graphics streams to the null file.
	    do fd = STDGRAPH, PSIOCTRL
		call fseti (fd, F_DEVICE, locpr(zardnu))

	} else {
	    # Connect the pseudofiles to the pseudofile driver.
	    do fd = STDIN, PSIOCTRL
		call fseti (fd, F_DEVICE, locpr(zardps))
	}

	# Associate a device channel with the two IPC streams and with each
	# pseudofile.

	call fseti (CLIN,  F_CHANNEL, stdin_chan)
	call fseti (CLOUT, F_CHANNEL, stdout_chan)

	if (devtype == TEXT_FILE) {
	    call fseti (STDIN,  F_CHANNEL, stdin_chan)
	    call fseti (STDOUT, F_CHANNEL, stdout_chan)
	    call fseti (STDERR, F_CHANNEL, stdout_chan)

	    # Open a null file on each graphics stream.
	    do fd = STDGRAPH, PSIOCTRL {
		call zopnnu ("", READ_WRITE, chan)
		call fseti (fd, F_CHANNEL, chan)
	    }

	} else {
	    # The channel code for a pseudofile is used for the pseudofile code,
	    # since the actual i/o is always on channels CLIN and CLOUT.

	    do fd = STDIN, PSIOCTRL
		call fseti (fd, F_CHANNEL, fd)
	}

	call fseti (STDERR, F_FLUSHNL, YES)		# flush error messages
	# call fseti (CLOUT,  F_FLUSHNL, YES)		# flush CL commands

	# Get device block size, and the minimum optimal buffer size for
	# efficient sequential i/o.

	do fd = CLIN, PSIOCTRL				# device parameters
	    call fgdev_param (fd)

	# Seek is needed to set the proper logical offset for each file,
	# as well as to seek to the end of a text file if no CL.

	call seek (CLIN,  BOFL)
	call seek (CLOUT, EOFL)
	call seek (STDIN, BOFL)

	do fd = STDOUT, PSIOCTRL
	    call seek (fd, EOFL)
end
