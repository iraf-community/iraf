# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>
include	<config.h>
include	<fset.h>
include	<fio.h>
include	<ctype.h>
include	<clio.h>

define	SZ_NUMSTR	8

.help zfiocl
.nf __________________________________________________________________________
ZFIOCL -- FIO z-routines (machine independent) for pseudofile i/o.  The STDIN,
STDOUT, STDERR, and STDGRAPH streams are implemented as "pseudofiles" when a
process is run as a connected subprocess communicating with the parent process
via the IPC facilities.  In such a configuration the standard i/o streams
(opened by the system [clopen] at process startup) are as follows:

	fd	name		description

	1	CLIN		IPC input from the parent	[low level]
	2	CLOUT		IPC output to the parent	[low level]
	3	STDIN		standard input
	4	STDOUT		standard output
	5	STDERR		standard error output
	6	STDGRAPH	standard graphics output
	7	STDIMAGE	standard greyscal output
	8	STDPLOT		stdgraph plotter output
	9	PSIOCTRL	pseudofile i/o control

The CLIN and CLOUT streams are the FIO equivalents of the IPC channels, i.e.,
they are connected to physical host i/o channels.  The standard i/o streams
are multiplexed over the IPC channels by a packet passing protocol to the
parent process, which redirects each stream to an FIO file (which may in turn
be a pseudofile or a regular file).

During execution of the child the parent has its command input switched to
the CLOUT stream of the child, and the child commands the parent.  Commands
are sent over the IPC channels as an SPP text stream, i.e., unpacked ASCII
lines delimited by the newline character.   Pseudofile data is sent as a
binary block preceded by a command to the parent to read so many chars from
the channel and pass it on to the indicated actual file.  Typical output to
CLOUT might be as follows:

	param1 =
	param2 =
	xmit(4,34)
	[34 chars of data]

This example consists of three independent commands to the parent.  The first
two are parameter requests and each is followed by a read from CLIN to get
the parameter value (which is returned in ASCII and is limited to a single
line).  Syncronization occurs on the read.  The binary data block in the
third command (XMIT) may contain arbitrary binary data, i.e., pseudofile
i/o is not limited to text.  In the example shown the 34 chars of data will
be copied to the file associated by the parent with the STDOUT stream of
the child.  The association of pseudofile codes at the IPC level with FIO
file descriptor codes is simple: the pseudofile codes are the same as the
fd codes.

Although it is not relevant to a discussion of the pseudofile drivers, perhaps
we should mention what happens to these streams when a process is run stand
alone, i.e., no IPC channels.  When a process is run stand-alone (prtype ==
PR_HOST) CLIN, CLOUT, and the pseudofiles are connected to standard i/o
channels of the process as follows:

	CLIN,STDIN		-> process_stdin
	CLOUT,STDOUT		-> process_stdout
	STDERR			-> process_stderr
	STDGRAPH,etc.		-> null file

The standard i/o streams are normally limited to text data in stand alone
mode, since the process channels are normally connected to a terminal.  Any
standard i/o stream may be redirected on the IRAF Main command line to
either a text or binary file, regardless of the process type (connected,
detached, or host).

This file contains the FIO device drivers for each pseudofile.  The device
codes are SI, SO, SE, and SG.  ZARDBF, ZAWRBF, ZAWTBF, and ZSTTBF entry
points are supplied for each device.
.endhelp _____________________________________________________________________


# ZCLSPS -- Dummy close procedure for all pseudofile streams.

procedure zclsps (chan, status)

int	chan
int	status

begin
	status = ERR
end


# ZARDPS -- "Asynchronous" read primitive for a pseudofile.  The read is
# initiated by sending the following command to the parent:
# 
# 	xfer(ps,maxchars)\n
# 
# where "ps" is the pseudofile code (3=STDIN, etc.) and "maxchars" is the
# maximum number of chars to be returned.  The parent responds with the actual
# number of chars to be sent, followed by newline, followed by the block of
# data, i.e.:
# 
#		CLOUT			          CLIN
#	    xfer(3,512)\n
#					    40\n
#					    [40 chars of data]
# 
# The parent responds by writing to the child's CLIN.
#
# NOTE1 -- Since this is a device driver (effectively a kernel procedure despite
# the machine independence) only low level procedures may be used, else a
# recursive call may result.
# NOTE2 -- There are some subtleties inherent in all this which are not obvious
# at first glance.  Since CLIN and STDIN both read from the same IPC, some care
# is required to ensure that one stream does not steal messages intended for
# the other.  Fortunately this is not our concern here, but rather that of the
# high level code.

procedure zardps (ps, buf, maxbytes, offset)

int	ps				# pseudofile
char	buf[ARB]			# buffer to receive data
int	maxbytes, maxchars		# capacity of buffer
long	offset				# ignored at present

char	numstr[SZ_NUMSTR]
int	nbytes, nchars, ndigits, ip, clin_chan, raw_mode
int	ctoi(), cl_psio_request(), fstati()
include	"clio.com"
define	ioerr_ 91

begin
	if (ps == STDOUT || ps == STDERR)
	    goto ioerr_
	clin_chan = fstati (CLIN, F_CHANNEL)
	raw_mode  = fstati (ps, F_RAW)

	# Send the XFER command to the parent.  If raw mode is in effect on
	# the pseudofile, request only a single char.

	if (raw_mode == YES)
	    maxchars = 1
	else
	    maxchars = maxbytes / SZB_CHAR

	if (cl_psio_request ("xfer", ps, maxchars) == ERR)
	    goto ioerr_

	# Get the number of chars to be read.
	call zardpr (clin_chan, numstr, SZ_NUMSTR * SZB_CHAR, offset)
	call zawtpr (clin_chan, nbytes)

	if (nbytes < 0)
	    goto ioerr_
	else
	    ndigits = nbytes / SZB_CHAR

	# Decode count of chars in data block, a simple positive integer
	# constant followed by a newline.  The case of a single digit
	# (nchars < 10) is optimized.

	if (ndigits == 0)
	    nchars = 0
	else if (ndigits == 1)
	    nchars = TO_INTEG (numstr[1])
	else {
	    numstr[ndigits+1] = EOS
	    ip = 1
	    if (ctoi (numstr, ip, nchars) <= 0)
		goto ioerr_
	}

	# Read the data.
	nbytes = nchars * SZB_CHAR
	if (nchars == 0)
	    ps_status[ps] = 0				# EOF
	else {
	    call zardpr (clin_chan, buf, nbytes, offset)
	    call zawtpr (clin_chan, ps_status[ps])
	}
	return
ioerr_
	ps_status[ps] = ERR
	return
end


# ZAWRPS -- Write primitive for a pseudofile.  The write is initiated by
# sending the following command to the CL:
# 
# 	xmit(ps,nchars)\n
# 
# where "ps" is the pseudofile number, and "nchars" is the number of chars
# of binary data to be read from CLOUT and copied to the file connected to
# pseudofile "ps" by the parent.

procedure zawrps (ps, buf, nbytes, offset)

int	ps				# pseudofile
char	buf[ARB]			# buffer to receive data
int	nbytes				# capacity of buffer
long	offset				# ignored at present

int	nchars, clout_chan
int	cl_psio_request(), fstati()
include	"clio.com"
define	ioerr_ 91

begin
	if (ps == STDIN)
	    goto ioerr_
	clout_chan = fstati (CLOUT, F_CHANNEL)

	# Send the XMIT command to the parent.
	nchars = nbytes / SZB_CHAR
	if (cl_psio_request ("xmit", ps, nchars) == ERR)
	    goto ioerr_

	# Send the data block.
	call zawrpr (clout_chan, buf, nbytes, offset)
	call zawtpr (clout_chan, ps_status[ps])
	return
ioerr_
	ps_status[ps] = ERR
	return
end


# ZAWTPS -- Wait for i/o to a pseudofile (required by the FIO interface,
# though pseudofile i/o is not really asynchronous).

procedure zawtps (ps, status)

int	ps			# pseudofile code
int	status			# nbytes transferred in last packed (output)
include	"clio.com"

begin
	status = ps_status[ps]
end


# ZSTTPS -- Channel status of a pseudofile.  With the exception of the optimal
# buffer size for STDERR we default to the IPC status parameters, since i/o
# is ultimately over IPC channels to the parent process.

procedure zsttps (ps, what, lvalue)

int	ps			# pseudofile
int	what			# status parameter requested
long	lvalue			# output value (long)

int	fstati()

begin
	if (ps == STDERR && what == FSTT_OPTBUFSIZE)
	    lvalue = SZ_LINE * SZB_CHAR
	else
	    call zsttpr (fstati(CLIN,F_CHANNEL), what, lvalue)
end


# CL_PSIO_REQUEST -- Output "cmd(arg1,arg2)\n" to CLOUT.  Called by CL_ZARDPS
# and CL_ZAWRPS to send the XMIT and XFER commands to the CL, when writing to
# or reading from a pseudofile.

int procedure cl_psio_request (cmd, arg1, arg2)

char	cmd[ARB]		# e.g. "xmit" or "xfer"
int	arg1, arg2		# integer arguments

int	ip, status, clout_chan
pointer	obuf, sp, op
long	offset
int	itoc(), fstati()
define	output {Memc[op]=$1;op=op+1}

begin
	call smark (sp)
	call salloc (obuf, SZ_PATHNAME, TY_CHAR)

	clout_chan = fstati (CLOUT, F_CHANNEL)

	op = obuf
	for (ip=1;  cmd[ip] != EOS;  ip=ip+1)
	    output (cmd[ip])

	# Encode argument list.  Arguments are assumed to always be
	# nonnegative.  Optimized for simple single digit numbers.

	output ('(')
	if (arg1 < 10)
	    output (TO_DIGIT (arg1))
	else
	    op = op + itoc (arg1, Memc[op], SZ_PATHNAME-(op-obuf))

	output (',')

	if (arg2 < 10)
	    output (TO_DIGIT (arg2))
	else
	    op = op + itoc (arg2, Memc[op], SZ_PATHNAME-(op-obuf))

	output (')')
	output ('\n')

	call zawrpr (clout_chan, Memc[obuf], (op-obuf) * SZB_CHAR, offset)
	call zawtpr (clout_chan, status)

	call sfree (sp)
	return (status)
end
