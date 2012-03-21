# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<fio.h>
include	<fset.h>
include	<chars.h>
include	<error.h>
include	<config.h>
include	<syserr.h>

define	LEN_STACK	5		# depth one level, four fields
define	LEN_STACKFRAME	5		# size of one pushed stack frame

define	S_PR		stack[$1]
define	S_IN		stack[$1+1]
define	S_DESTFD	stack[$1+2]
define	S_STREAM	stack[$1+3]
define	S_REQUEST	stack[$1+4]

define	push		{stkp=stkp+1;stack[stkp]=($1)}
define	pop		{$1=stack[stkp];stkp=stkp-1}


# PR_PSIO -- Pseudofile i/o for a process.  Process an i/o request for the
# specified pseudofile stream of the specified process.  Called either to read
# command input from the CLIN of a process, or to process a read or write
# request to a pseudofile of a process.  I/O to STDIN, STDOUT, and STDERR
# consists of simple binary copies of records.  Output to the graphics streams
# STDGRAPH, STDIMAGE, or STDPLOT consists of GKI metacode and is optionally
# spooled and filtered with GIOTR (to apply the workstation transformation)
# before be passed to a graphics kernel.  Process to process i/o is tricky
# since we must wait for a process to read or write (send us an XMIT or XFER)
# before transferring a data record.  A process may read or write to streams
# of its own choosing before satisfying our request.  A graphics kernel
# connected as a subprocess reads and writes metacode on a graphics stream
# and is free to read and write STDIN, STDOUT, etc. during execution as if
# the kernel were being run as a CL task.
#
# Note: This code is far more subtle than it appears.  I was unable to express
# the subtleties in comments, eventually concluding that the code was easier
# to understand than the explanation (see Notes.psio for the attempts).  Beware
# that the code is not completely general and assumes certain restrictions on
# process configurations.

int procedure pr_psio (pid, fd, rwflag)

int	pid			# process id number
int	fd			# file for which request is desired
int	rwflag			# type of transfer to wait for

pointer	ip, op
int	stack[LEN_STACK], stkp, nchars, nleft
int	stream, pr, in, record_type, rq, iotype
int	pseudofile, destfd, destpr, destination, ps, flags
bool	filter_gki, graphics_stream, xmit_pending, ioctrl

int	filbuf(), read(), strncmp(), fstati()
int	pr_findproc(), psio_isxmit(), zfunc2(), zfunc3()
errchk	epa_writep, epa_giotr, epa_writetty, epa_readtty, epa_gflush
errchk	pr_findproc, psio_xfer, filbuf, read, write, flush, syserr
include	<fio.com>
include	"prc.com"

begin
	stream = fd
	pr = pr_findproc (pid)
	in = pr_infd[pr]
	stkp = 0

	if (rwflag == FF_WRITE) {
	    # We have been called to write to a subprocess.  Simulate a
	    # pending XMIT request by pushing an XMIT on the stack.  This
	    # causes XFER requests to be processed from the subprocess
	    # until the FIO buffer for destfd is exhausted.

	    push (pr)
	    push (in)
	    push (fd)
	    push (stream)
	    push (XMIT)
	}

# call putline (STDERR, "----------------------------------------------\n")
# call eprintf ("PSIO (pid=%d, pr=%d, stream=%d)\n")
# call pargi(pid); call pargi(pr); call pargi(stream)

	# Process i/o requests from the subprocess until a request is received
	# for the stream FD.  Unsolicited requests from the process do not
	# satisfy the request we were called for, e.g., a process may read from
	# STDIN or write to STDERR before reading from fd=STDGRAPH.

	repeat {
	    repeat {
# call eprintf ("\n.....fill buffer: in=%d, pr=%d, stream=%d, stkp=%d\n")
# call pargi(in); call pargi(pr); call pargi(stream); call pargi(stkp)
		nleft = itop[in] - iop[in]
		if (nleft <= 0)
		    nleft = filbuf (in)

# for(ip=iop[in];ip<itop[in];ip=ip+1)call putc(STDERR, Memc[ip])
		if (nleft == EOF) {
		    stkp = 0
		    nchars = 0
		    break
		} else
		    ip = iop[in]

		# Determine the type of directive, e.g., OS escape, xmit/xfer,
		# or data (anything other than a pseudofile directive).

		if (Memc[ip] == '!')
		    record_type = OSCMD
		else if (Memc[ip] != 'x')
		    record_type = DATA
		else 
		    record_type = psio_isxmit (Memc[ip], pseudofile, nchars)

# call eprintf ("record_type=%d, ps=%d, nchars=%d\n")
# call pargi(record_type); call pargi(pseudofile); call pargi(nchars)

		if (record_type == OSCMD) {
		    itop[in] = ip
		    nchars = nleft

		} else if (record_type == DATA) {
		    pseudofile = CLIN
		    nchars = nleft

		} else if (pseudofile < PSIOCTRL) {
		    # Decode the destination code into the destination FD,
		    # process slot number, and GKI filter flag.

		    destination = pr_pstofd[pr,pseudofile]
# call eprintf ("redir code = %d\n"); call pargi (destination)
		    if (destination <= 0) {
			destination = -destination
			filter_gki = true
		    } else
			filter_gki = false
		    
		    if (destination > KSHIFT) {
			destfd = mod (destination, KSHIFT)
			destpr = destination / KSHIFT
		    } else {
			destfd = destination
			destpr = 0
		    }

		    graphics_stream = (destfd >= STDGRAPH && destfd <= STDPLOT)

		    # Discard the xmit or xfer directive.  We will reuse the
		    # buffer at Memc[ip] later for temporary storage.

		    itop[in] = ip
# call eprintf ("filter=%b, destfd=%d, destpr=%d\n")
# call pargb(filter_gki); call pargi (destfd); call pargi (destpr)

		} else {
		    # Pseudofile control directive.
		    record_type = PSIO
		    itop[in] = ip
		}

		# Process the record (data or PSIO directive).
		switch (record_type) {
		case DATA:
		    # Ordinary data record.  If the requested fd is not CLIN
		    # we have an unsolicited command input error.  If this 
		    # occurs on a graphics stream, reset the stream (close the
		    # kernel and free all storage) to avoid leaving the graphics
		    # system in a funny state.

		    iotype = FF_READ
		    destfd = CLIN

		    if (stream != destfd) {
			# Reset graphics stream.
			if (graphics_stream)
			    call zcall1 (epa_gflush, stream)

			# Take error action.
			Memc[ip+nchars] = EOS
			call putline (STDERR, Memc[ip])
			call syserr (SYS_PRPSIOUCI)
		    }

		case XMIT:
		    # Write to a process pseudofile.
		    iotype = FF_READ

		    # If pseudofile output is not connected to a stream read
		    # and discard the data block.  This situation occurs
		    # whenever a task in interrupted.

		    if (destfd == 0) {
			if (read (in, Memc[ip], nchars) < nchars)
			    call syserr (SYS_PRIPCSYNTAX)
			next
		    }

		    if (filter_gki) {
			# Process a block of GKI metacode.  Append the block
			# to the frame buffer for the stream and call GIOTR
			# to process the metacode.

			op = zfunc2 (epa_writep, destfd, nchars)
			if (read (in, Memc[op], nchars) < nchars)
			    call syserr (SYS_PRIPCSYNTAX)
# call eprintf ("___giotr, %d chars\n")
# call pargi (nchars)

			# Call GIOTR to process the graphics data.  Any data
			# to be returned to the client is spooled in the
			# graphics stream to be read by a subsequent XFER.

			call fseti (destfd, F_CANCEL, OK)
			call zcall1 (epa_giotr, destfd)
			call seek (destfd, BOFL)

		    } else {
			# Binary transfer.

			if (read (in, Memc[ip], nchars) < nchars)
			    call syserr (SYS_PRIPCSYNTAX)

			# If writing to a standard stream and a raw mode i/o
			# control sequence is seen, do a fseti on STDIN in
			# this process to set up raw mode at the FIO level
			# on the stream.  If we don't do this, raw mode will
			# be set in the driver but will be disabled in the
			# first i/o operation by a nchars>1 read request from
			# FIO.

			if (destfd >= STDIN && destfd <= STDERR) {
			    ioctrl = (Memc[ip] == ESC &&
				(nchars==LEN_RAWCMD || nchars==LEN_RAWCMD+1))
			    if (ioctrl) {
				if (strncmp(Memc[ip],RAWOFF,LEN_RAWCMD) == 0) {
				    flags = IO_NORMAL
				} else if (strncmp (Memc[ip],
				    RAWON, LEN_RAWCMD) == 0) {
				    flags = IO_RAW
				    if (Memc[ip+LEN_RAWCMD] == 'N')
					flags = flags + IO_NDELAY
				} else
				    ioctrl = false
			    }

			    if (ioctrl)
				call fseti (STDIN, F_IOMODE, flags)
			    else {
				call zcall3 (epa_writetty, destfd, Memc[ip],
				    nchars)
			    }
			} else {
			    call write (destfd, Memc[ip], nchars)
			    if (!graphics_stream)
				call flush (destfd)
			}
		    }

		    # If writing to another process, push the current request
		    # and transfer command input to the destination process.
		    # Rewind the destfd file buffer so that it may be read by
		    # the process in a subsequent XFER call on the stream.

		    if (destpr != 0) {
			if (graphics_stream)
			    call seek (destfd, BOFL)

			push (pr)
			push (in)
			push (destfd)
			push (stream)
			push (XMIT)

			pr = destpr
			in = pr_infd[pr]
			stream = destfd
# call eprintf ("push XMIT, new in = %d, new pr = %d\n")
# call pargi (in); call pargi(pr)

		    } else if (stkp > 0) {
			# If the XMIT just completed satisfies a pending XFER
			# request, complete the XFER request and pop it from
			# the stack.

			rq = stkp - LEN_STACKFRAME + 1
			if (S_REQUEST(rq) == XFER &&
			    S_DESTFD(rq)  == destfd) {

			    pop (record_type)
			    pop (stream)
			    pop (destfd)
			    pop (in)
			    pop (pr)

			    call seek (stream, BOFL)
			    nchars = itop[stream] - iop[stream]
			    if (nchars <= 0)
				nchars = 0
			    else
				nchars = read (stream, Memc[ip], nchars)
# call eprintf ("XFER completed from fd=%d to pr=%d, %d chars\n")
# call pargi(stream); call pargi(pr); call pargi(nchars)
			    call psio_xfer (pr_outfd[pr], Memc[ip], nchars)

			    # The stream buffer should now be empty as only
			    # one IPC record at a time is buffered for an
			    # XFER request.  Mark the buffer empty.

			    call fseti (stream, F_CANCEL, OK)
			}
		    }

		case XFER:
		    # Read from a pseudofile.
		    iotype = FF_WRITE

		    if (destpr != 0 && iop[destfd] >= itop[destfd]) {
			# Read from another process.  Pseudofile FIO buffer
			# is empty.  Push the current request and transfer
			# command input to the second process to give it an
			# opportunity to write data into the buffer so that
			# we can complete the XFER request.

			push (pr)
			push (in)
			push (destfd)
			push (stream)
			push (XFER)

			pr = destpr
			in = pr_infd[pr]
			stream = destfd
# call eprintf ("push XFER, new in = %d, new pr = %d\n")
# call pargi (in); call pargi(pr)

		    } else {
			# Binary transfer.  If reading from the stream
			# associated with a pushed XMIT and the stream
			# buffer is empty, the XMIT has been completed
			# and must be popped.

			xmit_pending = false
			if (stkp > 0) {
			    rq = stkp - LEN_STACKFRAME + 1
			    if (S_REQUEST(rq) == XMIT && S_DESTFD(rq) == destfd)
				xmit_pending = true
			}

# call eprintf ("in XFER: req=%d, str=%d, xmp=%b, iop=%d, itop=%d\n")
# call pargi(S_REQUEST(rq)); call pargi(S_STREAM(rq))
# call pargb(xmit_pending); call pargi(iop[destfd]); call pargi(itop[destfd])

			if (xmit_pending && iop[destfd] >= itop[destfd]) {
			    # The pending XMIT has been completed (the stream
			    # buffer has been emptied by the reading process).
			    # Push the current XFER request back into the
			    # process input stream since we are not prepared
			    # to deal with it now.

			    itop[in] = iop[in] + nleft

			    # Pop the XMIT request.
			    pop (record_type)
			    pop (stream)
			    pop (destfd)
			    pop (in)
			    pop (pr)
			    ip = iop[in]

			    # Empty the fully read stream buffer.
			    call fseti (destfd, F_CANCEL, OK)
# call eprintf ("XFER pops XMIT; push back XFER for later\n")

			} else {
			    # Satisfy XFER by reading a data record and
			    # returning it to the requesting process.
			    # A request to read a single char enables raw
			    # mode, just as it does for ZGETTX.

# This should not be necessary since the raw mode control sequence is
# intercepted above.  Also it is incorrect since F_NDELAY is not supported.
#			    if (nchars == 1)
#				call fseti (destfd, F_RAW, YES)

			    if (destfd == STDIN) {
				nchars = zfunc3 (epa_readtty,
				    destfd, Memc[ip], nchars)
			    } else
				nchars = read (destfd, Memc[ip], nchars)
			    if (nchars == EOF)
				nchars = 0
# call eprintf ("XFER completed from fd=%d to pr=%d, %d chars\n")
# call pargi(destfd); call pargi(pr); call pargi(nchars)
			    call psio_xfer (pr_outfd[pr], Memc[ip], nchars)
			}
		    }


		case PSIO:
		    # Pseudofile i/o control directive.  These directives are
		    # used to connect graphics kernels to streams, to set and
		    # get WCS, set cursor mode parameters, etc.  An XMIT to the
		    # pseudofile PSIOCTRL is use to pass control instructions
		    # via us to GTR_CONTROL, below.  Note that the PSIOCTRL
		    # pseudofile is used to control all the graphics pseudofile
		    # streams.  The number of the graphics pseudofile stream
		    # to be operated upon by gtr_control is passed as the first
		    # integer word of the data block.

		    # Read pseudofile number.
		    iotype = 0
		    if (read (in, ps, SZ_INT32) < SZ_INT32)
			call syserr (SYS_PRIPCSYNTAX)

		    # Read data block.
		    nchars = nchars - SZ_INT32
		    if (read (in, Memc[ip], nchars) < nchars)
			call syserr (SYS_PRIPCSYNTAX)

		    # Call gtr_control to process the control directives.
		    iferr (call zcall3 (epa_control,ps,Memc[ip],pr_pid[pr]))
			call erract (EA_WARN)

		    # When writing to a graphics subkernel gtr_control may
		    # leave graphics metacode spooled in the graphics stream
		    # which we need to pass on to the subkernel.  This is
		    # done by pushing an XMIT on the psio control stack to
		    # cause the subkernel process to be polled to see if it
		    # wants the spooled data.

		    nchars = fstati (ps, F_FILESIZE)
		    if (nchars > 0) {
			destination = abs(pr_pstofd[pr,ps])
			if (destination > KSHIFT) {
			    destfd = mod (destination, KSHIFT)
			    destpr = destination / KSHIFT
			} else {
			    destfd = destination
			    destpr = 0
			}

			if (destpr != 0) {
			    call seek (destfd, BOFL)

			    push (pr)
			    push (in)
			    push (destfd)
			    push (stream)
			    push (XMIT)

			    pr = destpr
			    in = pr_infd[pr]
			    stream = destfd
			}
		    }

		case OSCMD:
		    # OS escape directive.  There are portability problems
		    # with issuing ZOSCMD os escapes from subprocesses, so
		    # subprocesses send OS escapes to us (the parent process)
		    # as !cmd commands on CLOUT.

		    Memc[ip+nchars] = EOS
		    call proscmd (pr, Memc[ip+1])
		}

	    } until (stkp <= 0 && record_type != OSCMD)

# call eprintf ("termination ps(%d)=st(%d), io(%d)=rw(%d)\n")
# call pargi(pseudofile); call pargi(stream)
# call pargi(iotype); call pargi(rwflag)
	} until (pseudofile == stream && iotype == rwflag)

# call putline (STDERR, "----------------------------------------------\n")
# call eprintf ("EXIT PSIO\n")

	if (nchars == 0)
	    return (EOF)
	else
	    return (nchars)
end
