# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<mach.h>
include	<fio.h>

# PUTLINE -- Put a line or part of a line of text to a file.  May be called
# several times to build up a line of text.  FLUSHNL should always be set
# for text files (when the file is opened) to avoid flushing partial lines
# to text files.  This is a major output procedure hence the code has been
# carefully optimized to do as much as possible in loops.

procedure putline (fd, linebuf)

int	fd				# output file
char	linebuf[ARB]			# line to be output

int	ch, ip, ip_top, i
pointer	op, op_top
int	and()
errchk	syserr, flsbuf
include	<fio.com>
define	done_ 91

begin
	if (fd <= 0 || fiodes[fd] == NULL)
	    call syserr (SYS_FILENOTOPEN)

	# Copy the i/o pointers into local storage for more efficient access.
	op = iop[fd]
	op_top = otop[fd]

	# Check for a file fault.
	if (op < bufptr[fd] || op > op_top) {
	    call flsbuf (fd, 0)
	    op = iop[fd]
	    op_top = otop[fd]
	}

	# Copy all characters until EOS is seen.  Flush the buffer if it fills
	# or if newline is seen and FF_FLUSH is set for the stream.

	if (and (fflags[fd], FF_FLUSH) == 0) {
	    # Flush on newline is disabled.  A special loop is used for this
	    # case to eliminate the need to compare every character against
	    # newline.

	    for (ip=1;  linebuf[ip] != EOS;  ip=ip_top+1) {
		# A do loop is used here to trigger Fortran optimization.  Note
		# that FLSBUF must not be called from within the loop or loop
		# optimization will be turned off by most compilers.

		ip_top = ip + (op_top-op) - 1
		do i = ip, ip_top {
		    Memc[op] = linebuf[i]
		    op = op + 1
		    if (linebuf[i+1] == EOS)
			goto done_
		}

		# If we reach here then the buffer is full and needs to be
		# flushed.

		iop[fd] = op
		call flsbuf (fd, 0)
		op = iop[fd]
		op_top = otop[fd]
	    }

	} else {
	    # This section of code is used when it is necessary to check for
	    # newline and flush after every line of text.

	    for (ip=1;  linebuf[ip] != EOS;  ip=ip_top+1) {
		ip_top = ip + (op_top-op) - 1
		do i = ip, ip_top {
		    ch = linebuf[i]
		    Memc[op] = ch
		    op = op + 1
		    if (ch == '\n') {
			ip_top = i
			break
		    }
		    if (linebuf[i+1] == EOS)
			goto done_
		}

		# If we get here then either newline has been seen or the output
		# buffer is full.  In either case the buffer must be flushed.

		iop[fd] = op
		call flsbuf (fd, 0)
		op = iop[fd]
		op_top = otop[fd]
	    }
	}
done_
	iop[fd] = op
	FNCHARS(fiodes[fd]) = ip - 1
end
