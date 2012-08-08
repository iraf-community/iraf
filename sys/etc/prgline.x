# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<ctype.h>
include	<fset.h>

define	XMIT		0
define	XFER		1

# PRGETLINE -- Get a line of text from a process.  The function is equivalent
# to the ordinary FIO getline with the exception that pseudofile read and
# write directives are intercepted and processed.  Hence, the reader sees a
# stream of application specific commands need not know about pseudofile i/o.
# The function of PRGETLINE is such that it can be used in place of GETLINE
# on any file; pseudofile directives are recognized and process only if the
# FD is associated with a connected subprocess.

int procedure prgetline (fd, lbuf)

int	fd			# parent's input IPC from child process
char	lbuf[SZ_LINE]		# output line buffer

char	ch
int	nchars, maxchars, nchars_read, raw_mode_set, ndigits
int	bufsize, outfd, destfd, pr, pseudofile, line_type, offset
pointer	sp, buf, ip

char	getc()
int	getline(), read(), fstati(), ctoi(), itoc()
errchk	syserr, getline, fstati, read, write, pr_decodeargs, putc, getc
include	"prc.com"

begin
	call smark (sp)

	pr = 0
	buf = NULL
	raw_mode_set = 0

	repeat {
	    nchars = getline (fd, lbuf)

	    # Return immediately if not XMIT or XFER directive.  This code is
	    # exercised heavily when performing raw mode i/o, hence some
	    # clarity is sacrificed for the sake of efficiency.
	    #
	    # Syntax: "xmit(P,NNN)" or "xfer(P,NNN)"
	    #	       12345678         12345678
	    #
	    # where P is the pseudofile code (0<P<10) and NNN is the size of
	    # the data block in chars.  In the following code all explicit
	    # integer constants refer to the character offsets shown above.

	    if (lbuf[1] != 'x' || nchars == EOF) {
		break
	    } else if (lbuf[2] == 'm') {
		if (lbuf[3] == 'i' && lbuf[4] == 't' && lbuf[5] == '(') {
		    line_type = XMIT
		    pseudofile = TO_INTEG (lbuf[6])
		} else
		    break
	    } else if (lbuf[2] == 'f') {
		if (lbuf[3] == 'e' && lbuf[4] == 'r' && lbuf[5] == '(') {
		    line_type = XFER
		    pseudofile = TO_INTEG (lbuf[6])
		} else
		    break
	    } else
		break

	    # Ignore directive if FD not associated with a process.  To minimize
	    # searches of the process table we keep track of the slot number
	    # of the last active pid.

	    if (pr == 0) {
		if (pr_infd[pr_lastio] == fd && pr_pid[pr_lastio] != NULL)
		    pr = pr_lastio
		else {
		    for (pr=1;  pr <= MAX_CHILDPROCS;  pr=pr+1)
			if (pr_pid[pr] != NULL)
			    if (pr_infd[pr] == fd)
				break
		    if (pr > MAX_CHILDPROCS)
			break
		    pr_lastio = pr
		}
		outfd  = pr_outfd[pr]
	    }

	    # Map pseudofile code to a file descriptor in the local process.

	    destfd = pr_pstofd[pr,pseudofile]


	    # RAW mode transfers are handled as a special case to minimize the
	    # per-character overhead.

	    if (lbuf[8] == '1' && lbuf[9] == ')') {
		if (line_type == XMIT) {
		    # XMIT
		    if (getc (fd, ch) == EOF)
			call syserr (SYS_PRIPCSYNTAX)

		    # Clear RAW input mode if newline is encountered in output.
		    # Only works for STDIN/STDOUT, but that is all raw mode is
		    # used for with pseudofiles.

		    if (ch == '\n')
			if (destfd == STDOUT) {
			    call fseti (STDIN, F_RAW, NO)
			    if (raw_mode_set == STDIN)
				raw_mode_set = 0
			}

		    call putc (destfd, ch)
		    call flush (destfd)

		} else {
		    # XFER
		    if (raw_mode_set != destfd) {
			call fseti (destfd, F_RAW, YES)
			raw_mode_set = destfd
		    }

		    if (getc (destfd, ch) == EOF)
			call putline (outfd, "0\n")
		    else {
			call putline (outfd, "1\n")
			call flush (outfd)
			call putc (outfd, ch)
		    }
		    call flush (outfd)
		}
		next
	    }


	    # GENERAL XMIT or XFER directive.  Read a block of data from one
	    # stream and transmit it to the other stream.

	    if (buf == NULL) {
		bufsize = fstati (fd, F_BUFSIZE)
		call salloc (buf, bufsize, TY_CHAR)
	    }
	    
	    offset = 8
	    if (ctoi (lbuf, offset, nchars) <= 0)
		call syserr (SYS_PRIPCSYNTAX)

	    if (line_type == XMIT) {
		# XMIT -- Copy the block of data from the IPC channel to the
		# destination file.

		nchars_read = read (fd, Memc[buf], nchars)
		if (nchars_read != nchars)
		    call syserr (SYS_PRIPCSYNTAX)
		else {
		    # Clear RAW input mode if set and newline is encountered
		    # in the output stream.

		    if (destfd == STDOUT)
			if (fstati (STDIN, F_RAW) == YES)
			    for (ip=buf+nchars-1;  ip >= buf;  ip=ip-1)
				if (Memc[ip] == '\n') {
				    call fseti (STDIN, F_RAW, NO)
				    if (raw_mode_set == STDIN)
					raw_mode_set = 0
				    break
				}

		    call write (destfd, Memc[buf], nchars)
		    call flush (destfd)
		}
		next

	    } else {
		# XFER -- Read up to maxchars chars from the input file and
		# pass them on to the output IPC channel.

		maxchars = min (nchars, bufsize)
		nchars = read (destfd, Memc[buf], maxchars)
		if (nchars == EOF)
		    nchars = 0

		# Write the byte count record followed by the data record.
		# These must be written as two separate records or deadlock
		# will occur (with the reader waiting for the second record).

		ndigits = itoc (nchars, lbuf, SZ_LINE)
		lbuf[ndigits+1] = '\n'
		call write (outfd, lbuf, ndigits + 1)
		call flush (outfd)

		call write (outfd, Memc[buf], nchars)
		call flush (outfd)

		next
	    }
	}

	call sfree (sp)
	return (nchars)
end
