# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	<fset.h>

define	PAUSE		20	# pause if no data, msec


# TTYREAD -- Read from the terminal in raw mode until sufficient data is
# accumulated to match the given encoded pattern.  Any additional data read
# prior to the matched pattern (normally due to type-ahead) is pushed back
# into the input stream.  If timeout > 0 nonblocking reads are used, and
# the operation will time out if the pattern is not matched within the given
# interval in milliseconds.  A valid read returns nchars > 0 indicating the
# length of the matched pattern; 0 is returned for a timeout and ERR for a
# garbled read.

int procedure ttyread (fd, tty, outbuf, maxch, patbuf, timeout)

int	fd			# output file
pointer	tty			# terminal descriptor
char	outbuf[maxch]		# output data buffer
int	maxch			# max chars out
char	patbuf[ARB]		# encoded pattern
int	timeout			# timeout interval, msec (0 for no timeout)

bool	match
pointer	sp, ip, op, buf
int	sv_iomode, iomode, delay, first, last, nchars, ch
int	fstati(), patmatch(), gpatmatch(), getci(), gstrcpy()
errchk	getci, unread
define	abort_ 91

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Save raw mode state and set up for nonblocking raw mode reads
	# if a timeout interval was specified.

	iomode = IO_RAW
	if (timeout > 0)
	    iomode = iomode + IO_NDELAY

	sv_iomode = fstati (fd, F_IOMODE)
	if (sv_iomode != iomode)
	    call fseti (fd, F_IOMODE, iomode)

	outbuf[1] = EOS
	match = false
	nchars = 0
	delay = 0
	op = buf

	# Accumulate input characters in nonblocking raw mode until either
	# the given pattern is matched, or we timeout.

	repeat {
	    # Read characters until the full sequence has been input or no
	    # more data is available.

	    while (!match && (op-buf < SZ_LINE) && getci(fd, ch) != EOF) {
		if (ch==INTCHAR || ch==EOFCHAR || ch == '\r' || ch == '\n') {
		    nchars = ERR
		    goto abort_
		}

		Memc[op] = ch;  op = op + 1
		Memc[op] = EOS
		match = (gpatmatch (Memc[buf], patbuf, first, last) > 0)

		if (match) {
		    ip = buf + first - 1
		    if (first > 1) {
			# Put back any input typed before our data block.
			call unread (fd, Memc[buf], first-1)

			# Redo the match to correct index marks for string
			# offset.
			match = (patmatch (Memc[ip], patbuf) > 0)
		    }
		    nchars = gstrcpy (Memc[ip], outbuf, maxch)
		}
	    }

	    # If the nonblocking read returns EOF, indicating no input was
	    # queued, wait a bit and try again.

	    if (!match && ch == EOF) {
		call zwmsec (PAUSE)
		delay = delay + PAUSE
	    }
	} until (match || delay > timeout)

abort_
	# Restore previous raw mode state.
	if (sv_iomode != iomode)
	    call fseti (fd, F_IOMODE, sv_iomode)

	call sfree (sp)
	return (nchars)
end
