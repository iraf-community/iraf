# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GTR_DISCONNECT -- Disconnect from a kernel subprocess.  To achieve an orderly
# shutdown we process any outstanding XMIT or XFER requests, then transmit an
# end of file (zero length record) to the kernel task when it reads from the
# graphics stream.  The kernel should then shutdown and eventually we will
# receive "bye" from the process.  We then call PRCLOSE to shutdown the
# process for good.  Note: we do not expect anything but an XFER (read) request
# on the graphics stream, but it seems prudent to do something reasonable if
# some other request is received.

procedure gtr_disconnect (pid, in, out, stream)

int	pid			# process id of subprocess
int	in, out			# command i/o streams of the subprocess
int	stream			# graphics stream used by kernel

pointer	sp, sp2, lbuf, buf
int	pseudofile, nchars, junk
bool	streq()
int	getline(), read(), strncmp(), psio_isxmit(), prclose(), pr_findproc()
errchk	getline, prclose, read, write

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	while (getline (in, Memc[lbuf]) != EOF) {
	    if (streq (Memc[lbuf], "bye\n") ||
		strncmp (Memc[lbuf], "ERROR", 5) == 0) {

		junk = prclose (pid)
		break

	    } else if (Memc[lbuf] == '!') {
		# OS escape.
		call proscmd (pr_findproc(pid), Memc[lbuf+1])

	    } else {
		call smark (sp2)

		switch (psio_isxmit (Memc[lbuf], pseudofile, nchars)) {
		case XMIT:
		    call salloc (buf, nchars, TY_CHAR)
		    nchars = read (in, Memc[buf], nchars)
		    if (nchars > 0)
			if (pseudofile == STDOUT || pseudofile == STDERR)
			    call write (pseudofile, Memc[buf], nchars)

		case XFER:
		    call salloc (buf, nchars, TY_CHAR)
		    if (pseudofile == STDIN)
			nchars = read (pseudofile, Memc[buf], nchars)
		    else
			nchars = 0		# this is the EOF
		    call psio_xfer (out, Memc[buf], nchars)
		}

		call sfree (sp2)
	    }
	}

	call sfree (sp)
end
