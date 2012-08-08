# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>

# XERPUTC -- Low level routine, called by the error handling code, to
# accumulate an error command, and send it off to the CL.  Clumsy, but
# necessary to avoid recursion if an error abort occurs in a routine
# such as PUTC or FLSBUF.  The buffer is automatically flushed to CLOUT
# when newline is encountered.

procedure xerputc (ch)

char	ch
int	op, junk, nchars
char	msg[SZ_LINE+1]
include	<fio.com>
data	op /1/

begin
	msg[op] = ch

	if (ch == '\n' || op > SZ_PATHNAME) {
	    fp = fiodes[CLOUT]
	    nchars = op
	    op = 0

	    if (FTYPE(fp) == TEXT_FILE)
		call fputtx (CLOUT, msg, nchars, junk)
	    else {
		call zcall4 (ZAWRBF(fp), FCHAN(fp), msg, nchars * SZB_CHAR, 0)
	    }
	}

	op = op + 1
end
