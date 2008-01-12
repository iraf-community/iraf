# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttset.h>
include	<chars.h>
include	<fset.h>
include	"stdgraph.h"

define	MAXCH	16
define	RESET	"reset"


# SGF_POST_FILTER -- Post the stdgraph tty input filter to the VOS tty driver.
# This input filter is used to intercept and process escape sequences sent by
# the terminal to the IRAF client, to notify the client of events such as a
# terminal reset.

procedure sgf_post_filter (fd)

int	fd			#I terminal file

int	locpr()
extern	sgf_ttyfilter()

begin
	# Install stdgraph filter in terminal driver.
	call ttseti (fd, TT_FILTER, locpr(sgf_ttyfilter))
	call ttseti (fd, TT_FILTERKEY, ESC)

	# Register escapes with terminal.
	call stg_outstr ("EZ", RESET)
	call stg_outstr ("ER", "R")
end


# SGF_TTYFILTER -- Terminal input filter.

procedure sgf_ttyfilter (fd, buf, maxch, status)

int	fd			#I input file
char	buf[ARB]		#U input buffer
int	maxch			#I max chars in buffer
int	status			#U number of chars in buffer

char	escape[MAXCH]
char	svbuf[MAXCH+4]
int	ip, op, sp, ch, iomode

bool	streq()
int	sgf_getchar(), fstati()
include	"stdgraph.com"
define	failed_ 91

begin
	# Disable the filter if reading from the terminal in nonblocking
	# raw mode.  We shouldn't receive a stdgraph escape at such a time,
	# and this code isn't prepared to deal with nonblocking i/o.  This
	# case occurs, e.g., during a screen size query, where the terminal
	# returns an escape sequence to the client (in nonblocking raw mode).

	iomode = fstati (STDIN, F_IOMODE)
	if (and (iomode, IO_NDELAY) != 0)
	    return

	# The escape sequence is of the form "ESC P <text> ESC \", the ANSI
	# device control string (DCS).  This escape sequence is recognized by
	# the vt100 terminal emulator in xgterm, which will accumulate and
	# ignore the sequence.  This is important because when a terminal
	# (xgterm) reset occurs when IRAF is not reading from the terminal in
	# raw mode, the character are echoed to the terminal and would be
	# printed on the screen if not recognized by the terminal as an
	# escape.  By using a known escape which xgterm ignores the escape is
	# transmitted without being seen by (and probably confusing) the
	# user.  If the reset occurs while in graphics mode and a cursor read
	# is in progress, the terminal will be in raw mode and the sequence
	# will not be echoed, hence the problem does not occur.

	ip = 1
	sp = 1
	ch = sgf_getchar (fd, svbuf, sp, buf, ip, maxch, status)
	if (ch != ESC)
	    goto failed_
	ch = sgf_getchar (fd, svbuf, sp, buf, ip, maxch, status)
	if (ch != 'P')
	    goto failed_

	# Accumulate escape data string.
	op = 1
	repeat {
	    ch = sgf_getchar (fd, svbuf, sp, buf, ip, maxch, status)
	    if (ch < 0 || op > MAXCH)
		goto failed_
	    if (ch == ESC) {
		escape[op] = EOS
		ch = sgf_getchar (fd, svbuf, sp, buf, ip, maxch, status)
		break
	    } else {
		escape[op] = ch
		op = op + 1
	    }
	}

	# Process the escape.
	if (streq (escape, RESET)) {
	    call stg_reset()
	    call ttseti (fd, TT_FILTER, NULL)
	    if (g_sg != NULL)
		SG_UIFDATE(g_sg) = 0
	} else				# add additional escapes here
	    goto failed_

	# Edit the input buffer to remove the escape.
	op = 1
	for (  ;  ip <= status && op <= maxch;  ip=ip+1) {
	    buf[op] = buf[ip]
	    op = op + 1
	}
	status = op - 1
	return

failed_
	# Unrecognized escape.  Append any newly read data to the input
	# buffer and return all the data.

	if (sp > 1) {
	    call amovc (svbuf, buf[status+1], sp - 1)
	    status = status + sp - 1
	}
end


# SGF_GETCHAR -- Get a character from the input terminal.  ERR or EOF is
# returned if the input is exhausted.  If reading in raw mode additional
# reads will be performed as necessary.

int procedure sgf_getchar (fd, svbuf, sp, buf, ip, maxch, nchars)

int	fd			#I input file
char	svbuf[ARB]		#O save chars as they are read
int	sp			#U pointer into save buffer
char	buf[ARB]		#U input buffer
int	ip			#I input index
int	maxch			#I max chars in buffer
int	nchars			#U number of chars in buffer

int	ch
int	status

begin
	if (ip > nchars) {
	    if (maxch == 1) {
		call zgetty (fd, svbuf[sp], maxch, status)
		if (status <= 0)
		    return (ERR)
		ch = svbuf[sp]
		sp = sp + 1
		return (ch)
	    } else
		return (EOF)
	}

	ch = buf[ip]
	ip = ip + 1

	return (ch)
end
