# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<chars.h>
include	"stdgraph.h"

# STG_READTTY -- Read a line of text from the graphics terminal.
# If the workstation is currently activated the read is performed in raw mode,
# presumably with the cursor positioned to the end of a prompt string on the
# status line.  The workstation will have been put into text mode and the
# cursor positioned to the status line by an immediately preceding call to
# stg_writetty, which is called by pseudofile i/o when the user task writes
# to STDOUT or STDERR while the workstation is activated.  The input sequence
# terminates when the user types return or newline, causing exit with
# transmission of the GE sequence to restore the terminal to graphics mode.

int procedure stg_readtty (fd, obuf, maxch)

int	fd			#I input stream	[NOT USED]
char	obuf[ARB]		#O output buffer
int	maxch			#I max chars to read

int	nchars, op, ch
int	read(), getci(), fstati()
include	"stdgraph.com"
errchk	read, getci, ttyctrl
string	delstr "\010 \010"

begin
	call flush (STDERR)
	call flush (STDOUT)

	if (g_active == NO) {
	    # Workstation in normal text mode; normal text input.
	    return (read (STDIN, obuf, maxch))

	} else if (g_msglen > 0) {
	    # The message data has already been transmitted and resides in
	    # the message buffer.

	    nchars = min (maxch, g_msglen)
	    call amovc (Memc[g_msgbuf], obuf, nchars)
	    obuf[nchars+1] = EOS
	    g_msglen = 0
	    return (nchars)

	} else {
	    # Workstation is activated; read status line in raw mode.
	    # If already in rew mode, read a single char with no echo.
	    # Note that genable is not automatic in raw input mode.

	    if (g_enable == YES)
		call stg_gdisab()

	    if (fstati (g_in, F_RAW) == YES) {
		if (getci (g_in, ch) == EOF) {
		    obuf[1] = EOS
		    return (EOF)
		} else if (ch == '\004' || ch == '\032') {
		    obuf[1] = EOS
		    return (EOF)
		} else {
		    obuf[1] = ch
		    obuf[2] = EOS
		    return (1)
		}
	    } else
		call fseti (g_in, F_RAW, YES)

	    for (op=1;  getci (g_in, ch) != EOF;  op=min(maxch,op)) {
		switch (ch) {
		case EOF, '\004', '\032':		# EOF
		    call stg_genab()
		    break
		case '\n', '\r':
		    obuf[op] = '\n'
		    op = op + 1
		    call putline (g_out, "\r\n")
		    call stg_genab()
		    break
		case INTCHAR, '\025':			# <ctrl/u>
		    for (;  op > 1;  op=op-1)
			call putline (g_out, delstr)
		case BS, '\177':
		    if (op > 1) {
			call putline (g_out, delstr)
			op = op - 1
		    } else {
			call stg_genab()
			break				# exit
		    }
		default:
		    call putci (g_out, ch)
		    obuf[op] = ch
		    op = op + 1
		}
		call flush (g_out)
	    }

	    obuf[op] = EOS
	    call fseti (g_in, F_RAW, NO)

	    if (op > 1)
		return (op - 1)
	    else
		return (EOF)
	}
end


# STG_GETLINE -- Get a line of text from the graphics terminal, reading from
# the status line if the workstation is activated, and doing a normal text
# read otherwise.

int procedure stg_getline (fd, obuf)

int	fd			#I input file
char	obuf[SZ_LINE]		#O output buffer

int	stg_readtty()

begin
	return (stg_readtty (fd, obuf, SZ_LINE))
end


# STG_MSGLEN -- This routine is called to determine if there is any message
# data buffered in the kernel, to be returned in the next call to stg_readtty.

int procedure stg_msglen (fd)

int	fd			#I input file
include	"stdgraph.com"

begin
	return (g_msglen)
end
