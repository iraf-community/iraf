# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_WRITETTY -- Write one or more lines of text to the terminal in text mode.
# If the workstation is currently activated output is to the status line,
# otherwise output is to the indicated stream (STDOUT or STDERR).
# Terminal output is directed to the status line by the GD sequence, and
# graphics output is reenabled by the GE sequence.  The output text should
# consist of only a single line, but if multiple lines are present they are
# output line by line, without the trailing newline, since the status line
# can display only a single line of text.
#
# NOTE - If output occurs while in graphics mode and the output text is newline
# terminated, the GE (graphics enable) sequence is output to restore the
# terminal to graphics mode before exiting.  If the text is not newline
# terminated, e.g., if it is a prompt, the workstation is left in alpha mode,
# ready for a read from STDIN.  Thus one can write a prompt to STDOUT and read
# the user response from STDIN, while in graphics mode.
#
# This procedure is called by pseudofile i/o (gio/cursor/prpsio) whenever a
# task writes to STDOUT or STDERR.

procedure stg_writetty (fd, text, nchars)

int	fd			# output stream
char	text[ARB]		# text to be output
int	nchars			# nchars to be written

int	ip
pointer	sp, lbuf, op
include	"stdgraph.com"
errchk	write

begin
	if (g_active == NO) {
	    # Workstation not activated (normal text mode); normal text output.
	    call write (STDOUT, text, nchars)
	    call flush (STDOUT)

	} else {
	    # Workstation is activated; write to status line.  Writing anything
	    # when graphics is enabled causes the status line to be cleared;
	    # newline causes a graphics enable; the string "\n\n" will always
	    # clear the status line and leave the terminal in graphics mode,
	    # regardless of the state of g_enable when issued.

	    call smark (sp)
	    call salloc (lbuf, SZ_LINE, TY_CHAR)

	    if (g_enable == YES)
		call stg_gdisab()

	    op = lbuf
	    for (ip=1;  ip <= nchars;  ip=ip+1) {
		if (text[ip] == '\n') {
		    if (g_enable == YES)
			call stg_gdisab()
		    if (op > lbuf)
			call write (g_out, Memc[lbuf], op-lbuf)
		    call stg_genab()
		    op = lbuf
		} else {
		    Memc[op] = text[ip]
		    op = min (lbuf+SZ_LINE, op+1)
		}
	    }

	    # Output a partial line, leaving graphics disabled.
	    if (op > lbuf) {
		if (g_enable == YES)
		    call stg_gdisab()
		call write (g_out, Memc[lbuf], op-lbuf)
	    }

	    call flush (g_out)
	    call sfree (sp)
	}
end


# STG_PUTLINE -- Output an EOS delimited line of text to the graphics terminal
# with stg_writetty.

procedure stg_putline (fd, text)

int	fd			# output file
char	text[ARB]		# EOS delimited line of text
int	strlen()

begin
	call stg_writetty (fd, text, strlen(text))
end
