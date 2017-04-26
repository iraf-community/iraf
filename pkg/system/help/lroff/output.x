# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<chars.h>
include	"lroff.h"

.help outstr, outc, getoutcol, set_outbuf
.nf __________________________________________________________________________
OUTSTR, OUTC, OUTCOL -- Routines for buffering the output line.  An output
line is built up with calls to OUTPUT to pass strings, and to OUTC to pass
characters.  OUTCOL may be called to determine the index of the PRINTABLE
column at which the next character will be deposited (this is unaffected by
control chars in the output stream).

The output buffer is flushed when OUTC is called to deposit a newline
character, by calling the user supplied output procedure.  The output column
pointer is reset to the CURRENT left margin when the first character of the
new line is deposited.  Any changes to the left margin made after the first
character is deposited do not take effect until the next line.  OUTCOL returns
the value of the current left margin if called when the buffer is empty.

NOTE: set_outbuf() must be called during Lroff startup and shutdown to
allocate the output buffer, the size of which depends on the maximum output
line length.
.endhelp _____________________________________________________________________


# SET_OUTBUF -- Allocate and initialize the output buffer.

procedure set_outbuf (outbuf_size)

int	outbuf_size		# new buffer size in chars

include	"lroff.com"
errchk	malloc, out
bool	first_time
data	first_time /true/

pointer	obuf, op, otop
int	col, old_left_margin, buffer_empty
common	/lroout/ obuf, op, otop, col, old_left_margin, buffer_empty

begin
	if (first_time) {
	    obuf = NULL
	    buffer_empty = YES
	    first_time = false
	}

	if (outbuf_size <= 0 && obuf != NULL)
	    call mfree (obuf, TY_CHAR)
	else {
	    call malloc (obuf, outbuf_size, TY_CHAR)
	    otop = obuf + outbuf_size - 1
	    op = obuf
	    buffer_empty = YES
	    old_left_margin = 1
	}
end


# OUTC -- Output a single character.  Note that the character value is
# passed as an integer.

procedure outc (out, ch)

extern	out()			# user line output procedure
int	ch			# character to be output

char	text[1]
data	text[1] /EOS/

begin
	text[1] = ch
	call outstr (out, text)
end


# OUTSTR -- Output a text string.

procedure outstr (out, text)

extern	out()			# user line output procedure
char	text[ARB]		# text string to be output

int	ch, ip, i
pointer	obuf, op, otop
int	col, old_left_margin, buffer_empty
common	/lroout/ obuf, op, otop, col, old_left_margin, buffer_empty
include	"lroff.com"

begin
	for (ip=1;  text[ip] != EOS;  ) {
	    if (buffer_empty == YES) {
		if (obuf == NULL)
		    call error (1, "No Lroff output buffer allocated")

		# If left margin has been moved inward, blank out the unused
		# columns.

		if (left_margin != old_left_margin) {
		    for (i=old_left_margin;  i < left_margin;  i=i+1)
			Memc[obuf+i-1] = BLANK
		    old_left_margin = left_margin
		}

		op = obuf + left_margin - 1
		col = left_margin
		buffer_empty = NO
	    }

	    # Move the text string into the buffer.  The string may contain
	    # more than one line of text.

	    for (;  text[ip] != EOS;  ip=ip+1) {
		ch = text[ip]
		Memc[op] = ch
		op = op + 1

		if (INVISIBLE(ch) || op > otop) {
		    if (ch == '\r' || ch == '\n') {
			# Flush the buffer.
			Memc[op] = EOS
			call out (out_magic_arg, Memc[obuf])
			buffer_empty = YES

			# If all text data has not been copied (buffer overflow
			# or newline embedded in the text), we must reinit the
			# buffer and copy the remaining data.  Otherwise we must
			# return without calling the buffer_empty code to give
			# the caller a chance to change the left margin.

			if (text[ip+1] == EOS)
			    return
		    }
		} else
		    col = col + 1
	    }
	}
end


# GETOUTCOL -- Return the index of the next column of output.

procedure getoutcol (next_column)

int	next_column		# next col to be written (output)
pointer	obuf, op, otop
int	col, old_left_margin, buffer_empty
common	/lroout/ obuf, op, otop, col, old_left_margin, buffer_empty
include	"lroff.com"

begin
	if (buffer_empty == YES)
	    next_column = left_margin
	else
	    next_column = col
end


# OUTCC -- Output a control sequence, i.e., a forms control sequence.
# Called only after a line has already been output.  Does not interfere
# with output buffer.  Sequence is not newline terminated.

procedure outcc (out, ctrlchar)

extern	out()			# user supplied line output procedure
int	ctrlchar		# character to be output (INT)
char	ctrlstr[1]
include	"lroff.com"

begin
	ctrlstr[1] = ctrlchar
	ctrlstr[2] = EOS
	call out (out_magic_arg, ctrlstr)
end


# OUTLINE -- Output a string and append a newline to flush the output buffer.

procedure outline (out, text)

extern	out()
char	text[ARB]
errchk	outstr

begin
	call outstr (out, text)
	call outc (out, '\n')
end
