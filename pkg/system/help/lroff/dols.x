# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	<error.h>
include	"lroff.h"

# DO_LS -- Depending on the action, push an LS block (print label and indent
# one level), or pop an LS block (restore the previous indent level).  INIT_LS
# clears the LS stack.  Called with the LS command line in "lbuf", minus
# the ".ls".  The command line may consist of an argument specifying the number
# of spaces to indent, the label string, both, or neither.
# If the label string is shorter than the amount by which the block is indented,
# the text block will begin on the same line as the label, otherwise the line
# is broken and the text block begins on the following line.

procedure do_LS (out, lbuf, action, last_command)

extern	out()
char	lbuf[ARB]			# ".ls [arg] [text]"
int	action				# LS or LE
int	last_command

int	indent[MAX_NLS]
int	n, ip, next_output_column
int	lgetarg(), strlen()
errchk	skiplines, outstr, outc, breakline
include	"lroff.com"

begin
	switch (action) {
	case LS:
	    # We normally skip a line when beginning an LS block.  If two or
	    # more LS directives are given in a row, however, only skip a
	    # single line.

	    call breakline (out, NJ)
	    if (last_command != LS)
		call skiplines (out, 1)
	    call testpage (out, 3)

	    # Push new LS block on stack.
	    nls = nls + 1
	    if (nls > MAX_NLS) {
		iferr (call error (1, "LS blocks nested too deep"))
		    call erract (EA_WARN)
		nls = MAX_NLS
	    }

	    # Get number of spaces to indent, if given.  If arg is negative,
	    # do not remember the argument, otherwise make it the new default.
	    ip = 1
	    n = lgetarg (lbuf, ip, ls_indent)
	    if (n < 0)
		indent[nls] = -n
	    else {
		ls_indent = n
		indent[nls] = ls_indent
	    }

	    # Copy the label, if any, into the output buffer.  We must do this
	    # before we change the left margin since the label is not indented.

	    call outstr (out, lbuf[ip])

	    # Try to adjust the left margin by the indicated amount.  Save the
	    # actual indentation level for restoration by LE.

	    indent[nls] = max (perm_left_margin, min (right_margin,
		left_margin + indent[nls])) - left_margin
	    left_margin = left_margin + indent[nls]

	    # If the length of the label string plus one blank does not leave
	    # space to start the first line of the text block on the same line,
	    # we must break the line and start the block on the next line.
	    # Otherwise, output spaces until the new left margin is reached.

	    if (strlen (lbuf[ip]) >= indent[nls])
		call outc (out, '\n')
	    else {
		call getoutcol (next_output_column)
		while (next_output_column < left_margin) {
		    call outc (out, BLANK)
		    call getoutcol (next_output_column)
		}
	    }

	case LE:					# end LS block
	    call breakline (out, NJ)
	    if (nls >= 1) {
		left_margin = left_margin - indent[nls]
		nls = nls - 1
	    }

	default:
	    call error (1, "do_LS")
	}
end


# INIT_LS -- Set or clear any LS indentation.

procedure init_ls()

include	"lroff.com"

begin
	nls = 0
end
