# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gki.h>
include	<gio.h>
include	"gtr.h"
include "grc.h"

# GRC_TEXT -- Draw a text string.  The instruction is encoded and appended to
# the frame buffer and GIOTR is called to draw the new instructions.

procedure grc_text (stream, x, y, text)

int	stream			# graphics stream
real	x, y			# NDC coordinates of ll corner of first char
char	text[ARB]		# text string

pointer	tr, op, last_op
int	fd, save1, save2, nwords
int	stropen(), strlen()
pointer	gtr_init(), gtr_writep()
errchk	gtr_init, stropen, gki_redir

begin
	tr = gtr_init (stream)

	# Allocate space in the frame buffer for the text set attribute
	# and text drawing instructions.  Set the last op for undo to undo
	# the line.  This is also set by writep, hence we must wait to set
	# TR_LASTOP until after the call to writep.

	last_op = TR_OP(tr)
	nwords = GKI_TXSET_LEN + GKI_TEXT_LEN + strlen(text)
	op = gtr_writep (stream, nwords)
	TR_LASTOP(tr) = last_op

	# Open the frame buffer as a file and redirect the graphics stream
	# output into the buffer.

	fd = stropen (Mems[op], nwords, NEW_FILE) 
	call gki_redir (stream, fd, save1, save2)

	# Output the set text attribute instruction and the text drawing
	# instruction.

	call gki_txset (stream, TR_TXAP(tr))
	call gki_text (stream, nint(x*GKI_MAXNDC), nint(y*GKI_MAXNDC), text)

	# Restore the normal output for the stream.
	call gki_redir (stream, 0, save1, save2)
	call close (fd)

	# Call giotr to send the new instructions off to the kernel, optionally
	# applying the workstation transformation in the process.

	call giotr (stream)
end
