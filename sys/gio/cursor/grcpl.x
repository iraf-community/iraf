# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gki.h>
include	<gio.h>
include	"gtr.h"
include "grc.h"

# GRC_POLYLINE -- Draw a solid polyline.  The instruction is encoded and
# appended to the frame buffer and GIOTR is called to draw the line,
# possibly applying the workstation transformation in the process.

procedure grc_polyline (stream, v, npts)

int	stream			# graphics stream
real	v[ARB]			# polyline, NDC units
int	npts			# number of points (coord pairs) in polyline

pointer	tr, sp, p, pl, op, last_op
int	nwords, fd, save1, save2, i
int	stropen()
pointer	gtr_init(), gtr_writep()
errchk	gtr_init, gtr_writep, gki_redir

begin
	call smark (sp)
	call salloc (p, npts * 2, TY_SHORT)

	tr = gtr_init (stream)

	# Transform the type real, NDC polyline to GKI units, type short.
	do i = 1, npts * 2, 2 {
	    Mems[p+i-1] = v[i  ] * GKI_MAXNDC
	    Mems[p+i  ] = v[i+1] * GKI_MAXNDC
	}

	# Allocate space in the frame buffer for the polyline set attribute
	# and line drawing instructions.  Set the last op for undo to undo
	# the line.  This is also set by writep, hence we must wait to set
	# TR_LASTOP until after the call to writep.

	last_op = TR_OP(tr)
	nwords = GKI_PLSET_LEN + GKI_POLYLINE_LEN + (npts * 2)
	op = gtr_writep (stream, nwords)
	TR_LASTOP(tr) = last_op

	# Open the frame buffer as a file and redirect the graphics stream
	# output into the buffer.

	fd = stropen (Mems[op], nwords, NEW_FILE) 
	call gki_redir (stream, fd, save1, save2)

	# Output a polyline set attribute instruction to ensure that a solid
	# line is drawn.  Output the polyline.

	pl = TR_PLAP(tr)
	call gki_plset (stream, pl)
	call gki_polyline (stream, Mems[p], npts)

	# Restore the normal output for the stream.
	call gki_redir (stream, 0, save1, save2)
	call close (fd)

	# Call giotr to send the new instructions off the to the kernel,
	# optionally applying the workstation transformation in the process.

	call giotr (stream)
	call sfree (sp)
end
