# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_FRAME -- Clear the frame buffer, used to spool the metacode instructions
# required to draw a graphics frame.  This is done by moving the metacode data
# at the end of the buffer (beginning with the word pointed to by gki) to the
# beginning of the buffer and adjusting the input and output pointers
# accordingly.  The workstation transformation is also reset to the unitary
# transformation when the frame is cleared, i.e., zoom is cancelled.

procedure gtr_frame (tr, gki, stream)

pointer	tr			# giotr descriptor
pointer	gki			# pointer to first word to be preserved
int	stream			# graphics stream

pointer	bp
int	nwords, shift

begin
	bp = TR_FRAMEBUF(tr)

	if (gki > bp) {
	    nwords = TR_OP(tr) - gki
	    call amovs (Mems[gki], Mems[bp], nwords)
	    shift = gki - bp
	    TR_IP(tr) = TR_IP(tr) - shift
	    TR_OP(tr) = TR_OP(tr) - shift
	} else {
	    TR_IP(tr) = bp
	    TR_OP(tr) = bp
	}

	call gtr_ptran (stream, 0., 1., 0., 1.)
	TR_OPSB(tr) = TR_SCRATCHBUF(tr)
	TR_LASTOP(tr) = TR_OP(tr)
	TR_WCS(tr) = NULL
end
