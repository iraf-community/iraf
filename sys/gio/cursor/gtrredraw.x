# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	"gtr.h"

# GTR_REDRAW -- Redraw the screen from the metacode spooled in the frame
# buffer.

procedure gtr_redraw (stream)

int	stream			# graphics stream to be redrawn

pointer	tr, ip_save, op_save
pointer	gtr_init()
errchk	gtr_init

begin
	tr = gtr_init (stream)

	if (TR_SPOOLDATA(tr) == YES && TR_OP(tr) > TR_FRAMEBUF(tr)) {
	    # Rewind the input pointer into the frame buffer.
	    TR_IP(tr) = TR_FRAMEBUF(tr)

	    # Redraw frame buffer.
	    call gki_clear (stream)
	    call giotr (stream)

	    # Redraw scratch buffer (axes).  Set i/o pointers to the scratch
	    # buffer and draw its contents.  Turn off interrupts to prevent
	    # an interrupt from leaving the pointers pointing to the wrong
	    # buffer.

	    if (TR_OPSB(tr) > TR_SCRATCHBUF(tr)) {
		call intr_disable()
		ip_save = TR_IP(tr);	TR_IP(tr) = TR_SCRATCHBUF(tr)
		op_save = TR_OP(tr);	TR_OP(tr) = TR_OPSB(tr)

		call giotr (stream)

		TR_IP(tr) = ip_save
		TR_OP(tr) = op_save
		call intr_enable()
	    }

	    # Flush graphics output.
	    call gki_flush (stream)
	}
end
