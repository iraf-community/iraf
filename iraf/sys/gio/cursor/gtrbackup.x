# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_BACKUP -- Backup one drawing instruction in the frame buffer.  Erase
# the graphics if possible.  The effects of this function may be undone by
# the UNDO operator.

procedure gtr_backup (stream)

int	stream				# graphics stream

int	opcode
pointer	tr, op, bp, sp, ap
pointer	gtr_init()
errchk	gtr_init
include	"gtr.com"

begin
	call smark (sp)
	call salloc (ap, LEN_PL, TY_STRUCT)

	tr = gtr_init (stream)

	# Scan backward to the beginning of the last drawing instruction in the
	# frame buffer.

	op = TR_OP(tr)
	bp = TR_FRAMEBUF(tr)
	if (op <= bp) {
	    call sfree (sp)
	    return
	}

	repeat {
	    op = op - 1
	    while (Mems[op] != BOI)
		if (op <= bp) {
		    TR_OP(tr) = bp
		    TR_IP(tr) = bp
		    call sfree (sp)
		    return
		} else
		    op = op - 1
	    opcode = Mems[op+GKI_HDR_OPCODE-1]
	} until (opcode >= GKI_POLYLINE && opcode <= GKI_PUTCELLARRAY)

	# Redraw the last instruction to erase it (device permitting).
	if (opcode == GKI_POLYLINE) {
	    PL_LTYPE(ap) = GL_CLEAR
	    PL_WIDTH(ap) = 1.0
	    PL_COLOR(ap) = 1
	    call gki_plset (stream, ap)

	    if (wstranset == YES)
		call gtr_wstran (Mems[op])
	    else
		call gki_write (stream, Mems[op])

	    PL_LTYPE(ap) = GL_SOLID
	    call gki_plset (stream, ap)
	    call gki_fflush (stream)
	}

	# Return the space in the buffer.
	TR_LASTOP(tr) = TR_OP(tr)
	TR_OP(tr) = op
	TR_IP(tr) = min (op, TR_IP(tr))

	call sfree (sp)
end
