# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_UNDO -- Undo the last frame buffer edit.  Successive pairs of undos leave
# the frame buffer unchanged.

procedure gtr_undo (stream)

int	stream				# graphics stream
int	opcode
pointer	tr, op, new_op, old_op, sp, ap

pointer	gtr_init()
errchk	gtr_init
include	"gtr.com"

begin
	call smark (sp)
	call salloc (ap, LEN_PL, TY_STRUCT)

	tr = gtr_init (stream)

	old_op = TR_OP(tr)
	new_op = TR_LASTOP(tr)
	if (new_op == old_op || new_op <= TR_FRAMEBUF(tr)) {
	    call sfree (sp)
	    return
	}

	# Edit the frame buffer.
	TR_LASTOP(tr) = old_op
	TR_OP(tr) = new_op
	TR_IP(tr) = min (new_op, TR_IP(tr))

	# Redraw the last drawing instruction to erase it (device permitting),
	# if we are backing up one instruction.  Note that it may be necessary
	# to skip one or more control instructions.  We assume that the undo
	# only has to undo one drawing instruction.

	if (new_op < old_op) {
	    op = new_op
	    repeat {
		opcode = Mems[op+GKI_HDR_OPCODE-1]
		if (opcode == GKI_POLYLINE)
		    break
		else
		    op = op + Mems[op+GKI_HDR_LENGTH-1]
	    } until (op >= old_op)

	    if (opcode == GKI_POLYLINE && op < old_op) {
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
	    }

	} else if (new_op > old_op) {
	    # Call giotr to redraw the recovered instructions.
	    call giotr (stream)
	}

	call gki_flush (stream)
	call sfree (sp)
end
