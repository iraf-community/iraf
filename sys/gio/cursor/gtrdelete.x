# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_DELETE -- Delete an instruction from the frame buffer.  This prevents
# the instruction from being executed if the frame is redrawn.

procedure gtr_delete (tr, gki)

pointer	tr			#I giotr descriptor
pointer	gki			#I instruction to be deleted

pointer	inext
int	nwords, shift, ilen

begin
	ilen = Mems[gki+GKI_HDR_LENGTH-1]
	inext = gki + ilen

	if (inext >= TR_OP(tr)) {
	    # Instruction is the last one in the buffer.
	    TR_OP(tr) = gki
	    TR_LASTOP(tr) = TR_OP(tr)
	    if (TR_IP(tr) >= gki)
		TR_IP(tr) = gki

	} else {
	    # If the instruction is small and would be expensive to delete
	    # just change the opcode to disable it, otherwise shift the
	    # buffer contents back to overwrite the deleted instruction.

	    nwords = TR_OP(tr) - inext
	    if (ilen < 32 && nwords > 2048)
		Mems[gki+GKI_HDR_OPCODE-1] = GKI_UNKNOWN
	    else {
		call amovs (Mems[inext], Mems[gki], nwords)
		shift = inext - gki
		TR_IP(tr) = TR_IP(tr) - shift
		TR_OP(tr) = TR_OP(tr) - shift
		TR_LASTOP(tr) = TR_OP(tr)
	    }
	}
end
