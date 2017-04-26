# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTR_TRUNCATE -- Truncate the frame buffer, which has grown larger than
# the limit set by the user (or the system default).  This is done by moving
# the metacode data at the end of the buffer (beginning with the word pointed
# to by gki) to the maximum upper limit of the buffer and adjusting the input
# and output pointers accordingly.

procedure gtr_truncate (tr, gki)

pointer	tr			# giotr descriptor
pointer	gki			# pointer to first word to be preserved
pointer	top
int	nwords

begin
	# Find the first instruction preceding the soft upper limit on the
	# size of the buffer.

	top = TR_FRAMEBUF(tr) + TR_MAXLENFRAMEBUF(tr)
	while (Mems[top] != BOI && top > TR_FRAMEBUF(tr))
	    top = top - 1

	# Move the partial instruction likely to be at the end of the buffer
	# to the new "top".  Note that we can only truncate (discard)
	# instructions which have already been executed, hence the partial
	# instruction at the end of the buffer must be preserved.

	if (gki != top) {
	    nwords = TR_OP(tr) - gki
	    call amovs (Mems[gki], Mems[top], nwords)
	    TR_IP(tr) = top
	    TR_OP(tr) = top + nwords
	}
end
