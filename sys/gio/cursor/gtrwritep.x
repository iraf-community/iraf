# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<gio.h>
include	<gki.h>
include "gtr.h"

# GTR_WRITEP -- Virtually write (append) to the graphics frame buffer.  Return a
# pointer to the start of the area reserved for the data and advance the
# output pointer beyond the new data area.  The use of a buffer pointer here
# yields a very efficient graphics i/o dataflow.  For the stdgraph kernel,
# XMIT (pr_psio) places a block of metacode directly in the frame buffer at
# the memory location we point to.  GIOTR is then called to process the new
# data block.  GIOTR calls GTR_FETCH, which "fetches" the next instruction
# by merely returning a pointer into the frame buffer.  The stdgraph kernel
# is then called to execute the instruction.  Hence in the simple case, there
# are no memory to memory copies and the contents of an instruction are
# touched only by the kernel.

pointer procedure gtr_writep (fd, nchars)

int	fd			# graphics stream
int	nchars			# nchars to reserve at end of buffer

pointer	tr, bufp, top, segp
int	blen, nwords, ip_offset, op_offset
errchk	syserr, realloc
include	"gtr.com"

begin
	tr = trdes[fd]
	if (tr == NULL)
	    call syserr (SYS_GWRITEP)

	nwords = nchars / SZ_SHORT
	bufp = TR_FRAMEBUF(tr)
	blen = TR_LENFRAMEBUF(tr)
	segp = TR_OP(tr)			# pointer to next segment
	top  = bufp + blen

	# Make space available in the buffer.  We must always allocate the
	# requested space, even if the result is a buffer larger than the
	# (soft) maximum size permitted.  Buffer space will be returned
	# after GIOTR processes the new instructions if the buffer grows
	# too large.

	if (nwords > top - segp) {
	    # Note that realloc may move the buffer, hence we must adjust any
	    # pointers into the buffer after the call to realloc.
	    
	    ip_offset = TR_IP(tr) - bufp
	    op_offset = segp - bufp
	    blen = blen + max (INC_LENFRAMEBUF, nwords)

	    call realloc (bufp, blen, TY_SHORT)

	    TR_FRAMEBUF(tr) = bufp
	    TR_LENFRAMEBUF(tr) = blen
	    TR_IP(tr) = bufp + ip_offset
	    segp = bufp + op_offset
	} 

	TR_OP(tr) = segp + nwords
	TR_LASTOP(tr) = TR_OP(tr)

	return (segp)
end
