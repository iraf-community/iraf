# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_ALLOC -- Allocate space in the line list buffer, returning the llbuf
# offset of the allocated area as the function value.  If overflow occurs
# the buffer is resized.

int procedure pl_alloc (pl, nwords)

pointer	pl			#I mask descriptor
int	nwords			#I number of words of storage to allocate

int	newbuf
int	len, o_len, inc, op
errchk	realloc

begin
	len = PL_LLLEN(pl)		# current buffer length
	inc = PL_LLINC(pl)		# length increment
	op  = PL_LLOP(pl)		# next available location

	newbuf = op
	op = newbuf + nwords

	for (o_len = len;  op >= len;  ) {
	    inc = min (PL_MAXINC, inc * 2)
	    len = len + inc
	}

	if (len != o_len)
	    call realloc (PL_LLBP(pl), len, TY_SHORT)

	PL_LLLEN(pl) = len
	PL_LLINC(pl) = inc
	PL_LLOP(pl)  = op

	return (newbuf)
end
