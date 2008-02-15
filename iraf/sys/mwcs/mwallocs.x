# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_ALLOCS -- Allocate space in the global string buffer.  The size of the
# buffer is automatically increased if necessary.  Note that reallocation of
# the buffer may cause it to move, hence all data items are referred to by
# their offset in the buffer, rather than by an absolute pointer.  Since we
# are allocating space for string data, a space for the EOS is automatically
# allocated in addition to space for the indicated number of data chars.

int procedure mw_allocs (mw, nchars)

pointer	mw		#I pointer to MWCS descriptor
int	nchars		#I number of chars to allocate space for

int	sbufused, sbuflen, offset, nelem
errchk	realloc

begin
	sbufused = MI_SBUFUSED(mw)
	sbuflen  = MI_SBUFLEN(mw)
	offset   = sbufused + 1
	nelem	 = nchars + 1

	# Increase buffer size?
	if (sbufused + nelem > sbuflen) {
	    sbuflen = sbuflen + INC_SZSBUF
	    while (sbufused + nelem > sbuflen)
		sbuflen = sbuflen + INC_SZSBUF

	    call realloc (MI_SBUF(mw), sbuflen, TY_CHAR)
	    call aclrc (S(mw,offset), sbuflen - offset + 1)
	    MI_SBUFLEN(mw) = sbuflen
	}

	# Allocate the space in the buffer, and return the buffer offset
	# of the allocated area.

	MI_SBUFUSED(mw) = max (0, sbufused + nelem)
	return (offset)
end
