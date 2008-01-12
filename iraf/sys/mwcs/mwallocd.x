# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_ALLOCD -- Allocate space in the data buffer.  The size of the buffer
# is automatically increased if necessary.  Note that reallocation of the
# buffer may cause it to move, hence all data items are referred to by their
# offset in the buffer, rather than by an absolute pointer.

int procedure mw_allocd (mw, nelem)

pointer	mw		#I pointer to MWCS descriptor
int	nelem		#I number of elements to alloc space for

int	dbufused, dbuflen, offset
errchk	realloc

begin
	dbufused = MI_DBUFUSED(mw)
	dbuflen  = MI_DBUFLEN(mw)
	offset   = dbufused + 1

	# Increase buffer size?
	if (dbufused + nelem > dbuflen) {
	    dbuflen = dbuflen + INC_SZDBUF
	    while (dbufused + nelem > dbuflen)
		dbuflen = dbuflen + INC_SZDBUF

	    call realloc (MI_DBUF(mw), dbuflen, TY_DOUBLE)
	    call aclrd (D(mw,offset), dbuflen - offset + 1)
	    MI_DBUFLEN(mw) = dbuflen
	}

	# Allocate the space in the buffer, and return the buffer offset
	# of the allocated area.

	MI_DBUFUSED(mw) = max (0, dbufused + nelem)
	return (offset)
end
