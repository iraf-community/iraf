# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include	<config.h>

# MALLOC -- Allocate space on the heap.  An array of at least NELEMS elements
# of data type DTYPE is allocated, aligned to SZ_MEMALIGN (the biggest type)
# with the global common Mem.

procedure malloc (ubufp, nelems, dtype)

pointer	ubufp			# user buffer pointer (output)
int	nelems			# number of elements of storage required
int	dtype			# datatype of the storage elements

extern	kmalloc(), krealloc(), mfree(), realloc(), salloc(), vmalloc()
int	first_time, locpr()

int	sz_align, fwa_align
int	malloc1()
int	zrtadr()
include	"memdbg.com"
data	first_time /0/

begin
	# Reference the other MEMDEBUG routines to force them to be loaded.
	if (first_time == 0) {
	    retaddr = locpr (kmalloc)
	    retaddr = locpr (krealloc)
	    retaddr = locpr (mfree)
	    retaddr = locpr (realloc)
	    retaddr = locpr (salloc)
	    retaddr = locpr (vmalloc)
	    first_time = 1
	}

	retaddr = zrtadr()
	sz_align = SZ_MEMALIGN
	call zlocva (Memc, fwa_align)
	if (malloc1 (ubufp, nelems, dtype, sz_align, fwa_align) == ERR)
	    call syserr (SYS_MFULL)
end
