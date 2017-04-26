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

int	sz_align, fwa_align
int	malloc1()

begin
	sz_align = SZ_MEMALIGN
	call zlocva (Memc, fwa_align)
	if (malloc1 (ubufp, nelems, dtype, sz_align, fwa_align) == ERR)
	    call syserr (SYS_MFULL)
end
