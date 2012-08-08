# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>

# KMALLOC -- Allocate space on the heap.  Equivalent to MALLOC, except that a
# memory allocation failure is indicated by returning ERR as the function value.

int procedure kmalloc (ubufp, nelems, dtype)

pointer	ubufp			# user buffer pointer (output)
int	nelems			# number of elements of storage required
int	dtype			# datatype of the storage elements

int	sz_align, fwa_align
int	malloc1()

begin
	sz_align = SZ_MEMALIGN
	call zlocva (Memc, fwa_align)
	return (malloc1 (ubufp, nelems, dtype, sz_align, fwa_align))
end
