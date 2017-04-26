# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<mach.h>

# VMALLOC -- Like malloc, but force the buffer to be aligned on a virtual
# memory page boundary.  This feature can be used, e.g., in 4.XBSD UNIX
# to "bypass" the system buffer cache (to avoid copying file data from the
# system cache into the file buffer).  VMALLOC can be made equivalent to MALLOC
# via the parameters in <config.h>, if the local machine which does not have
# virtual memory.

procedure vmalloc (ubufp, nelems, dtype)

pointer	ubufp			# user buffer pointer (output)
int	nelems			# number of elements of storage required
int	dtype			# datatype of the storage elements

int	sz_align, fwa_align
int	malloc1()

begin
	sz_align = SZ_VMEMALIGN
	fwa_align = VMEM_BASE
	if (malloc1 (ubufp, nelems, dtype, sz_align, fwa_align) == ERR)
	    call syserr (SYS_MFULL)
end
