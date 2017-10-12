# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MGDPTR -- Given the fwa of a memory area, compute a pointer to the start
# of the data area which satisfies the desired alignment criteria.  Memory
# is allocated in units of chars, and ZLOCVA, ZMALOC, etc., return pointers
# in units of chars.

pointer procedure mgdptr (fwa, dtype, sz_align, fwa_align)

int	fwa, dtype, sz_align, fwa_align
long	bufadr
pointer	bufptr
int	modulus, loc_Mem
int	sizeof()
data	loc_Mem /NULL/

begin
	# Compute the address of the start of the user buffer area, which
	# must be aligned with fwa_align (usually Mem) for all data types.

	if (loc_Mem == NULL)
	    call zlocva (Memc, loc_Mem)
	bufadr = fwa + (5 * SZ_INT)

	modulus = mod (bufadr - fwa_align, sz_align)
	if (modulus < 0)
	    modulus = modulus + sz_align
	if (modulus != 0)
	    bufadr = bufadr + (sz_align - modulus)

	# Compute the buffer pointer for the desired datatype.
	bufptr = (bufadr - loc_Mem) / sizeof(dtype) + 1

	return (bufptr)
end
