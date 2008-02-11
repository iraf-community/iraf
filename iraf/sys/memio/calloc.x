# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CALLOC -- Allocate and zero a block of memory.

procedure calloc (ubufp, buflen, dtype)

pointer	ubufp			# user buffer pointer [OUTPUT]
size_t	buflen			# nelements of space required,
int	dtype		# of this data type

pointer	char_ptr
size_t	npix, i
pointer	coerce()
int	sizeof()
errchk	malloc

begin
	call malloc (ubufp, buflen, dtype)
	char_ptr = coerce (ubufp, dtype, TY_CHAR)
	npix = buflen * sizeof (dtype)
	do i = 0, npix-1
	    Memc[char_ptr + i] = 0
end
