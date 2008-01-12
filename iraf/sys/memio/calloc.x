# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CALLOC -- Allocate and zero a block of memory.

procedure calloc (ubufp, buflen, dtype)

pointer	ubufp			# user buffer pointer [OUTPUT]
int	buflen			# nelements of space required,
int	dtype		# of this data type

pointer	char_ptr
pointer	coerce()
int	sizeof()
errchk	malloc

begin
	call malloc (ubufp, buflen, dtype)
	char_ptr = coerce (ubufp, dtype, TY_CHAR)
	call aclrc (Memc[char_ptr], buflen * sizeof (dtype))
end
