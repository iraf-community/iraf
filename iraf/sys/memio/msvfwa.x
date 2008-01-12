# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MSVFWA -- Determine the buffer address which satisfies the maximum alignment
# criteria, save the buffer fwa in the integer cell immediately preceding
# this, and return a pointer to the user area of the buffer.

pointer procedure msvfwa (fwa, dtype, sz_align, fwa_align)

int	fwa, dtype, sz_align, fwa_align
pointer	bufptr, mgdptr()
int	coerce()

begin
	# Compute the pointer to the data area which satisfies the desired
	# alignment criteria.  Store the fwa of the actual OS allocated buffer
	# in the integer cell preceeding the data area.

	bufptr = mgdptr (fwa, TY_INT, sz_align, fwa_align)
	Memi[bufptr-1] = fwa

	# Return pointer of type dtype to the first cell of the data area.
	return (coerce (bufptr, TY_INT, dtype))
end
