# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>


# MSVFWA -- Determine the buffer address which satisfies the maximum alignment
# criteria, save the buffer fwa in the integer cell immediately preceding
# this, and return a pointer to the user area of the buffer.

pointer procedure msvfwa (fwa, dtype, nelem, sz_align, fwa_align)

int	fwa, dtype, nelem, sz_align, fwa_align, nbits
pointer	bufptr, lwl, offset

pointer	mgdptr()
int	coerce(), sizeof()

include "nmemio.com"

begin
	# Compute the pointer to the data area which satisfies the desired
	# alignment criteria.  Store the fwa of the actual OS allocated buffer
	# in the integer cell preceeding the data area.

	bufptr = mgdptr (fwa, TY_INT, sz_align, fwa_align)

	nbits = sizeof(TY_INT) * 8 * SZB_CHAR
	if (nbits == 64) {
	    if (sizeof (dtype) == sizeof (TY_CHAR))
	        offset = (nelem / SZ_INT + 1)
	    else if (sizeof (dtype) == sizeof (TY_REAL))
	        offset = (nelem / SZ_REAL + 1)
	    else
	        offset = nelem

	} else if (nbits == 32) { 

	    if (sizeof(dtype) <= SZ_INT)
	        offset = (nelem / (SZ_INT / sizeof(dtype))) + 1
	    else
	        offset = (nelem * sizeof (dtype)) / SZB_CHAR
	}

	lwl = bufptr + offset

	Memi[bufptr-5] = fwa				# first word address
	Memi[bufptr-4] = lwl				# last word location
	Memi[bufptr-3] = dtype				# data type
	Memi[bufptr-2] = nelem				# no. of elements
	Memi[bufptr-1] = lsentinal			# lower sentinal
	Memi[lwl]      = usentinal 			# upper sentinal

	# Return pointer of type dtype to the first cell of the data area.
	return (coerce (bufptr, TY_INT, dtype))
end
