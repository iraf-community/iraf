# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALUT -- Map an array using table lookup.  Note that an input value of zero
# indexes the first element of the lookup table.  No bounds checking is
# performed.

procedure alutr (a, b, npix, lut)

int	a[ARB]				# input array of indices

real	b[ARB]				# output data array
real	lut[ARB]			# lookup table
int	npix, i

begin
	do i = 1, npix
	    b[i] = lut[a[i]+1]
end
