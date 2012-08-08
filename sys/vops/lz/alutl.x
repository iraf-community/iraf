# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ALUT -- Map an array using table lookup.  Note that an input value of zero
# indexes the first element of the lookup table.  No bounds checking is
# performed.

procedure alutl (a, b, npix, lut)

long	a[ARB]

long	b[ARB]				# output data array
long	lut[ARB]			# lookup table
int	npix, i

begin
	do i = 1, npix
	    b[i] = lut[a[i]+1]
end
