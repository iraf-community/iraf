# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AIFTRX -- Inverse discreet fourier transform (real transform, complex data
# array in).  The input array A of length NPIX/2+1 contains the DC term and
# the NPIX/2 (real,imag) pairs for each of the NPIX/2 harmonics of the real
# transform.  Upon output array B contains the NPIX real data pixels from the
# inverse transform.  The transform may be performed in place if desired.
#
# N.B.: The Fortran 77 standard guarantees that a complex datum is represented
# as two reals, and that the first real in storage order is the real part of
# the complex datum and the second real the imaginary part.  We have defined
# B to be a type COMPLEX array in the calling program, but FFS expects a
# REAL array containing (real,imag) pairs.  The Fortran standard appears to
# guarantee that this will work.

procedure aiftrx (a, b, npix)

complex	a[ARB]			# transform, npix/2+1 elements
real	b[ARB]			# output data array
int	npix
int	ier

begin
	# The following is a no-op if A and B are the same array.
	call amovx (a, b, npix / 2 + 1)

	# Compute the inverse real transform.
	call ffs (b, npix, ier)
	if (ier == 1)
	    call fatal (1, "afftrx: npix not a power of 2")
end
