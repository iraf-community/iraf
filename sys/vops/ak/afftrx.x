# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AFFTRX -- Forward fourier transform (real transform, complex output).
# The fourier transform of the real array A of length NPIX pixels is computed
# and the NPIX/2+1 complex transform coefficients are returned in the complex
# array B.  The first element of array B upon output contains the dc term at
# zero frequency, and the remaining elements contain the real and imaginary
# components of the harmonics.  The transformation may be performed in place
# if desired.  NPIX must be a power of 2.
#
# N.B.: The Fortran 77 standard guarantees that a complex datum is represented
# as two reals, and that the first real in storage order is the real part of
# the complex datum and the second real the imaginary part.  We have defined
# B to be a type COMPLEX array in the calling program, but FFA expects a
# REAL array containing (real,imag) pairs.  The Fortran standard appears to
# guarantee that this will work.

procedure afftrx (a, b, npix)

real	a[ARB]		# data (input)
complex	b[ARB]		# transform (output).  Dim npix/2+1
int	npix
int	ier

begin
	# The following is a no-op if A and B are the same array.
	call amovr (a, b, npix)

	# Compute the forward real transform.
	call ffa (b, npix, ier)
	if (ier == 1)
	    call fatal (1, "afftrx: npix not a power of 2")
end
