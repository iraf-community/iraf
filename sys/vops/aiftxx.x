# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AIFTXX -- Inverse fourier transform (complex transform, complex array).
# The fourier transform of the complex array A of length NPIX pixels is
# computed and the NPIX complex data points are returned in the complex array
# B.  The transformation may be performed in place if desired.  NPIX must be
# a power of 2.
#
# N.B.: The Fortran 77 standard guarantees that a complex datum is represented
# as two reals, and that the first real in storage order is the real part of
# the complex datum and the second real the imaginary part.  We have defined
# A and B to be type COMPLEX arrays in the calling program, but FFT842 expects
# a REAL array containing (real,imag) pairs.  The Fortran standard appears to
# guarantee that this will work.

procedure aiftxx (a, b, npix)

complex	a[ARB]			# transform (input)
complex	b[ARB]			# data (output)
int	npix
int	ier
pointer	sp, xr, xi

begin
	call smark (sp)
	call salloc (xr, npix, TY_REAL)
	call salloc (xi, npix, TY_REAL)

	# Rearrange the elements of the A array as required by FFT842.
	# Convert the array A of complex values into an array of reals
	# and an array of imaginaries.

	call aupxr (a, Memr[xr], Memr[xi], npix)

	# Compute the inverse transform.
	call fft842 (1, npix, Memr[xr], Memr[xi], ier)
	if (ier == 1)
	    call fatal (1, "afftxx: npix not a power of 2")

	# Repack the real and imaginary arrays to form the complex output
	# array.
	call apkxr (Memr[xr], Memr[xi], b, npix)

	call sfree (sp)
end
