# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AFFTXR -- Forward fourier transform (complex transform, real arrays).
# The fourier transform of the real arrays SR and SI containing complex data
# pairs is computed and the complex transform coefficients are returned in
# the real arrays FR and FI.  The transformation may be performed in place if
# desired.  NPIX must be a power of 2.

procedure afftxr (sr, si, fr, fi, npix)

real	sr[ARB], si[ARB]	# data, spatial domain (input)
real	fr[ARB], fi[ARB]	# transform, frequency domain (output)
int	npix
int	ier

begin
	# The following are no-ops if the transform is being performed
	# in place.

	call amovr (sr, fr, npix)
	call amovr (si, fi, npix)

	# Compute the forward transform.
	call fft842 (0, npix, fr, fi, ier)
	if (ier == 1)
	    call fatal (1, "afftxr: npix not a power of 2")
end
