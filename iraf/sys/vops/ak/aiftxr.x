# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AIFTXR -- Inverse fourier transform (complex transform, real arrays).
# The inverse transform of the real arrays FR and FI containing complex data
# pairs is computed and the complex spatial data coefficients are returned in
# the real arrays SR and SI.  The transformation may be performed in place if
# desired.  NPIX must be a power of 2.

procedure aiftxr (fr, fi, sr, si, npix)

real	fr[ARB], fi[ARB]	# transform, frequency domain (input)
real	sr[ARB], si[ARB]	# data, spatial domain (output)
int	npix
int	ier

begin
	# The following are no-ops if the transform is being performed
	# in place.

	call amovr (fr, sr, npix)
	call amovr (fi, si, npix)

	# Compute the inverse transform.
	call fft842 (1, npix, sr, si, ier)
	if (ier == 1)
	    call fatal (1, "afftxr: npix not a power of 2")
end
