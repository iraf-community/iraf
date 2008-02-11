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
size_t	sz_val

begin
	# The following are no-ops if the transform is being performed
	# in place.

	sz_val = npix
	call amovr (fr, sr, sz_val)
	call amovr (fi, si, sz_val)

	# Compute the inverse transform.
	call fft842 (1, npix, sr, si, ier)
	if (ier == 1)
	    call fatal (1, "afftxr: npix not a power of 2")
end
