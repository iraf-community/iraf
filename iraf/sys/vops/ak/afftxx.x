# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AFFTXX -- Forward fourier transform (complex transform, complex data).
# The fourier transform of the complex array A of length NPIX pixels is
# computed and the NPIX complex transform coefficients are returned in the
# complex array B.  The transformation may be performed in place if desired.
# NPIX must be a power of 2.

procedure afftxx (a, b, npix)

complex	a[ARB]			# data (input)
complex	b[ARB]			# transform (output)
int	npix

int	ier
pointer	sp, xr, xi
size_t	sz_val

begin
	sz_val = npix
	call smark (sp)
	call salloc (xr, sz_val, TY_REAL)
	call salloc (xi, sz_val, TY_REAL)

	# Rearrange the elements of the A array as required by FFT842.
	# Convert the array A of complex values into an array of reals
	# and an array of imaginaries.

	call aupxr (a, Memr[xr], Memr[xi], sz_val)

	# Compute the forward transform.
	call fft842 (0, npix, Memr[xr], Memr[xi], ier)
	if (ier == 1)
	    call fatal (1, "afftxx: npix not a power of 2")

	# Repack the real and imaginary arrays to form the complex output
	# array.
	call apkxr (Memr[xr], Memr[xi], b, sz_val)

	call sfree (sp)
end
