# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AIFTRR -- Inverse fourier transform (real transform, real output arrays).
# The inverse transform of the real arrays FR and FI of length NPIX/2+1 is
# returned in the real array SR of length NPIX.  Since the real inverse
# transform is being performed the array SI is ignored and may be omitted.
# The transformation may be performed in place if desired.  NPIX must be a
# power of 2.

procedure aiftrr (fr, fi, sr, si, npix)

real	fr[ARB], fi[ARB]	# real and imag parts of transform (input)
real	sr[ARB], si[ARB]	# spatial data (output).  SI NOT USED.
int	npix
int	ier
pointer	sp, work

begin
	call smark (sp)
	call salloc (work, npix + 2, TY_REAL)

	# Pack the real and imaginary parts into a complex array as required
	# by FFS.
	call apkxr (fr, fi, Memr[work], npix / 2 + 1)

	# Compute the inverse transform.
	call ffs (Memr[work], npix, ier)
	if (ier == 1)
	    call fatal (1, "aiftrr: npix not a power of 2")

	# The work array now contains the real part of the transform; merely
	# copy it to the output array.
	call amovr (Memr[work], sr, npix)

	call sfree (sp)
end
