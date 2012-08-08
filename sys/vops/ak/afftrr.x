# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AFFTRR -- Forward fourier transform (real transform, real output arrays).
# The forward transform of the real array SR length NPIX is computed and
# returned in the real arrays FR and FI of length NPIX/2+1.  Since the real
# transform is being performed the array SI is ignored and may be omitted.
# The transformation may be performed in place if desired.  NPIX must be a
# power of 2.

procedure afftrr (sr, si, fr, fi, npix)

real	sr[ARB], si[ARB]	# spatial data (input).  SI NOT USED.
real	fr[ARB], fi[ARB]	# real and imag parts of transform (output)
int	npix
int	ier
pointer	sp, work

begin
	call smark (sp)
	call salloc (work, npix + 2, TY_REAL)

	# Copy the real data vector into the work array.
	call amovr (sr, Memr[work], npix)

	# Compute the forward transform.
	call ffa (Memr[work], npix, ier)
	if (ier == 1)
	    call fatal (1, "afftrr: npix not a power of 2")

	# Unpack the real and imaginary parts into the output arrays.
	call aupxr (Memr[work], fr, fi, npix / 2 + 1)

	call sfree (sp)
end
