# EQWIDTH -- Compute equivalent width, flux and center

procedure eqwidth (sh, gfd, wx1, wy1, x, y, n, fd1, fd2)

pointer	sh
int	gfd
real	wx1, wy1
real	x[ARB]
real	y[ARB]
int	n
int	fd1, fd2

char	command[SZ_FNAME]
real	wx2, wy2, sigma0, invgain
real	flux_diff, rsum[2], esum[2], sum[2], cont, ctr[2]
int	i, wc, key
pointer	sp, s

real	clgetr()
int	clgcur()
double	shdr_wl()

begin
	# Get second position
	call printf ("e again:")
	i = clgcur ("cursor", wx2, wy2, wc, key, command, SZ_FNAME)

	if (wx1 == wx2) {
	    call printf ("Cannot get EQW - move cursor")
	    return
	}

	# Set noise.
	sigma0 = clgetr ("sigma0")
	invgain = clgetr ("invgain")
	if (IS_INDEF(sigma0) || IS_INDEF(invgain) || sigma0<0. || invgain<0.) {
	    sigma0 = INDEF
	    invgain = INDEF
	}
	call smark (sp)
	call salloc (s, n, TY_REAL)
	if (IS_INDEF(invgain))
	    call amovkr (INDEF, Memr[s], n)
	else {
	    do i = 1, n {
		if (y[i] > 0)
		    Memr[s+i-1] = sqrt (sigma0  ** 2 + invgain * y[i])
		else
		    Memr[s+i-1] = sqrt (sigma0  ** 2)
	    }
	}

	# Derive the needed values
	call sumflux (sh, x, y, Memr[s], n, wx1, wx2, wy1, wy2,
	    sum, rsum, esum, ctr)

	# Compute difference in flux between ramp and spectrum
	flux_diff = sum[1] - rsum[1]
	
	# Compute eq. width of feature using ramp midpoint as
	# continuum
	cont = 0.5 * (wy1 + wy2)

	# Print on status line - save in answer buffer
	call printf (
	    "center = %9.7g, eqw = %9.4f, continuum = %9.7g flux = %9.6g\n")
	    call pargr (ctr[1])
	    call pargr (esum[1])
	    call pargr (cont)
	    call pargr (flux_diff)

	if (fd1 != NULL) {
	    call fprintf (fd1, " %9.7g %9.7g %9.6g %9.4g\n")
		call pargr (ctr[1])
		call pargr (cont)
		call pargr (flux_diff)
		call pargr (esum[1])
	}
	if (fd2 != NULL) {
	    call fprintf (fd2, " %9.7g %9.7g %9.6g %9.4g\n")
		call pargr (ctr[1])
		call pargr (cont)
		call pargr (flux_diff)
		call pargr (esum[1])
	}
	if (!IS_INDEF(sigma0)) {
	    if (fd1 != NULL) {
		call fprintf (fd1,
		"  (%7.5g) %9w (%7.4g) (%7.2g)\n")
		    call pargr (ctr[2])
		    call pargr (sum[2])
		    call pargr (esum[2])
	    }
	    if (fd2 != NULL) {
		call fprintf (fd2,
		"  (%7.5g) %9w (%7.4g) (%7.2g)\n")
		    call pargr (ctr[2])
		    call pargr (sum[2])
		    call pargr (esum[2])
	    }
	}

	# Draw cursor position
	i = max (1, min (n, nint (shdr_wl (sh, double(ctr[1])))))
	call gline (gfd, wx1, wy1, wx2, wy2)
	call gline (gfd, ctr[1], cont, ctr[1], y[i])

	call sfree (sp)
end
