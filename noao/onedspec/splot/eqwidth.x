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
real	wx2, wy2
real	flux_diff, rsum, esum, sum, cont, eqw, ctr
int	i, wc, key

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

	# Derive the needed values
	call sumflux (sh, x, y, n, wx1, wx2, wy1, wy2, sum, rsum, esum, ctr)

	# Compute difference in flux between ramp and spectrum
	flux_diff = sum - rsum
	
	# Compute eq. width of feature using ramp midpoint as
	# continuum
	cont = 0.5 * (wy1 + wy2)
	if (cont != 0.0)
	    eqw = -flux_diff / cont
	else
	    eqw = INDEFR

	# Print on status line - save in answer buffer
	call printf (
	    "center = %9.7g, eqw = %9.4f, continuum = %9.7g flux = %9.6g\n")
	    call pargr (ctr)
#	    call pargr (eqw)
	    call pargr (esum)
	    call pargr (cont)
	    call pargr (flux_diff)

	if (fd1 != NULL) {
	    call fprintf (fd1, " %9.7g %9.7g %9.6g %9.4g %9w %9w %9w (%7.4g)\n")
		call pargr (ctr)
		call pargr (cont)
		call pargr (flux_diff)
		call pargr (esum)
		call pargr (eqw)
	}
	if (fd2 != NULL) {
	    call fprintf (fd2, " %9.7g %9.7g %9.6g %9.4g %9w %9w %9w (%7.4g)\n")
		call pargr (ctr)
		call pargr (cont)
		call pargr (flux_diff)
		call pargr (esum)
		call pargr (eqw)
	}

	# Draw cursor position
	i = max (1, min (n, nint (shdr_wl (sh, double(ctr)))))
	call gline (gfd, wx1, wy1, wx2, wy2)
	call gline (gfd, ctr, cont, ctr, y[i])
end
