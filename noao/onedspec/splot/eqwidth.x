# EQWIDTH -- Compute equivalent width, flux and center

procedure eqwidth (gfd, wx, wy, x1, x2, dx, pix, ans)

int	gfd
real	wx, wy, x1, x2, dx
real	pix[ARB]
char	ans[ARB]

char	command[SZ_FNAME]
real	wc
real	eqx1, eqx2, eqy1, eqy2
real	flux_diff, rsum, esum, sum, cont, eqw, ctr
real	temp, hctr
int	key, junk

int	clgcur()

begin
	eqx1 = wx
	eqy1 = wy

	# Get second position
	call printf ("e again:")
	junk = clgcur ("cursor", eqx2, eqy2, wc, key, command, SZ_FNAME)

	# Reverse cursor positions if necessary
	call fixx (eqx1, eqx2, eqy1, eqy2, x1, x2)

	if (eqx1 == eqx2) {
	    call printf ("Cannot get EQW - move cursor")
	    return
	}

	# Derive the needed values
	call sumflux (pix, x1, x2, dx, eqx1, eqx2, eqy1, eqy2,
	    sum, rsum, esum, ctr)

	# Compute difference in flux between ramp and spectrum
	flux_diff = rsum - sum
	
	# Compute eq. width of feature using ramp midpoint as
	# continuum
	cont = 0.5 * (eqy1 + eqy2)
	if (cont != 0.0)
	    eqw = abs (flux_diff / cont)
	else
	    eqw = INDEFR

	# Print on status line - save in answer buffer
	call printf (
	    "center = %9.7g, eqw = %9.4f, continuum = %9.7g flux = %9.6g\n")
	    call pargr (ctr)
#	    call pargr (eqw)
	    call pargr (esum)
	    call pargr (cont)
	    call pargr (-(flux_diff))

	call sprintf (ans, 2*SZ_LINE,
	    " %9.7g %9.7g %9.6g %9.4g %9w %9w %9w (%7.4g)\n")
	    call pargr (ctr)
	    call pargr (cont)
	    call pargr (-(flux_diff))
	    call pargr (esum)
	    call pargr (eqw)

	# Draw cursor position
	temp = (ctr - x1) / dx + 1.0
	hctr = pix[int(temp)]

	call gline (gfd, eqx1, eqy1, eqx2, eqy2)
	call gline (gfd, ctr, cont, ctr, hctr)
end
