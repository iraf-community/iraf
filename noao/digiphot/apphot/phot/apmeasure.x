# APMEASURE -- Procedure to measure the fluxes and effective areas of a set of
# apertures.

procedure apmeasure (im, wx, wy, c1, c2, l1, l2, aperts, sums, areas, naperts)

pointer	im			# pointer to image
real	wx, wy			# center of subraster
int	c1, c2			# column limits
int	l1, l2			# line limits
real	aperts[ARB]		# array of apertures
real	sums[ARB]		# array of sums
real	areas[ARB]		# aperture areas
int	naperts			# number of apertures

int	i, j, k, nx, yindex
pointer	buf
real	xc, yc, apmaxsq, dy2, r2, r, fctn
pointer	imgs2r()

begin
	# Initialize.
	nx = c2 - c1 + 1
	xc = wx - c1 + 1
	yc = wy - l1 + 1
	apmaxsq = (aperts[naperts] + 0.5) ** 2
	call aclrr (sums, naperts)
	call aclrr (areas, naperts)

	# Loop over the pixels.
	do j = l1, l2 {
	    buf = imgs2r (im, c1, c2, j, j)
	    if (buf == EOF)
		return
	    yindex = j - l1 + 1
	    dy2 = (yindex - yc) ** 2
	    do i = 1, nx {
		r2 = (i - xc) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		r = sqrt (r2) - 0.5
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    sums[k] = sums[k] + fctn * Memr[buf+i-1]
		    areas[k] = areas[k] + fctn
		}
	    }
	}
end
