# GG_LINTRAN -- Linearly transform a vector.  Map pixel values P1,P2
# onto Q1,Q2.

procedure gg_lintran (x, npix, p1in, p2in, q1, q2)

real	x[npix]			# Vector to transform
int	npix			# Number of pixels in vector
real	p1in, p2in		# Range of input values to map
real	q1, q2			# Range for output values
real	p1, p2
real	xscale

begin
	# If P1 and P2 are not set, use full range of input pixels indices.
	if (p1in == 0 && p2in == 0) {
	    p1 = 1.0
	    p2 = npix
	} else {
	    p1 = p1in
	    p2 = p2in
	}

	if (p2 - p1 == 0)
	    xscale = (q2 - q1)
	else
	    xscale = (q2 - q1) / (p2 - p1)

	call asubkr (x, p1, x, npix)
	call amulkr (x, xscale, x, npix)
	call aaddkr (x, q1, x, npix)
end
