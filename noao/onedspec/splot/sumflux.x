# SUMFLUX -- Sum up the flux in a specified bandpass

procedure sumflux (sh, x, y, n, eqx1, eqx2, eqy1, eqy2,
	sum, rsum, esum, ctr)

pointer	sh
real	x[n], y[n]
int	n
real	eqx1, eqx2, eqy1, eqy2
real	sum, rsum, esum, ctr

real	slope, csum, sum2, rampval, scale, delta, wpc
real	w1, w2
int	i, i1, i2
bool	fp_equalr()

begin
	call fixx (sh, eqx1, eqx2, eqy1, eqy2, i1, i2)
	slope = (eqy2-eqy1) / (eqx2-eqx1)

	sum  = 0.0
	rsum = 0.0
	esum = 0.0
	csum = 0.0
	sum2 = 0.0
	scale = 0.0

	for (i=i1+1; i <= i2-1; i = i+1) {
	    rampval = eqy1 + slope * (x[i] - eqx1)
	    sum = sum + y[i]
	    rsum = rsum + rampval
	    if (!IS_INDEF(esum)) {
		if (fp_equalr (0., rampval))
		    esum = INDEF
		else
	            esum = esum + (1. - y[i] / rampval)
	    }
	    scale = max (scale, y[i])
	}
	if (scale <= 0.)
	    scale = 1.

	for (i=i1+1; i <= i2-1; i = i+1) {
	    rampval = eqy1 + slope * (x[i] - eqx1)
	    delta = (y[i] - rampval) / scale
	    csum = csum + abs(delta)**1.5 * x[i]
	    sum2 = sum2 + abs(delta)**1.5
	}

	# end points
	if (eqx1 < x[i1]) {
	    if (i1 > 1)
		w1 = (x[i1] - eqx1) / (x[i1] - x[i1-1])
	    else
		w1 = (x[i1] - eqx1) / (x[i1+1] - x[i1])
	} else {
	    if (i1 < n)
		w1 = (x[i1] - eqx1) / (x[i1+1] - x[i1])
	    else
		w1 = (x[i1] - eqx1) / (x[i1] - x[i1-1])
	}
	if (eqx2 < x[i2]) {
	    if (i2 > 1)
		w2 = (x[i2] - eqx2) / (x[i2] - x[i2-1])
	    else
		w2 = (x[i2] - eqx2) / (x[i2+1] - x[i2])
	} else {
	    if (i2 < n)
		w2 = (x[i2] - eqx2) / (x[i2+1] - x[i2])
	    else
		w2 = (x[i2] - eqx2) / (x[i2] - x[i2-1])
	}
	w2 = 1.0 - w2

	sum = sum + w1 * y[i1] + w2 * y[i2]
	rsum = rsum + w1 * eqy1 + w2 * eqy2
	if (!IS_INDEF(esum)) {
	    if (fp_equalr (0., eqy1)|| fp_equalr (0., eqy2))
		esum = INDEF
	    else
		esum = esum + w1 * (1. - y[i1] / eqy1) +
		    w2 * (1. - y[i2] / eqy2)
	}

	delta = (y[i1] - eqy1) / scale
	csum = csum + w1 * abs(delta)**1.5 * eqx1
	sum2 = sum2 + w1 * abs(delta)**1.5

	delta = (y[i1] - eqy1) / scale
	csum = csum + w2 * abs(delta)**1.5 * eqx2
	sum2 = sum2 + w2 * abs(delta)**1.5

	if (sum2 != 0.0)
	    ctr = csum / sum2
	else
	    ctr = 0.0

	# Correct for angstroms/channel
	if (i1 != i2)
	    wpc = (x[i2] - x[i1]) / (i2 - i1)
	else if (i1 < n)
	    wpc = x[i1+1] - x[i1]
	else
	    wpc = x[i1-1] - x[i1]
	sum = sum * wpc
	esum = esum * wpc
	rsum = rsum * wpc
end
