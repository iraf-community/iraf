# SUMFLUX -- Sum up the flux in a specified bandpass

procedure sumflux (sh, x, y, s, n, eqx1, eqx2, eqy1, eqy2,
	sum, rsum, esum, ctr)

pointer	sh
real	x[n], y[n], s[n]
int	n
real	eqx1, eqx2, eqy1, eqy2
real	sum[2], rsum[2], esum[2], ctr[2]

real	slope, csum[2], sum2[2], rampval, scale, delta, wpc
real	w1, w2
int	i, i1, i2
bool	fp_equalr()

begin
	call fixx (sh, eqx1, eqx2, eqy1, eqy2, i1, i2)
	slope = (eqy2-eqy1) / (eqx2-eqx1)

	sum[1]  = 0.0
	rsum[1] = 0.0
	esum[1] = 0.0
	csum[1] = 0.0
	sum2[1] = 0.0
	scale = 0.0

	for (i=i1+1; i <= i2-1; i = i+1)
	    scale = max (scale, y[i])
	if (scale <= 0.)
	    scale = 1.

	for (i=i1+1; i <= i2-1; i = i+1) {
	    rampval = eqy1 + slope * (x[i] - eqx1)
	    sum[1] = sum[1] + y[i]
	    rsum[1] = rsum[1] + rampval
	    if (!IS_INDEF(esum[1])) {
		if (fp_equalr (0., rampval/scale))
		    esum[1] = INDEF
		else
	            esum[1] = esum[1] + (1. - y[i] / rampval)
	    }
	}

	for (i=i1+1; i <= i2-1; i = i+1) {
	    rampval = eqy1 + slope * (x[i] - eqx1)
	    delta = (y[i] - rampval) / scale
	    csum[1] = csum[1] + abs(delta)**1.5 * x[i]
	    sum2[1] = sum2[1] + abs(delta)**1.5
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

	sum[1] = sum[1] + w1 * y[i1] + w2 * y[i2]
	rsum[1] = rsum[1] + w1 * eqy1 + w2 * eqy2
	if (!IS_INDEF(esum[1])) {
	    if (fp_equalr (0., eqy1/scale)|| fp_equalr (0., eqy2/scale))
		esum[1] = INDEF
	    else
		esum[1] = esum[1] + w1 * (1. - y[i1] / eqy1) +
		    w2 * (1. - y[i2] / eqy2)
	}

	delta = (y[i1] - eqy1) / scale
	csum[1] = csum[1] + w1 * abs(delta)**1.5 * eqx1
	sum2[1] = sum2[1] + w1 * abs(delta)**1.5

	delta = (y[i2] - eqy2) / scale
	csum[1] = csum[1] + w2 * abs(delta)**1.5 * eqx2
	sum2[1] = sum2[1] + w2 * abs(delta)**1.5

	if (sum2[1] != 0.0)
	    ctr[1] = csum[1] / sum2[1]
	else
	    ctr[1] = 0.0

	# Correct for angstroms/channel
	if (i1 != i2)
	    wpc = abs ((x[i2] - x[i1]) / (i2 - i1))
	else if (i1 < n)
	    wpc = abs (x[i1+1] - x[i1])
	else
	    wpc = abs (x[i1-1] - x[i1])
	sum[1] = sum[1] * wpc
	if (!IS_INDEF(esum[1]))
	    esum[1] = esum[1] * wpc
	rsum[1] = rsum[1] * wpc

	# Errors (Note there are no errors in the ramp values).
	if (!IS_INDEF(s[1])) {
	    sum[2]  = 0.0
	    rsum[2] = 0.0
	    esum[2] = 0.0
	    csum[2] = 0.0
	    sum2[2] = 0.0
	    for (i=i1+1; i <= i2-1; i = i+1) {
		rampval = eqy1 + slope * (x[i] - eqx1)
		sum[2] = sum[2] + s[i]**2
		if (!IS_INDEF(esum[1])) {
		    if (fp_equalr (0., rampval/scale))
			esum[2] = INDEF
		    else
			esum[2] = esum[2] + (s[i] / rampval) ** 2
		}
	    }

	    for (i=i1+1; i <= i2-1; i = i+1) {
		rampval = eqy1 + slope * (x[i] - eqx1)
		delta = (y[i] - rampval) / scale
		csum[2] = csum[2] + abs(delta)*((x[i]-ctr[1])*s[i]) ** 2
	    }

	    # endpoints
	    sum[2] = sum[2] + (w1 * s[i1])**2 + (w2 * s[i2])**2
	    if (!IS_INDEF(esum[1])) {
		if (fp_equalr (0., eqy1/scale)|| fp_equalr (0., eqy2/scale))
		    esum[2] = INDEF
		else
		    esum[2] = esum[2] + (w1 * s[i1] / eqy1) ** 2 +
			(w2 * s[i2] / eqy2) ** 2
	    }

	    delta = (y[i1] - eqy1) / scale
	    csum[2] = csum[2] + abs(delta)*(w1*(eqx1-ctr[1])*s[i1]) ** 2

	    delta = (y[i2] - eqy2) / scale
	    csum[2] = csum[2] + abs(delta)*(w2*(eqx2-ctr[1])*s[i2]) ** 2

	    if (sum2[1] != 0.0)
		ctr[2] = 1.5 / scale * sqrt (csum[2]) / sum2[1]
	    else
		ctr[2] = 0.0

	    sum[2] = sqrt (sum[2])
	    esum[2] = sqrt (esum[2])

	    # Correct for angstroms/channel
	    sum[2] = sum[2] * wpc
	    if (!IS_INDEF(esum[1]))
		esum[2] = esum[2] * wpc
	}
end
