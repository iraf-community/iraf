# SUMFLUX -- Sum up the flux in a specified bandpass

procedure sumflux (pix, w0, we, wpc, eqx1, eqx2, eqy1, eqy2,
	sum, rsum, esum, ctr)

real	pix[ARB]
real	w0, we, wpc, eqx1, eqx2, eqy1, eqy2, sum, rsum, esum, ctr

real	slope, csum, sum2, rampval, scale, delta
real	w1, w2
int	i, i1, i2
bool	fp_equalr()

begin
	i1 = aint ((eqx1-w0)/wpc +0.5) + 1
	i2 = aint ((eqx2-w0)/wpc +0.5) + 1

	slope = (eqy2-eqy1) / (eqx2-eqx1)

	sum  = 0.0
	rsum = 0.0
	esum = 0.0
	csum = 0.0
	sum2 = 0.0
	scale = 0.0

	for (i=i1+1; i <= i2-1; i = i+1) {
	    rampval = eqy1 + slope * (w0 + (i-1)*wpc - eqx1)
	    sum = sum + pix[i]
	    rsum = rsum + rampval
	    if (!IS_INDEF(esum)) {
		if (fp_equalr (0., rampval))
		    esum = INDEF
		else
	            esum = esum + (1. - pix[i] / rampval)
	    }
	    scale = max (scale, pix[i])
	}
	if (scale <= 0.)
	    scale = 1.

	for (i=i1+1; i <= i2-1; i = i+1) {
	    rampval = eqy1 + slope * (w0 + (i-1)*wpc - eqx1)
	    delta = (pix[i] - rampval) / scale
	    csum = csum + abs(delta)**1.5 * (w0 + (i-1)*wpc)
	    sum2 = sum2 + abs(delta)**1.5
	}

	# start pt
	w1 = (w0 + (i1-1)*wpc - eqx1) / wpc
	w2 = 1.0 - (w0 + (i2-1)*wpc - eqx2) / wpc

	sum = sum + w1 * pix[i1] + w2 * pix[i2]
	rsum = rsum + w1 * eqy1 + w2 * eqy2
	if (!IS_INDEF(esum)) {
	    if (fp_equalr (0., eqy1)|| fp_equalr (0., eqy2))
		esum = INDEF
	    else
		esum = esum + w1 * (1. - pix[i1] / eqy1) +
		    w2 * (1. - pix[i2] / eqy2)
	}

	delta = (pix[i1] - eqy1) / scale
	csum = csum + w1 * abs(delta)**1.5 * eqx1
	sum2 = sum2 + w1 * abs(delta)**1.5

	delta = (pix[i1] - eqy1) / scale
	csum = csum + w2 * abs(delta)**1.5 * eqx2
	sum2 = sum2 + w2 * abs(delta)**1.5

	if (sum2 != 0.0)
	    ctr = csum / sum2
	else
	    ctr = 0.0

	# Correct for angstroms/channel
	sum = sum * wpc
	esum = esum * wpc
	rsum = rsum * wpc
end
