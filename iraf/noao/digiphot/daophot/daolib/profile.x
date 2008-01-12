include "../lib/daophotdef.h"

define	NGL	4

# DP_PROFILE -- Evaluate the analytic part of the psf function and its
# derivatives.

real procedure dp_profile (ipstyp, dx, dy, par, dhdxc, dhdyc, term, ideriv)

int	ipstyp			# the analytic psf function type
real	dx, dy			# distance of point from center of function
real	par[ARB]		# the current parameter values
real	dhdxc, dhdyc		# derivatives of the function integral wrt x,y
real	term[ARB]		# derivatives of the function wrt parameters
int	ideriv			# compute the derivatives ?

int	npt, ix, iy
real	d[NGL,NGL], w[NGL,NGL], x[NGL], xsq[NGL], p1xsq[NGL]
real	p1p2, dhdsx, dhdsy, erfx, erfy, p1sq, p2sq, y, ysq, p2ysq, profile
real	xy, wt, denom, alpha, func, p4fod, wp4fod, f, e, rsq, wf
real	wfsq, onemp3, dfby, deby, dbyx0, dbyy0
real	daoerf()

data    d / 0.0,         0.0,        0.0,        0.0,
            -0.28867513,  0.28867513, 0.0,        0.0,
            -0.38729833,  0.0,        0.38729833, 0.0,
            -0.43056816, -0.16999052, 0.16999052, 0.43056816 /
data    w / 1.0,         0.0,        0.0,        0.0,
             0.5,         0.5,        0.0,        0.0,
             0.27777778,  0.44444444, 0.27777778, 0.0,
             0.17392742,  0.32607258, 0.32607258, 0.17392742 /
begin
	# Initialize.
	profile = 0.0
	dhdxc = 0.0
	dhdyc = 0.0

	# Compute the analytic part of the profile for a given x and y.

	switch (ipstyp) {

	# Evaluate the Gaussian function
	#     f = erfx * erfy / (par[1] * par[2])
	#     par[1] is the hwhm in x; sigma(x) = 0.8493218 * hwhm
	#     par[2] is the hwhm in y; sigma(y) = 0.8493218 * hwhm

	case FCTN_GAUSS:

	    p1p2 = par[1] * par[2]
	    erfx = daoerf (dx, 0.0, par[1], dhdxc, dhdsx)
	    erfy = daoerf (dy, 0.0, par[2], dhdyc, dhdsy)
	    profile = erfx * erfy / p1p2
	    dhdxc = dhdxc * erfy / p1p2
	    dhdyc = dhdyc * erfx / p1p2
	    if (ideriv > 0) {
		term[1] = (dhdsx - erfx / par[1]) * erfy / p1p2
		term[2] = (dhdsy - erfy / par[2]) * erfx / p1p2
	    }

	# Evaluate the Moffat25 function
	#
	# f = (beta - 1) / (ax * ay * (1 + (x/ax) ** 2 + (y/ay) ** 2 +
	#     (xy * axy)) ** beta)
	#
	#     par[1] is the hwhm in x at y = 0.0
	#     1/2 = 1 / (1 + (par[1] / ax) ** 2) ** beta
	# so
	#     2 ** (1/ beta) - 1 = (par[1] / ax) ** 2
	#     ax ** 2 = par[1] ** 2 / (2 ** (1 / beta) - 1)
	#
	# when   beta = 2.5   ax ** 2 = 3.129813 * par[1] ** 2
	#
	# f = 1 / par[1] * par[2] * (1 + 0.3195079 * ((x / par[1]) ** 2 +
	#     (y / par[2]) ** 2 + xy * par[3])) ** 2.5
	#

	case FCTN_MOFFAT25:

	    alpha = 0.3195079
	    #talpha = 0.6390158
	    p1sq = par[1] ** 2
	    p2sq = par[2] ** 2
	    p1p2 = par[1] * par[2]
	    xy = dx * dy
	    if (ideriv > 0)
	        call aclrr (term, 4)

	    denom = 1.0 + alpha * (dx ** 2 / p1sq + dy ** 2 / p2sq + xy *
		par[3])
	    if (denom > 1.0e4)
		return (profile)
	    func = 1.0 / (p1p2 * denom ** par[4])
	    if (func >= 0.046) {
		npt = 4
	    } else if (func >= 0.0022) {
		npt = 3
	    } else if (func >= 0.0001) {
		npt = 2
	    } else if (func >= 1.0e-10) {
		profile = (par[4] - 1.0) * func
		p4fod = par[4] * alpha * profile / denom
		dhdxc = p4fod * (2.0 * dx / p1sq + dy * par[3])
		dhdyc = p4fod * (2.0 * dy / p2sq + dx * par[3])
		if (ideriv > 0) {
		    term[1] =  (2.0 * p4fod * dx ** 2 / p1sq - profile) /
		        par[1]
		    term[2] =  (2.0 * p4fod * dy ** 2 / p2sq - profile) /
		        par[2]
		    term[3] = - p4fod * xy
		    term[4] = profile * (1.0 / (par[4] - 1.0) - log (denom))
		}
		return (profile)
	    } else {
		return (profile)
	    }

	    do ix = 1, npt {
		x[ix] = dx + d[ix,npt]
		xsq[ix] = x[ix] ** 2
		p1xsq[ix] = xsq[ix] / p1sq
	    }

	    do iy = 1, npt {
		y = dy + d[iy,npt]
		ysq = y ** 2
		p2ysq = ysq / p2sq
		do ix = 1, npt  {
		    wt = w[iy,npt] * w[ix,npt]
		    xy = x[ix] * y
		    denom = 1.0 + alpha * (p1xsq[ix] + p2ysq + xy * par[3])
      		    func = (par[4] - 1.0) / (p1p2 * denom ** par[4])
		    p4fod = par[4] * alpha * func / denom
		    wp4fod = wt * p4fod
		    wf = wt * func
		    profile = profile + wf
		    dhdxc = dhdxc + wp4fod * (2.0 * x[ix] / p1sq +
		        y * par[3])
		    dhdyc = dhdyc + wp4fod * (2. * y / p2sq + x[ix] *
		        par[3])
		    if (ideriv > 0) {
			term[1] = term[1] + (2.0 * wp4fod * p1xsq[ix] - wf) /
			    par[1]
			term[2] = term[2] + (2.0 * wp4fod * p2ysq - wf) /
			    par[2]
			term[3] = term[3] - wp4fod * xy
			term[4] = term[4] + wf * (1.0 / (par[4] - 1.0) -
			    log (denom))
		    }
		}
	    }

	#
	# Penny function which has a gaussian core and lorentzian wings.
	# The lorentzian is elongated along the x and y axes.

	case FCTN_PENNY1:

	    p1sq = par[1] ** 2
	    p2sq = par[2] ** 2
	    onemp3 = 1.0 - par[3]
	    xy = dx * dy
	    if (ideriv > 0)
	        call aclrr (term, 4)

	    rsq = dx ** 2 / p1sq + dy ** 2 / p2sq
	    if (rsq > 1.0e10)
		return (profile)

	    f = 1.0 / (1.0 + rsq)
	    rsq = rsq + xy * par[4]
	    if (rsq < 34.0) {
		e = exp (-0.6931472 * rsq)
		func = par[3] * e + onemp3 * f
	    } else {
		e = 0.0
		func = onemp3 * f
	    }

	    if (func >= 0.046) {
		npt = 4
	    } else if (func >= 0.0022) {
		npt = 3
	    } else if (func >= 0.0001) {
		npt = 2
	    } else if (func >= 1.0e-10) {
		profile = func
		dfby = onemp3 * f ** 2
		deby = 0.6931472 * par[3] * e
		dbyx0 = 2.0 * dx / p1sq
		dbyy0 = 2.0 * dy / p2sq
		dhdxc = deby * (dbyx0 + dy * par[4]) + dfby * dbyx0
		dhdyc = deby * (dbyy0 + dx * par[4]) + dfby * dbyy0
		if (ideriv > 0) {
		    dbyx0 = dbyx0 * dx / par[1]
		    dbyy0 = dbyy0 * dy / par[2]
		    dfby = dfby + deby
		    term[1] = dfby * dbyx0
		    term[2] = dfby * dbyy0
		    term[3] = e - f
		    term[4] = - deby * xy / (0.5 - abs(par[4]))
		}
		return (profile)
	    } else {
		return (profile)
	    }

	    do ix = 1, npt {
		x[ix] = dx + d[ix,npt]
		p1xsq[ix] = x[ix] / p1sq
	    }

	    do iy = 1, npt {
		y = dy + d[iy,npt]
		p2ysq = y / p2sq
		do ix = 1, npt {
		    wt = w[iy,npt] * w[ix,npt]
		    xy = x[ix] * y
		    rsq = p1xsq[ix] * x[ix] + p2ysq * y
		    f = 1.0 / (1.0 + rsq)
		    rsq = rsq + xy * par[4]
		    if (rsq < 34.0) {
		        e = exp (- 0.6931472 * rsq)
		        func = par[3] * e + onemp3 * f
			deby = 0.6931472 * wt * par[3] * e
		    } else {
			e = 0.0
			func = onemp3 * f
			deby = 0.0
		    }
		    profile = profile + wt * func
		    dfby = wt * onemp3 * f ** 2
		    dbyx0 = 2.0 * p1xsq[ix]
		    dbyy0 = 2.0 * p2ysq
		    dhdxc = dhdxc + deby * (dbyx0 + dy * par[4]) + dfby * dbyx0
		    dhdyc = dhdyc + deby * (dbyy0 + dx * par[4]) + dfby * dbyy0
		    if (ideriv > 0) {
			dbyx0 = dbyx0 * dx / par[1]
			dbyy0 = dbyy0 * dy / par[2]
			term[1] = term[1] + (dfby + deby) * dbyx0
			term[2] = term[2] + (dfby + deby) * dbyy0
			term[3] = term[3] + wt * (e - f)
			term[4] = term[4] - deby * xy
		    }
		}
	    }  

	# Penny function which has a gaussian core and lorentzian wings.
	# The gaussian and lorentzian may be tilted in different directions.

	case FCTN_PENNY2:

	    p1sq = par[1] ** 2
	    p2sq = par[2] ** 2
	    onemp3 = 1.0 - par[3]
	    xy = dx * dy
	    if (ideriv > 0)
	        call aclrr (term, 5)

	    rsq = dx ** 2 / p1sq + dy ** 2 / p2sq
	    dfby = rsq + par[5] * xy
	    if (dfby > 1.0e10)
		return (profile)
	    f = 1.0 / (1.0 + dfby)

	    deby = rsq + par[4] * xy
	    if (deby < 34.0)
		e = exp (-0.6931472 * deby)
	    else
		e = 0.0

	    func = par[3] * e + onemp3 * f
	    if (func >= 0.046) {
		npt = 4
	    } else if (func >= 0.0022) {
		npt = 3
	    } else if (func >= 0.0001) {
		npt = 2
	    } else if (func >= 1.0e-10) {
		profile = func
		dfby = onemp3 * f ** 2
		deby = 0.6931472 * par[3] * e
		dbyx0 = 2.0 * dx / p1sq
		dbyy0 = 2.0 * dy / p2sq
		dhdxc = deby * (dbyx0 + dy * par[4]) + dfby * (dbyx0 + dy *
		    par[5])
		dhdyc = deby * (dbyy0 + dx * par[4]) + dfby * (dbyy0 + dx *
		    par[5])
		if (ideriv > 0) {
		    dbyx0 = dbyx0 * dx / par[1]
		    dbyy0 = dbyy0 * dy / par[2]
		    term[5] = -dfby * xy
		    dfby = dfby + deby
		    term[1] = dfby * dbyx0
		    term[2] = dfby * dbyy0
		    term[3] = e - f
		    term[4] = - deby * xy 
		}
		return (profile)
	    } else {
		return (profile)
	    }

	    do ix = 1, npt {
		x[ix] = dx + d[ix,npt]
		p1xsq[ix] = x[ix] / p1sq
	    }

	    do iy = 1, npt {
		y = dy + d[iy,npt]
		p2ysq = y / p2sq
		do ix = 1, npt {
		    wt = w[iy,npt] * w[ix,npt]
		    xy = x[ix] * y
		    rsq = p1xsq[ix] * x[ix] + p2ysq * y
		    f = 1.0 / (1.0 + rsq + par[5] * xy)
		    deby = rsq + par[4] * xy
		    if (deby < 34.0) {
		        e = exp (- 0.6931472 * deby)
		        func = par[3] * e + onemp3 * f
			deby = 0.6931472 * wt * par[3] * e
		    } else {
			e = 0.0
			func = onemp3 * f
			deby = 0.0
		    }
		    profile = profile + wt * func
		    dfby = wt * onemp3 * f ** 2
		    dbyx0 = 2.0 * p1xsq[ix]
		    dbyy0 = 2.0 * p2ysq
		    dhdxc = dhdxc + deby * (dbyx0 + dy * par[4]) + dfby *
		        (dbyx0 + dy * par[5])
		    dhdyc = dhdyc + deby * (dbyy0 + dx * par[4]) + dfby *
		        (dbyy0 + dx * par[5])
		    if (ideriv > 0) {
			dbyx0 = dbyx0 * dx / par[1]
			dbyy0 = dbyy0 * dy / par[2]
			term[1] = term[1] + (dfby + deby) * dbyx0
			term[2] = term[2] + (dfby + deby) * dbyy0
			term[3] = term[3] + wt * (e - f)
			term[4] = term[4] - deby * xy
			term[5] = term[5] - dfby * xy
		    }
		}
	    }  

	# Evaluate the Moffat15 function
	#
	# f = (beta - 1) / (ax * ay * (1 + (x/ax) ** 2 + (y/ay) ** 2 +
	#     (xy * axy)) ** beta)
	#
	#     par[1] is the hwhm in x at y = 0.0
	#     1/2 = 1 / (1 + (par[1] / ax) ** 2) ** beta
	# so
	#     2 ** (1/ beta) - 1 = (par[1] / ax) ** 2
	#     ax ** 2 = par[1] ** 2 / (2 ** (1 / beta) - 1)
	#
	# when   beta = 1.5   ax ** 2 = 1.7024144 * par[1] ** 2
	#
	# f = 1 / par[1] * par[2] * (1 + 0.5874011 * ((x / par[1]) ** 2 +
	#     (y / par[2]) ** 2 + xy * par[3])) ** 2.5
	#

	case FCTN_MOFFAT15:

	    alpha = 0.5874011
	    #talpha = 1.1748021
	    p1sq = par[1] ** 2
	    p2sq = par[2] ** 2
	    p1p2 = par[1] * par[2]
	    xy = dx * dy
	    if (ideriv > 0)
	        call aclrr (term, 4)

	    denom = 1.0 + alpha * (dx ** 2 / p1sq + dy ** 2 / p2sq + xy *
		par[3])
	    if (denom > 5.0e6)
		return (profile)
	    func = 1.0 / (p1p2 * denom ** par[4])
	    if (func >= 0.046) {
		npt = 4
	    } else if (func >= 0.0022) {
		npt = 3
	    } else if (func >= 0.0001) {
		npt = 2
	    } else if (func >= 1.0e-10) {
		profile = (par[4] - 1.0) * func
		p4fod = par[4] * alpha * profile / denom
		dhdxc = p4fod * (2.0 * dx / p1sq + dy * par[3])
		dhdyc = p4fod * (2.0 * dy / p2sq + dx * par[3])
		if (ideriv > 0) {
		    term[1] =  (2.0 * p4fod * dx ** 2 / p1sq - profile) /
		        par[1]
		    term[2] =  (2.0 * p4fod * dy ** 2 / p2sq - profile) /
		        par[2]
		    term[3] = - p4fod * xy
		    term[4] = profile * (1.0 / (par[4] - 1.0) - log (denom))
		}
		return (profile)
	    } else {
		return (profile)
	    }

	    do ix = 1, npt {
		x[ix] = dx + d[ix,npt]
		xsq[ix] = x[ix] ** 2
		p1xsq[ix] = xsq[ix] / p1sq
	    }

	    do iy = 1, npt {
		y = dy + d[iy,npt]
		ysq = y ** 2
		p2ysq = ysq / p2sq
		do ix = 1, npt {
		    wt = w[iy,npt] * w[ix,npt]
		    xy = x[ix] * y
		    denom = 1.0 + alpha * (p1xsq[ix] + p2ysq + xy *
		        par[3])
		    func = (par[4] - 1.0) / (p1p2 * denom ** par[4])
		    p4fod = par[4] * alpha * func / denom
		    wp4fod = wt * p4fod
		    wf = wt * func
		    profile = profile + wf
		    dhdxc = dhdxc + wp4fod * (2.0 * x[ix] / p1sq + y *
		        par[3])
		    dhdyc = dhdyc + wp4fod * (2. * y / p2sq + x[ix] *
		        par[3])
		    if (ideriv > 0) {
			term[1] = term[1] + (2.0 * wp4fod * p1xsq[ix] - wf) /
			    par[1]
			term[2] = term[2] + (2.0 * wp4fod * p2ysq - wf) /
			    par[2]
			term[3] = term[3] - wp4fod * xy
			term[4] = term[4] + wf * (1.0 / (par[4] - 1.0) -
			    log (denom))
      		    }
		}
	    } 

	case FCTN_LORENTZ:

	    p1sq = par[1] ** 2
	    p2sq = par[2] ** 2
	    p1p2 = par[1] * par[2]
	    xy = dx * dy
	    if (ideriv > 0)
	        call aclrr (term, 3)

	    denom = 1.0 + dx ** 2 / p1sq + dy ** 2 / p2sq + xy * par[3]
	    if (denom > 1.0e10)
		return (profile)
	    func = 1.0 / denom
	    if (func >= 0.046) {
		npt = 4
	    } else if (func >= 0.0022) {
		npt = 3
	    } else if (func >= 0.0001) {
		npt = 2
	    } else if (func >= 1.0e-10) {
		profile = func
		wfsq = func ** 2
		dhdxc = wfsq * (2.0 * dx / p1sq + dy * par[3])
		dhdyc = wfsq * (2.0 * dy / p2sq + dx * par[3])
		if (ideriv > 0) {
		    term[1] = wfsq * (2.0 * dx ** 2 / p1sq) / par[1]
		    term[2] = wfsq * (2.0 * dy ** 2 / p2sq) / par[2]
		    term[3] = - wfsq * xy
		}
		return (profile)
	    } else {
		return (profile)
	    }

	    do ix = 1, npt {
		x[ix] = dx + d[ix,npt]
		xsq[ix] = x[ix] ** 2
		p1xsq[ix] = xsq[ix] / p1sq
	    }

	    do iy = 1, npt {
		y = dy + d[iy,npt]
		ysq = y ** 2
		p2ysq = ysq / p2sq
		do ix = 1, npt {
		    wt = w[iy,npt] * w[ix,npt]
		    xy = x[ix] * y
		    denom = 1.0 + p1xsq[ix] + p2ysq + xy * par[3]
		    func = 1.0 / denom
		    wf = wt * func
		    wfsq = wf * func
		    profile = profile + wf
		    dhdxc = dhdxc + wfsq * (2.0 * x[ix] / p1sq + y * par[3])
		    dhdyc = dhdyc + wfsq * (2.0 * y / p2sq + x[ix] * par[3])
		    if (ideriv > 0) {
			term[1] = term[1] + wfsq * (2.0 * p1xsq[ix]) / par[1]
			term[2] = term[2] + wfsq * (2.0 * p2ysq) / par[2]
			term[3] = term[3] - wfsq * xy
		    }
		}
	    }

	default:
	    profile = INDEFR
	}

	return (profile)
end
