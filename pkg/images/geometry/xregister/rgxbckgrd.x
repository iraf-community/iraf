include <mach.h>
include <math/gsurfit.h>
include "xregister.h"


# RG_XSCALE -- Compute the background offset and x and y slope.

procedure rg_xscale (xc, data, npts, nx, ny, offset, coeff)

pointer	xc		#I pointer to the cross-correlation function
real	data[ARB]	#I the input data
int	npts		#I the number of points
int	nx, ny		#I the dimensions of the original subraster
real	offset		#I the input offset
real	coeff[ARB]	#O the output coefficients

int	wborder
pointer	gs
real	loreject, hireject, zero
int	rg_xstati(), rg_znsum(), rg_znmedian(), rg_slope()
real	rg_xstatr()

begin
	loreject = rg_xstatr (xc, LOREJECT)
	hireject = rg_xstatr (xc, HIREJECT)
	wborder = rg_xstati (xc, BORDER)

	switch (rg_xstati (xc, BACKGRD)) {
	case XC_BNONE:
	    coeff[1] = offset
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case XC_MEAN:
	    if (rg_znsum (data, npts, zero, loreject, hireject) <= 0)
		zero = 0.0
	    coeff[1] = zero
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case XC_MEDIAN:
	    if (rg_znmedian (data, npts, zero, loreject, hireject) <= 0)
		zero = 0.0
	    coeff[1] = zero
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	case XC_SLOPE:
	    call gsinit (gs, GS_POLYNOMIAL, 2, 2, NO, 1.0, real (nx), 1.0,
	        real (ny))
	    if (rg_slope (gs, data, npts, nx, ny, wborder, loreject,
	        hireject) == ERR) {
		coeff[1] = 0.0
		coeff[2] = 0.0
		coeff[3] = 0.0
	    } else {
	        call gssave (gs, coeff)
		coeff[1] = coeff[GS_SAVECOEFF+1]
		coeff[2] = coeff[GS_SAVECOEFF+2]
		coeff[3] = coeff[GS_SAVECOEFF+3]
	    }
	    call gsfree (gs)
	default:
	    coeff[1] = offset
	    coeff[2] = 0.0
	    coeff[3] = 0.0
	}
end


# RG_ZNSUM -- Compute the mean and number of good points in the array with
# one optional level of rejections.

int procedure rg_znsum (data, npts, mean, lcut, hcut)

real	data[ARB]	#I the input data array
int	npts		#I the number of data points
real	mean		#O the mean of the data
real	lcut, hcut	#I the good data limits

int	i, ngpts
real	dif, sigma, sum, sumsq, lo, hi
real	asumr(), assqr()

begin
	# Get the mean.
	if (npts == 0) {
	    mean = INDEFR
	    return (0)
	} else if (npts == 1) {
	    mean = data[1]
	    return (1)
	} else {
	    sum = asumr (data, npts)
	    mean = sum / npts
	}

	# Quit if the rejection flags are not set.
	if (IS_INDEFR(lcut) && IS_INDEFR(hcut))
	    return (npts)

	# Compute sigma
	sumsq = assqr (data, npts)
	sigma = sumsq / (npts - 1) - mean * sum / (npts - 1)
	if (sigma <= 0.0)
	    sigma = 0.0
	else
	    sigma = sqrt (sigma)
	if (sigma <= 0.0)
	    return (npts)

	# Do the k-sigma rejection.
	if (IS_INDEF(lcut))
	    lo = -MAX_REAL
	else
	    lo = -lcut * sigma
	if (IS_INDEFR(hcut))
	    hi = MAX_REAL
	else
	    hi = hcut * sigma

	# Reject points.
	ngpts = npts
	do i = 1, npts {
	    dif = (data[i] - mean)
	    if (dif >= lo && dif <= hi)
	        next
	    ngpts = ngpts - 1
	    sum = sum - data[i]
	    sumsq = sumsq - data[i] ** 2
	}

	# Get the final mean.
	if (ngpts == 0) {
	    mean = INDEFR
	    return (0)
	} else if (ngpts == 1) {
	    mean = sum
	    return (1)
	} else
	    mean = sum / ngpts

	return (ngpts)
end


# RG_ZNMEDIAN -- Compute the median and number of good points in the array
# with one level of rejection.

int procedure rg_znmedian (data, npts, median, lcut, hcut)

real	data[ARB]	#I the input data array
int	npts		#I the number of data points
real	median		#O the median of the data
real	lcut, hcut	#I the good data limits

int	i, ngpts, lindex, hindex
pointer	sp, sdata
real	mean, sigma, dif, lo, hi
real	amedr()

begin
	if (IS_INDEFR (lcut) && IS_INDEFR(hcut))  {
	    median = amedr (data, npts)
	    return (npts)
	}

	# Allocate working space.
	call smark (sp)
	call salloc (sdata, npts, TY_REAL)
	call asrtr (data, Memr[sdata], npts)
	if (mod (npts, 2) == 0)
	    median = (Memr[sdata+(1+npts)/2-1] + Memr[sdata+(1+npts)/2]) / 2.0
	else
	    median = Memr[sdata+(1+npts)/2-1]

	# Compute the sigma.
	call aavgr (Memr[sdata], npts, mean, sigma)
	if (sigma <= 0.0) {
	    call sfree (sp)
	    return (npts)
	}

	# Do rejection.
	ngpts = npts
	if (IS_INDEFR(lo))
	    lo = -MAX_REAL
	else
	    lo = -lcut * sigma
	if (IS_INDEFR(hi))
	    hi = MAX_REAL
	else
	    hi = hcut * sigma

	do i = 1, npts {
	    lindex = i
	    dif = Memr[sdata+i-1] - median
	    if (dif >= lo)
		break
	}
	do i = npts, 1, -1 {
	    hindex = i
	    dif = Memr[sdata+i-1] - median
	    if (dif <= hi)
		break
	}

	ngpts = hindex - lindex + 1
	if (ngpts <= 0)
	    median = INDEFR
	else if (mod (ngpts, 2) == 0)
	    median = (Memr[sdata+lindex-1+(ngpts+1)/2-1] + Memr[sdata+lindex-1+
	        (ngpts+1)/2]) / 2.0
	else
	    median = Memr[sdata+lindex-1+(ngpts+1)/2-1]

	call sfree (sp)

	return (ngpts)
end


# RG_SLOPE -- Procedure to subtract a slope from the data to be crosscorrelated.

int procedure rg_slope (gs, data, npts, nx, ny, wborder, loreject, hireject)

pointer	gs			#I the pointer to surfit structure
real	data[ARB]		#I/O the input/output data
int	npts			#I the number of points
int	nx, ny			#I dimensions of the original data
int	wborder			#I the width of the border
real	loreject, hireject	#I the rejection criteria

int	i, stat, ier
pointer	sp, x, y, w, zfit
real	lcut, hcut, sigma
int	rg_xreject(), rg_breject()
real	rg_sigma(), rg_bsigma()

begin
	# Initialize.
	call smark (sp)
	call salloc (x, nx, TY_REAL)
	call salloc (y, nx, TY_REAL)
	call salloc (w, nx, TY_REAL)
	call salloc (zfit, nx, TY_REAL)
	do i = 1, nx
	    Memr[x+i-1] = i
	call amovkr (1.0, Memr[w], nx)

	# Accumulate the fit.
	call gszero (gs)
	if (npts >= nx * ny)
	    call rg_gsaccum (gs, Memr[x], Memr[y], Memr[w], data, nx, ny)
	else
	    call rg_gsborder (gs, Memr[x], Memr[y], Memr[w], data, nx, ny,
		wborder)

	# Solve the surface.
	call gssolve (gs, ier)
	if (ier == NO_DEG_FREEDOM) {
	    call sfree (sp)
	    return (ERR)
	}

	# Perform the rejection.
	if (! IS_INDEFR(loreject) || ! IS_INDEFR(hireject)) {
	    if (npts >= nx * ny)
		sigma = rg_sigma (gs, Memr[x], Memr[y], Memr[w], Memr[zfit],
		    data, nx, ny)
	    else
		sigma = rg_bsigma (gs, Memr[x], Memr[y], Memr[w], Memr[zfit],
		    data, nx, ny, wborder)
	    if (sigma <= 0.0) {
		call sfree (sp)
		return (OK)
	    }
	    if (! IS_INDEFR(loreject))
		lcut  = -loreject * sigma
	    else
		lcut = -MAX_REAL
	    if (! IS_INDEFR(hireject))
		hcut  = hireject * sigma
	    else
		hcut = MAX_REAL
	    if (npts >= nx * ny)
		stat = rg_xreject (gs, Memr[x], Memr[y], Memr[w], Memr[zfit],
		    data, nx, ny, lcut, hcut)
	    else
		stat = rg_breject (gs, Memr[x], Memr[y], Memr[w], Memr[zfit],
		    data, nx, ny, wborder, lcut, hcut)
	}

	call sfree (sp)
	return (stat)
end


# RG_AVGERR -- Procedure to compute the average difference given an array
# and the mean.

int procedure rg_avgerr (a, npts, zero, norm, locut, hicut)

real	a[npts]		#I the input  data
int	npts		#I the number of points
real	zero		#I the average value
real	norm		#O the computed normalization factor
real	locut		#I low side k-sigma rejection criterion
real	hicut		#I high side k-sigma rejection criterion

int	i, ngpts
real	sum, dif, lo, hi

begin
	ngpts = npts
	norm = 0.0

	sum = 0.0
	do i = 1, npts
	    sum = sum + (a[i] - zero) ** 2

	if (npts <= 0) {
	    return (0)
	} else if (locut == hicut) {
	    norm = sqrt (sum / npts)
	    return (npts)
	} else
	    norm = sqrt (sum / npts)

	ngpts = 0
	sum = 0.0
	lo = - locut * norm
	hi = hicut * norm
	do i = 1, npts {
	    dif = a[i] - zero
	    if (dif < -lo || dif > hi)
		next
	    sum = sum + dif ** 2
	    ngpts = ngpts + 1
	}

	if (ngpts <= 0)
	    norm = 0.0
	else
	    norm = sqrt (sum / ngpts)

	return (ngpts)
end


# RG_GSACCUM -- Accumulate the points into the fits assuming the data is in the
# form of a two-dimensional subraster.

procedure rg_gsaccum (gs, x, y, w, data, nx, ny)

pointer	gs		#I pointer to the surface fitting structure
real	x[ARB]		#I the input x array
real	y[ARB]		#I the input y array
real	w[ARB]		#I the input weight array
real	data[ARB]	#I the input data array
int	nx, ny		#I the size of the input data array

int	i, index

begin
	index = 1
	do i = 1, ny {
	    call amovkr (real (i), y, nx)
	    call gsacpts (gs, x, y, data[index], w, nx, WTS_USER)
	    index = index + nx
	}
end


# RG_GSBORDER -- Procedure to accumulate the points into the fit assuming
# that a border has been extracted

procedure rg_gsborder (gs, x, y, w, data, nx, ny, wborder)

pointer	gs		#I pointer to the surface fitting structure
real	x[ARB]		#I the input x array
real	y[ARB]		#I the input y array
real	w[ARB]		#I the input weight array
real	data[ARB]	#I the input data array
int	nx, ny		#I the dimensions of the input data
int	wborder		#I the width of the border

int	i, index, nborder

begin
	nborder = 2 * wborder * (nx + ny - 2 * wborder)

	index = 1
	do i = 1, wborder {
	    call amovkr (real (i), y, nx)
	    call gsacpts (gs, x, y, data[index], w, nx, WTS_USER)
	    index = index + nx
	}

	index = nx * wborder + 1
	do i = wborder + 1, ny - wborder {
	    call amovkr (real (i), y, nx)
	    call gsacpts (gs, x, y, data[index], w, wborder, WTS_USER)
	    index = index + wborder
	    call gsacpts (gs, x[1+nx-wborder], y[1+nx-wborder],
	        data[index], w[1+nx-wborder], wborder, WTS_USER)
	    index = index + wborder
	}

	index = 1 + nborder - nx * wborder
	do i = ny - wborder + 1, ny {
	    call amovkr (real (i), y, nx)
	    call gsacpts (gs, x, y, data[index], w, nx, WTS_USER)
	    index = index + nx
	}

end


# RG_GSVECTOR -- Fit the data assuming the data is in the form of a
# two-dimensional subraster.

procedure rg_gsvector (gs, x, y, zfit, data, nx, ny)

pointer	gs		#I the pointer to the surface fitting structure
real	x[ARB]		#I the input  x array
real	y[ARB]		#I the input y array
real	zfit[ARB]	#O the output fitted data
real	data[ARB]	#O the output data array
int	nx, ny		#I the size of the output data

int	i, index

begin
	index = 1
	do i = 1, ny {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, data[index], nx)
	    index = index + nx
	}
end


# RG_BGSVECTOR -- Fit the data assuming a border has been extracted from a
# subraster.

procedure rg_bgsvector (gs, x, y, zfit, data, nx, ny, wborder)

pointer	gs		#I the pointer to the surface fitting structure
real	x[ARB]		#I the input x array
real	y[ARB]		#I the input y array
real	zfit[ARB]	#O the fitted z array
real	data[ARB]	#I/O the input/output data array
int	nx, ny		#I dimensions of original subraster
int	wborder		#I the width of the border

int	i, index, nborder

begin
	nborder = 2 * wborder * (nx + ny - 2 * wborder)

	index = 1
	do i = 1, wborder {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, data[index], nx)
	    index = index + nx
	}

	index = 1 + nx * wborder
	do i = wborder + 1, ny - wborder {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, wborder)
	    call asubr (data[index], zfit, data[index], wborder)
	    index = index + wborder
	    call gsvector (gs, x[1+nx-wborder], y[1+nx-wborder], zfit, wborder)
	    call asubr (data[index], zfit, data[index], wborder)
	    index = index + wborder
	}

	index = 1 + nborder - nx * wborder
	do i = ny - wborder + 1, ny {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, data[index], nx)
	    index = index + nx
	}

end


# RG_SIGMA -- Compute sigma assuming the data is in the form of a
# two-dimensional subraster.

real procedure rg_sigma (gs, x, y, w, zfit, data, nx, ny)

pointer	gs		#I the pointer to the surface fitting structure
real	x[ARB]		#I the input x array
real	y[ARB]		#I the input y array
real	w[ARB]		#I the input w array
real	zfit[ARB]	#O the output fitted data
real	data[ARB]	#I/O the input/output data array
int	nx, ny		#I the dimensions of the output data

int	i, j, index, npts
real	sum

begin
	npts = 0
	index = 1
	sum = 0.0

	do i = 1, ny {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, zfit, nx)
	    do j = 1, nx {
		if (w[j] > 0.0) {
		    sum = sum + zfit[j] ** 2
		    npts = npts + 1
		}
	    }
	    index = index + nx
	}

	return (sqrt (sum / npts))
end


# RG_BSIGMA -- Procedure to compute sigma assuming a border has been
# extracted from a subraster.

real procedure rg_bsigma (gs, x, y, w, zfit, data, nx, ny, wborder)

pointer	gs		#I the pointer to the surface fitting structure
real	x[ARB]		#I the input x array
real	y[ARB]		#I the output y array
real	w[ARB]		#I the output weight array
real	zfit[ARB]	#O the fitted z array
real	data[ARB]	#I/O the input/output data array
int	nx, ny		#I the dimensions of original subraster
int	wborder		#I the width of the border

int	i, j, npts, nborder, index
real	sum

begin
	nborder = 2 * wborder * (nx + ny - 2 * wborder)
	npts = 0
	index = 1
	sum = 0.0

	do i = 1, wborder {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, zfit, nx)
	    do j = 1, nx {
		if (w[j] > 0.0) {
		    npts = npts + 1
		    sum = sum + zfit[j] ** 2
		}
	    }
	    index = index + nx
	}

	index = nx * wborder + 1
	do i = wborder + 1, ny - wborder {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, wborder)
	    call asubr (data[index], zfit, zfit, wborder)
	    do j = 1, wborder {
		if (w[j] > 0.0) {
		    npts = npts + 1
		    sum = sum + zfit[j] ** 2
		}
	    }
	    index = index + wborder
	    call gsvector (gs, x[1+nx-wborder], y[1+nx-wborder], zfit, wborder)
	    call asubr (data[index], zfit, zfit, wborder)
	    do j = 1, wborder {
		if (w[j] > 0.0) {
		    npts = npts + 1
		    sum = sum + zfit[j] ** 2
		}
	    }
	    index = index + wborder
	}

	index = 1 + nborder - nx * wborder
	do i = ny - wborder + 1, ny {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, zfit, nx)
	    do j = 1, nx {
		if (w[j] > 0.0) {
		    npts = npts + 1
		    sum = sum + zfit[j] ** 2
		}
	    }
	    index = index + nx
	}

	return (sqrt (sum / npts))
end


# RG_REJECT -- Reject points from the fit assuming the data is in the form of a
# two-dimensional subraster.

int procedure rg_xreject (gs, x, y, w, zfit, data, nx, ny, lcut, hcut)

pointer	gs		#I the pointer to the surface fitting structure
real	x[ARB]		#I the input x array
real	y[ARB]		#I the input y array
real	w[ARB]		#I the input w array
real	zfit[ARB]	#O the fitted data
real	data[ARB]	#I/O the input/output data array
int	nx, ny		#I the dimensions of the data
real	lcut, hcut	#I the lo and high side rejection criteria

int	i, j, index, ier

begin
	index = 1

	do i = 1, ny {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, zfit, nx)
	    do j = 1, nx {
		if (zfit[j] < lcut || zfit[j] > hcut)
		    call gsrej (gs, x[j], y[j], data[index+j-1], w[j], WTS_USER)
	    }
	    index = index + nx
	}

	call gssolve (gs, ier)
	if (ier == NO_DEG_FREEDOM)
	    return (ERR)
	else
	    return (OK)
end


# RG_BREJECT -- Reject deviant points from the fits assuming a border has
# been extracted from the subraster.

int procedure rg_breject (gs, x, y, w, zfit, data, nx, ny, wborder, lcut, hcut)

pointer	gs		#I the pointer to the surface fitting structure
real	x[ARB]		#I the input x array
real	y[ARB]		#I the input y array
real	w[ARB]		#I the input weight array
real	zfit[ARB]	#O the fitted z array
real	data[ARB]	#I/O the input/output data array
int	nx, ny		#I the dimensions of the original subraster
int	wborder		#I the width of the border
real	lcut, hcut	#I the low and high rejection criteria

int	i, j, nborder, index, ier

begin
	nborder = 2 * wborder * (nx + ny - 2 * wborder)
	index = 1

	do i = 1, wborder {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, zfit, nx)
	    do j = 1, nx {
		if (zfit[j] < lcut || zfit[j] > hcut)
		    call gsrej (gs, x[j], y[j], data[index+j-1], w[j],
		        WTS_USER)
	    }
	    index = index + nx
	}

	index = nx * wborder + 1
	do i = wborder + 1, ny - wborder {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, wborder)
	    call asubr (data[index], zfit, zfit, wborder)
	    do j = 1, wborder {
		if (zfit[j] < lcut || zfit[j] > hcut) 
		    call gsrej (gs, x[j], y[j], data[index+j-1], w[j],
		        WTS_USER)
	    }
	    index = index + wborder
	    call gsvector (gs, x[1+nx-wborder], y[1+nx-wborder], zfit, wborder)
	    call asubr (data[index], zfit, zfit, wborder)
	    do j = 1, wborder {
		if (zfit[j] < lcut || zfit[j] > hcut)
		    call gsrej (gs, x[j], y[j], data[index+j-1], w[j],
		        WTS_USER)
	    }
	    index = index + wborder
	}

	index = 1 + nborder - nx * wborder
	do i = ny - wborder + 1, ny {
	    call amovkr (real (i), y, nx)
	    call gsvector (gs, x, y, zfit, nx)
	    call asubr (data[index], zfit, zfit, nx)
	    do j = 1, nx {
		if (zfit[j] < lcut || zfit[j] > hcut)
		    call gsrej (gs, x[j], y[j], data[index+j-1], w[j], 
		        WTS_USER)
	    }
	    index = index + nx
	}

	call gssolve (gs, ier)
	if (ier == NO_DEG_FREEDOM)
	    return (ERR)
	else
	    return (OK)
end


# RG_BORDER -- Fetch the border pixels from a 2D subraster.

int procedure rg_border (buf, nx, ny, wborder, ptr)

real	buf[nx,ARB]	#I the input data subraster
int	nx, ny		#I the dimensions of the input subraster
int	wborder		#I the width of the border
pointer	ptr		#I the pointer to the output buffer
	
int	j, index, nborder

begin
	# Compute the size of the array
	nborder = 2 * wborder * ( nx + ny - 2 * wborder)
	if (nborder <= 0) {
	    ptr = NULL
	    return (0)
	} else if (nborder >= nx * ny) {
	    call malloc (ptr, nx * ny, TY_REAL)
	    call amovr (buf, Memr[ptr], nx * ny)
	    return (nx * ny)
	} else
	    call malloc (ptr, nborder, TY_REAL)

	# Fill the array.
	index = ptr
	do j = 1, wborder {
	    call amovr (buf[1,j], Memr[index], nx)
	    index = index + nx
	}
	do j = wborder + 1, ny - wborder {
	    call amovr (buf[1,j], Memr[index], wborder) 
	    index = index + wborder
	    call amovr (buf[nx-wborder+1,j], Memr[index], wborder)
	    index = index + wborder
	}
	do j = ny - wborder + 1, ny {
	    call amovr (buf[1,j], Memr[index], nx)
	    index = index + nx
	}

	return (nborder)
end


# RG_SUBTRACT -- Subtract a plane from the data.

procedure rg_subtract (data, nx, ny, zero, xslope, yslope)

real	data[nx,ARB]		#I/O the input/output data array
int	nx, ny			#I the dimensions of the input data array
real	zero			#I the input zero point
real	xslope			#I the input x slope
real	yslope			#I the input y slope

int	i, j
real	ydelta

begin
	do j = 1, ny {
	    ydelta = yslope * j
	    do i = 1, nx
		data[i,j] = data[i,j] - zero - xslope * i - ydelta
	}
end
