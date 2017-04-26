include <mach.h>
include <math/iminterp.h>
include <math/nlfit.h>
include "xregister.h"

define	NL_MAXITER		10
define	NL_TOL			0.001

# RG_FIT -- Fit the peak of the cross-correlation function using one of the
# fitting functions.

procedure rg_fit (xc, nreg, gd, xshift, yshift)

pointer	xc		    #I the pointer to the cross-corrrelation structure
int	nreg		    #I the current region
pointer	gd		    #I the pointer to the graphics stream
real	xshift, yshift	    #O the computed shifts

int	nrlines, xwindow, ywindow, xcbox, ycbox, xlag, ylag
real	xin, yin, xout, yout
int	rg_xstati()
pointer	rg_xstatp()

begin
	# Check the window and centering box sizes.
	nrlines = Memi[rg_xstatp(xc,RL2)+nreg-1] -
	    Memi[rg_xstatp(xc,RL1)+nreg-1] + 1
	xwindow = rg_xstati (xc, XWINDOW)
	if (nrlines == 1)
	    ywindow = 1
	else
	    ywindow = rg_xstati (xc, YWINDOW)
	xcbox = rg_xstati (xc, XCBOX)
	if (nrlines == 1)
	    ycbox = 1
	else
	    ycbox = rg_xstati (xc, YCBOX)

	# Do the centering.
	switch (rg_xstati (xc, PFUNC)) {
	case XC_PNONE:
	    call rg_maxmin (Memr[rg_xstatp(xc,XCOR)], xwindow, ywindow,
	        xshift, yshift)
	case XC_CENTROID:
	    call rg_imean (Memr[rg_xstatp(xc,XCOR)], xwindow,
	        ywindow, xcbox, ycbox, xshift, yshift)
	case XC_SAWTOOTH:
	    call rg_sawtooth (Memr[rg_xstatp(xc,XCOR)], xwindow,
	        ywindow, xcbox, ycbox, xshift, yshift)
	case XC_PARABOLA:
	    call rg_iparabolic (Memr[rg_xstatp(xc,XCOR)], xwindow, ywindow, 
		xcbox, ycbox, xshift, yshift)
	case XC_MARK:
	    if (gd == NULL)
	        call rg_imean (Memr[rg_xstatp(xc,XCOR)], xwindow,
	            ywindow, xcbox, ycbox, xshift, yshift)
	    else
	        call rg_xmkpeak (gd, xwindow, ywindow, xshift, yshift)
	default:
	    call rg_imean (Memr[rg_xstatp(xc,XCOR)], xwindow, ywindow,
	        xcbox, ycbox, xshift, yshift)
	}

	# Store the shifts.
	if (rg_xstati (xc, NREFPTS) > 0) {
	    xin  = (Memi[rg_xstatp(xc,RC1)+nreg-1] +
	        Memi[rg_xstatp(xc,RC2)+nreg-1]) / 2.0
	    yin  = (Memi[rg_xstatp(xc,RL1)+nreg-1] +
	        Memi[rg_xstatp(xc,RL2)+nreg-1]) / 2.0
	    call rg_etransform (xc, xin, yin, xout, yout)
	    xlag = xout - xin
	    ylag = yout - yin
	} else {
	    xlag = rg_xstati (xc, XLAG)
	    ylag = rg_xstati (xc, YLAG)
	}
	xshift = - (xshift + xlag)
	yshift = - (yshift + ylag)
	Memr[rg_xstatp(xc,XSHIFTS)+nreg-1] = xshift
	Memr[rg_xstatp(xc,YSHIFTS)+nreg-1] = yshift
end


# RG_MAXMIN -- Procedure to compute the peak of the cross-correlation function
# by determining the maximum point.

procedure rg_maxmin (xcor, xwindow, ywindow, xshift, yshift)

real	xcor[xwindow,ywindow]	#I the cross-correlation function
int	xwindow, ywindow	#I dimensions of cross-correlation function
real	xshift, yshift		#O x and shift of the peak

int	xindex, yindex

begin
	# Locate the maximum point.
	call rg_alim2r (xcor, xwindow, ywindow, xindex, yindex)
	xshift = xindex - (1.0 + xwindow) / 2.0
	yshift = yindex - (1.0 + ywindow) / 2.0
end


# RG_IMEAN -- Compute the peak of the cross-correlation function using the
# intensity weighted mean of the marginal distributions in x and y.

procedure rg_imean (xcor, xwindow, ywindow, xcbox, ycbox, xshift, yshift)

real	xcor[xwindow,ARB]	#I the cross-correlation function
int	xwindow, ywindow	#I dimensions of the cross-correlation function
int	xcbox, ycbox		#I dimensions of the centering box
real	xshift, yshift		#O x and y shift of cross-correlation function

int	xindex, yindex, xlo, xhi, ylo, yhi, nx, ny
pointer	sp, xmarg, ymarg

begin
	call smark (sp)
	call salloc (xmarg, xcbox, TY_REAL)
	call salloc (ymarg, ycbox, TY_REAL)

	# Locate the maximum point and normalize.
	call rg_alim2r (xcor, xwindow, ywindow, xindex, yindex)

	# Compute the limits of the centering box.
	xlo = max (1, xindex - xcbox / 2)
	xhi = min (xwindow, xindex + xcbox / 2)
	nx = xhi - xlo + 1
	ylo = max (1, yindex - ycbox / 2)
	yhi = min (ywindow, yindex + ycbox / 2)
	ny = yhi - ylo + 1

	# Accumulate the marginals.
	call rg_xmkmarg (xcor, xwindow, ywindow, xlo, xhi, ylo, yhi,
	    Memr[xmarg], Memr[ymarg])

	# Compute the shifts.
	call rg_centroid (Memr[xmarg], nx, xshift)
	xshift = xshift + xlo - 1 - (1.0 + xwindow) / 2.0
	call rg_centroid (Memr[ymarg], ny, yshift)
	yshift = yshift + ylo - 1 - (1.0 + ywindow) / 2.0

	call sfree (sp)
end


# RG_IPARABOLIC -- Computer the peak of the cross-correlation function by
# doing parabolic interpolation around the peak.

procedure rg_iparabolic (xcor, xwindow, ywindow, xcbox, ycbox, xshift, yshift)

real	xcor[xwindow,ARB]	#I the cross-correlation function
int	xwindow, ywindow	#I dimensions of the cross-correlation fucntion
int	xcbox, ycbox		#I the dimensions of the centering box
real	xshift, yshift		#O the x and y shift of the peak

int	i, j, xindex, yindex, xlo, xhi, nx, ylo, yhi, ny
pointer	sp, x, y, c, xfit, yfit

begin
	# Allocate working space.
	call smark (sp)
	call salloc (x, 3, TY_REAL)
	call salloc (y, 3, TY_REAL)
	call salloc (c, 3, TY_REAL)
	call salloc (xfit, 3, TY_REAL)
	call salloc (yfit, 3, TY_REAL)

	# Locate the maximum point.
	call rg_alim2r (xcor, xwindow, ywindow, xindex, yindex)

	xlo = max (1, xindex - 1)
	xhi = min (xwindow, xindex + 1)
	nx = xhi - xlo + 1
	ylo = max (1, yindex - 1)
	yhi = min (ywindow, yindex + 1)
	ny = yhi - ylo + 1

	 # Initialize.
	 do i = 1, 3
	    Memr[x+i-1] = i

	# Fit the x shift.
	if (nx >= 3) {
	    do j = ylo, yhi {
		 do i = xlo, xhi
		    Memr[y+i-xlo] = xcor[i,j]
		 call rg_iparab (Memr[x], Memr[y], Memr[c])
		 Memr[xfit+j-ylo] = - Memr[c+1] / (2.0 * Memr[c+2])
		 Memr[yfit+j-ylo] = Memr[c] + Memr[c+1] * Memr[xfit+j-ylo] +
		     Memr[c+2] * Memr[xfit+j-ylo] ** 2 
	    }
	    if (ny >= 3)
	        call rg_iparab (Memr[xfit], Memr[yfit], Memr[c])
	    xshift = - Memr[c+1] / (2.0 * Memr[c+2])
	} else 
	    xshift = xindex - xlo + 1 

	# Fit the y shift.
	if (ny >= 3) {
	    do i = xlo, xhi {
		do j = ylo, yhi
		    Memr[y+j-ylo] = xcor[i,j]
		call rg_iparab (Memr[x], Memr[y], Memr[c])
		Memr[xfit+i-xlo] = - Memr[c+1] / (2.0 * Memr[c+2])
		Memr[yfit+i-xlo] = Memr[c] + Memr[c+1] * Memr[xfit+i-xlo] +
		    Memr[c+2] * Memr[xfit+i-xlo] ** 2 
	    }
	    call rg_iparab (Memr[xfit], Memr[yfit], Memr[c])
	    yshift = - Memr[c+1] / (2.0 * Memr[c+2])
	} else 
	    yshift = yindex - ylo + 1 

	xshift = xshift + xlo - 1 - (1.0 + xwindow) / 2.0
	yshift = yshift + ylo - 1 - (1.0 + ywindow) / 2.0

	call sfree (sp)
end


define	NPARS_PARABOLA	3

# RG_PARABOLIC -- Compute the peak of the cross-correlation function by fitting
# a parabola to the peak.

procedure rg_parabolic (xcor, xwindow, ywindow, xcbox, ycbox, xshift, yshift)

real	xcor[xwindow,ARB]	#I the cross-correlation function
int	xwindow, ywindow	#I dimensions of the cross-correlation fucntion
int	xcbox, ycbox		#I the dimensions of the centering box
real	xshift, yshift		#O the x and y shift of the peak

extern	rg_polyfit, rg_dpolyfit
int	i,  xindex, yindex, xlo, xhi, ylo, yhi, nx, ny, npar, ier
pointer	sp, x, w, xmarg, ymarg, params, eparams, list, nl
int	locpr()

begin
	call smark (sp)
	call salloc (x, max (xwindow, ywindow), TY_REAL)
	call salloc (w, max (xwindow, ywindow), TY_REAL)
	call salloc (xmarg, max (xwindow, ywindow), TY_REAL)
	call salloc (ymarg, max (xwindow, ywindow), TY_REAL)
	call salloc (params, NPARS_PARABOLA, TY_REAL)
	call salloc (eparams, NPARS_PARABOLA, TY_REAL)
	call salloc (list, NPARS_PARABOLA, TY_INT)

	# Locate the maximum point.
	call rg_alim2r (xcor, xwindow, ywindow, xindex, yindex)

	xlo = max (1, xindex - xcbox / 2)
	xhi = min (xwindow, xindex + xcbox / 2)
	nx = xhi - xlo + 1
	ylo = max (1, yindex - ycbox / 2)
	yhi = min (ywindow, yindex + ycbox / 2)
	ny = yhi - ylo + 1

	# Accumulate the marginals.
	call rg_xmkmarg (xcor, xwindow, ywindow, xlo, xhi, ylo, yhi,
	    Memr[xmarg], Memr[ymarg])

	# Compute the x shift.
	if (nx >= 3) {
	    do i = 1, nx
		Memr[x+i-1] = i
	    do i = 1, nx
		Memr[w+i-1] = Memr[xmarg+i-1]
	    call rg_iparab (Memr[x+xindex-xlo-1], Memr[xmarg+xindex-xlo-1],
		Memr[params])
		    xshift = - Memr[params+1] / (2.0 * Memr[params+2]) 
		    call eprintf ("\txshift=%g\n")
			call pargr (xshift)
	    call aclrr (Memr[eparams], NPARS_PARABOLA)
	    do i = 1, NPARS_PARABOLA
		Memi[list+i-1] = i
	    call nlinitr (nl, locpr (rg_polyfit), locpr (rg_dpolyfit),
	        Memr[params], Memr[eparams], NPARS_PARABOLA, Memi[list],
		    NPARS_PARABOLA, .0001, NL_MAXITER)
	    call nlfitr (nl, Memr[x], Memr[xmarg], Memr[w], nx, 1, WTS_USER,
		ier)
	    call nlvectorr (nl, Memr[x], Memr[w], nx, 1)
	    do i = 1, nx {
		call eprintf ("x=%g y=%g yfit=%g\n")
		    call pargr (Memr[x+i-1])
		    call pargr (Memr[xmarg+i-1])
		    call pargr (Memr[w+i-1])
	    }
	    if (ier != NO_DEG_FREEDOM) {
		call nlpgetr (nl, Memr[params], npar)
		if (Memr[params+2] != 0)
		    xshift = - Memr[params+1] / (2.0 * Memr[params+2])
		else
	            xshift = xindex - xlo + 1 
	    } else
	        xshift = xindex - xlo + 1 
	    call nlfreer (nl)
	} else
	    xshift = xindex - xlo + 1 

	# Compute the y shift.
	if (ny >= 3) {
	    do i = 1, ny
		Memr[x+i-1] = i
	    do i = 1, ny
		Memr[w+i-1] = Memr[ymarg+i-1]
	    call rg_iparab (Memr[x+yindex-ylo-1], Memr[ymarg+yindex-ylo-1],
		Memr[params])
		    yshift = - Memr[params+1] / (2.0 * Memr[params+2]) 
		    call eprintf ("\tyshift=%g\n")
			call pargr (yshift)
	    call aclrr (Memr[eparams], NPARS_PARABOLA)
	    do i = 1, NPARS_PARABOLA
		Memi[list+i-1] = i
	    call nlinitr (nl, locpr (rg_polyfit), locpr (rg_dpolyfit),
	        Memr[params], Memr[eparams], NPARS_PARABOLA, Memi[list],
		    NPARS_PARABOLA, 0.0001, NL_MAXITER)
	    call nlfitr (nl, Memr[x], Memr[ymarg], Memr[w], ny, 1, WTS_USER,
		ier)
	    call nlvectorr (nl, Memr[x], Memr[w], ny, 1)
	    do i = 1, ny {
		call eprintf ("x=%g y=%g yfit=%g\n")
		    call pargr (Memr[x+i-1])
		    call pargr (Memr[ymarg+i-1])
		    call pargr (Memr[w+i-1])
	    }
	    if (ier != NO_DEG_FREEDOM) {
		call nlpgetr (nl, Memr[params], npar)
		if (Memr[params+2] != 0)
		    yshift = -Memr[params+1] / (2.0 * Memr[params+2])
		else
	            yshift = yindex - ylo + 1 
	    } else
	        yshift = yindex - ylo + 1 
	    call nlfreer (nl)
	} else
	    yshift = yindex - ylo + 1 

	xshift = xshift + xlo - 1 - (1.0 + xwindow) / 2.0
	yshift = yshift + ylo - 1 - (1.0 + ywindow) / 2.0

	call sfree (sp)
end

define	EMISSION	1		# emission features
define	ABSORPTION	2		# emission features

# RG_SAWTOOTH -- Compute the the x and y centers using a sawtooth
# convolution function.

procedure rg_sawtooth (xcor, xwindow, ywindow, xcbox, ycbox, xshift, yshift)

real	xcor[xwindow,ARB]	#I the cross-correlation function
int	xwindow, ywindow	#I the dimensions of the cross-correlation
int	xcbox, ycbox		#I the dimensions of the centering box
real	xshift, yshift		#O the x and y shifts

int	i, j, xindex, yindex, xlo, xhi, ylo, yhi, nx, ny
pointer	sp, data, xfit, yfit, yclean
real	ic

begin
	call smark (sp)
	call salloc (data, max (xwindow, ywindow), TY_REAL)
	call salloc (xfit, max (xwindow, ywindow), TY_REAL)
	call salloc (yfit, max (xwindow, ywindow), TY_REAL)
	call salloc (yclean, max (xwindow, ywindow), TY_REAL)

	# Locate the maximum point and normalize.
	call rg_alim2r (xcor, xwindow, ywindow, xindex, yindex)

	xlo = max (1, xindex - xcbox)
	xhi = min (xwindow, xindex + xcbox)
	nx = xhi - xlo + 1
	ylo = max (1, yindex - ycbox)
	yhi = min (ywindow, yindex + ycbox)
	ny = yhi - ylo + 1

	# Compute the y shift.
	if (ny >= 3) {
	    do j = ylo, yhi {
	        do i = xlo, xhi
		    Memr[data+i-xlo] = xcor[i,j]
	        call rg_x1dcenter (real (xindex - xlo + 1), Memr[data], nx,
	            Memr[xfit+j-ylo], Memr[yfit+j-ylo], real (nx / 2.0),
		    EMISSION, real (nx / 2.0), 0.0)
	    }
	    call arbpix (Memr[yfit], Memr[yclean], ny, II_SPLINE3,
	        II_BOUNDARYEXT) 
	    call rg_x1dcenter (real (yindex - ylo + 1), Memr[yclean], ny,
	        yshift, ic, real (ny / 2.0), EMISSION, real (ny / 2.0), 0.0)
	    if (IS_INDEFR(yshift))
	        yshift = yindex - ylo + 1
	} else 
	    yshift = yindex - ylo + 1
	yshift = yshift + ylo - 1 - (1.0 + ywindow) / 2.0

	# Compute the x shift.
	if (nx >= 3) {
	    if (ny >= 3) {
	        do i = xlo, xhi {
	            do j = ylo, yhi
		        Memr[data+j-ylo] = xcor[i,j]
	            call rg_x1dcenter (real (yindex - ylo + 1), Memr[data], ny,
	                Memr[xfit+i-xlo], Memr[yfit+i-xlo], real (ny / 2.0),
		        EMISSION, real (ny / 2.0), 0.0)
	        }
	        call arbpix (Memr[yfit], Memr[yclean], nx, II_SPLINE3,
	            II_BOUNDARYEXT) 
	        call rg_x1dcenter (real (xindex - xlo + 1), Memr[yclean], nx,
	            xshift, ic, real (nx / 2.0), EMISSION, real (nx / 2.0), 0.0)
	    } else {
		call rg_x1dcenter (real (xindex - xlo + 1), xcor[xlo,1], nx,
		    xshift, ic, real (nx / 2.0), EMISSION, real (nx / 2.0), 0.0)
	    }
	    if (IS_INDEFR(xshift))
	        xshift = xindex - xlo + 1
	} else
	    xshift = xindex - xlo + 1
	xshift = xshift + xlo - 1 - (1.0 + xwindow) / 2.0

	call sfree (sp)
end


# RG_ALIM2R -- Determine the pixel position of the data maximum.

procedure rg_alim2r (data, nx, ny, i, j)

real	data[nx,ARB]		#I the input data
int	nx, ny			#I the dimensions of the input array
int	i, j			#O the indices of the maximum pixel

int	ii, jj
real	datamax

begin
	datamax = -MAX_REAL
	do jj = 1, ny {
	    do ii = 1, nx {
		if (data[ii,jj] > datamax) {
		    datamax = data[ii,jj]
		    i = ii
		    j = jj
		}
	    }
	}
end


# RG_XMKMARG -- Acumulate the marginal arrays in x and y.

procedure rg_xmkmarg (xcor, xwindow, ywindow, xlo, xhi, ylo, yhi, xmarg,
	ymarg)

real	xcor[xwindow,ARB]	#I the cross-correlation function
int	xwindow, ywindow	#I dimensions of cross-correlation function
int	xlo, xhi		#I the x limits for centering
int	ylo, yhi		#I the y limits for centering
real	xmarg[ARB]		#O the output x marginal array
real	ymarg[ARB]		#O the output y marginal array

int	i, j, index, nx, ny

begin
	nx = xhi - xlo + 1
	ny = yhi - ylo + 1

	# Compute the x marginal.
	index = 1 - xlo
	do i = xlo, xhi {
	    xmarg[index+i] = 0.0
	    do j = ylo, yhi
		xmarg[index+i] = xmarg[index+i] + xcor[i,j]
	}

	# Normalize the x marginal.
	call adivkr (xmarg, real (ny), xmarg, nx)

	# Compute the y marginal.
	index = 1 - ylo
	do j = ylo, yhi {
	    ymarg[index+j] = 0.0
	    do i = xlo, xhi
		ymarg[index+j] = ymarg[index+j] + xcor[i,j]
	}

	# Normalize the ymarginal.
	call adivkr (ymarg, real (nx), ymarg, ny)
end


# RG_CENTROID -- Compute the intensity weighted maximum of an array.

procedure rg_centroid (a, npts, shift)

real	a[ARB]		#I the input array
int	npts		#I the number of points
real	shift		#O the position of the maximum

int	i
real	mean, dif, sumi, sumix
bool	fp_equalr()
real	asumr()

begin
	sumi = 0.0
	sumix = 0.0
	mean = asumr (a, npts) / npts

	do i = 1, npts {
	    dif = a[i]
	    dif = a[i] - mean
	    if (dif < 0.0)
		next
	    sumi = sumi + dif
	    sumix = sumix + i * dif
	}

	if (fp_equalr (sumi, 0.0))
	    shift = (1.0 + npts) / 2.0
	else
	    shift = sumix / sumi
end


define	MIN_WIDTH	3.		# minimum centering width
define	EPSILON		0.001		# accuracy of centering
define	EPSILON1	0.005		# tolerance for convergence check
define	ITERATIONS	100		# maximum number of iterations
define	MAX_DXCHECK	3		# look back for failed convergence
define	INTERPTYPE	II_SPLINE3	# image interpolation type


# RG_X1DCENTER -- Locate the center of a one dimensional feature.
# A value of INDEF is returned in the centering fails for any reason.
# This procedure just sets up the data and adjusts for emission or
# absorption features.  The actual centering is done by C1D_CENTER.

procedure rg_x1dcenter (x, data, npts, xc, ic, width, type, radius, threshold)

real	x				#I initial guess
real	data[npts]			#I data points
int	npts				#I number of data points
real	xc				#O computed center
real	ic				#O intensity at computed center
real	width				#I feature width
int	type				#I feature type
real	radius				#I centering radius
real	threshold			#I minimum range in feature

int	x1, x2, nx
real	a, b, rad, wid
pointer	sp, data1

begin
	# Check starting value.
	if (IS_INDEF(x) || (x < 1) || (x > npts)) {
	    xc = INDEF
	    ic = INDEF
	    return
	}

	# Set minimum width and error radius.  The minimum in the error radius
	# is for defining the data window.  The user error radius is used to
	# check for an error in the derived center at the end of the centering.

	wid = max (width, MIN_WIDTH)
	rad = max (2., radius)

	# Determine the pixel value range around the initial center, including
	# the width and error radius buffer.  Check for a minimum range.

	x1 = max (1., x - wid / 2 - rad - wid)
	x2 = min (real (npts), x + wid / 2 + rad + wid + 1)
	nx = x2 - x1 + 1
	call alimr (data[x1], nx, a, b)
	if (b - a < threshold) {
	    xc = INDEF
	    ic = INDEF
	    return
	}

	# Allocate memory for the continuum subtracted data vector.  The X
	# range is just large enough to include the error radius and the
	# half width.

	x1 = max (1., x - wid / 2 - rad)
	x2 = min (real (npts), x + wid / 2 + rad + 1)
	nx = x2 - x1 + 1

	call smark (sp)
	call salloc (data1, nx, TY_REAL)
	call amovr (data[x1], Memr[data1], nx)

	# Make the centering data positive, subtract the continuum, and
	# apply a threshold to eliminate noise spikes.

	switch (type) {
	case EMISSION:
	    a = 0.
	    call asubkr (data[x1], a + threshold, Memr[data1], nx)
	    call amaxkr (Memr[data1], 0., Memr[data1], nx)
	case ABSORPTION:
	    call anegr (data[x1], Memr[data1], nx)
	    call asubkr (Memr[data1], threshold - b, Memr[data1], nx)
	    call amaxkr (Memr[data1], 0., Memr[data1], nx)
	default:
	    call error (0, "Unknown feature type")
	}

	# Determine the center.
	call rg_xcenter (x - x1 + 1, Memr[data1], nx, xc, ic, wid)

	# Check user centering error radius.
	if (!IS_INDEF(xc)) {
	    xc = xc + x1 - 1
	    if (abs (x - xc) > radius) {
		xc = INDEF
		ic = INDEF
	    }
	}

	# Free memory and return the center position.
	call sfree (sp)
end


# RG_XCENTER -- One dimensional centering algorithm.

procedure rg_xcenter (x, data, npts, xc, ic, width)

real	x				#I starting guess
int	npts				#I number of points in data vector
real	data[npts]			#I data vector
real	xc				#O computed xc
real	ic				#O computed intensity at xc
real	width				#I centering width

int	i, j, iteration, dxcheck
real	hwidth, dx, dxabs, dxlast
real	a, b, sum1, sum2, intgrl1, intgrl2
pointer	asi1, asi2, sp, data1

real	asigrl(), asieval()

define	done_	99

begin
	# Find the nearest local maxima as the starting point.
	# This is required because the threshold limit may have set
	# large regions of the data to zero and without a gradient
	# the centering will fail.

	i = x
	for (i=x+.5; (i<npts) && (data[i]<=data[i+1]); i=i+1)
	    ;
	for (j=x+.5; (j>1) && (data[j]<=data[j-1]); j=j-1)
	    ;

	if (i-x < x-j)
	    xc = i 
	else
	    xc = j

	# Check data range.
	hwidth = width / 2
	if ((xc - hwidth < 1) || (xc + hwidth > npts)) {
	    xc = INDEF
	    ic = INDEF
	    return
	}

	# Set interpolation functions.
	call asiinit (asi1, INTERPTYPE)
	call asiinit (asi2, INTERPTYPE)
	call asifit (asi1, data, npts)

	# Allocate, compute, and interpolate the x*y values.
	call smark (sp)
	call salloc (data1, npts, TY_REAL)
	do i = 1, npts
	    Memr[data1+i-1] = data[i] * i
	call asifit (asi2, Memr[data1], npts)
	call sfree (sp)

	# Iterate to find center.  This loop exits when 1) the maximum
	# number of iterations is reached, 2) the delta is less than
	# the required accuracy (criterion for finding a center), 3)
	# there is a problem in the computation, 4) successive steps
	# continue to exceed the minimum delta.

	dxlast = 1.
	do iteration = 1, ITERATIONS {

	    # Triangle centering function.
	    a = xc - hwidth
	    b = xc - hwidth / 2
	    intgrl1 = asigrl (asi1, a, b)
	    intgrl2 = asigrl (asi2, a, b)
	    sum1 = (xc - hwidth) * intgrl1 - intgrl2
	    sum2 = -intgrl1
	    a = b
	    b = xc + hwidth / 2
	    intgrl1 = asigrl (asi1, a, b)
	    intgrl2 = asigrl (asi2, a, b)
	    sum1 = sum1 - xc * intgrl1 + intgrl2
	    sum2 = sum2 + intgrl1
	    a = b
	    b = xc + hwidth
	    intgrl1 = asigrl (asi1, a, b)
	    intgrl2 = asigrl (asi2, a, b)
	    sum1 = sum1 + (xc + hwidth) * intgrl1 - intgrl2
	    sum2 = sum2 - intgrl1

	    # Return no center if sum2 is zero.
	    if (sum2 == 0.)
		break

	    # Limit dx change in one iteration to 1 pixel.
	    dx = max (-1., min (1., sum1 / abs (sum2)))
	    dxabs = abs (dx)
	    xc = xc + dx
	    ic = asieval (asi1, xc)

	    # Check data range.  Return no center if at edge of data.
	    if ((xc - hwidth < 1) || (xc + hwidth > npts))
		break

	    # Convergence tests.
	    if (dxabs < EPSILON)
		goto done_
	    if (dxabs > dxlast + EPSILON1) {
		dxcheck = dxcheck + 1
		if (dxcheck > MAX_DXCHECK)
		    break
	    } else {
		dxcheck = 0
	        dxlast = dxabs
	    }
	}

	# If we get here then no center was found.
	xc = INDEF
	ic = INDEF

done_	call asifree (asi1)
	call asifree (asi2)
end


# RG_IPARAB -- Compute the coefficients of the parabola through three
# evenly spaced points.

procedure rg_iparab (x, y, c)

real	x[NPARS_PARABOLA]		#I input x values
real	y[NPARS_PARABOLA]		#I input y values
real	c[NPARS_PARABOLA]		#O computed coefficients

begin
	c[3] = (y[1]-y[2]) * (x[2]-x[3]) / (x[1]-x[2]) - (y[2]-y[3])
        c[3] = c[3] / ((x[1]**2-x[2]**2) * (x[2]-x[3]) / (x[1]-x[2]) -
                (x[2]**2-x[3]**2))

        c[2] = (y[1] - y[2]) - c[3] * (x[1]**2 - x[2]**2)
        c[2] = c[2] / (x[1] - x[2])

        c[1] = y[1] - c[2] * x[1] - c[3] * x[1]**2
end


# RG_POLYFIT -- Evaluate an nth order polynomial.

procedure rg_polyfit (x, nvars, p, np, z)

real	x		#I position coordinate
int	nvars		#I number of variables
real	p[ARB]		#I coefficients of polynomial
int	np		#I number of parameters 
real	z		#O function return

int	i
real	r

begin
	r = 0.0
	do i = 2, np 
	    r = r + x**(i-1) * p[i]
	z = p[1] + r
end


# RG_DPOLYFIT -- Evaluate an nth order polynomial and its derivatives.

procedure rg_dpolyfit (x, nvars, p, dp, np, z, der)

real	x		#I position coordinate
int	nvars		#I number of variables
real	p[ARB]		#I coefficients of polynomial
real	dp[ARB]		#I parameter derivative increments
int	np		#I number of parameters
real	z		#O function value
real	der[ARB]	#O derivatives

int	i

begin
	der[1] = 1.0
	z = 0.0
	do i = 2, np {
	    der[i] = x ** (i-1)
	    z = z + x**(i-1) * p[i]  
	}
	z = p[1] + z
end
