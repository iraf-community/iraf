# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <math/gsurfit.h>
include "geomap.h"

# GEOMINIT -- Procedure to initialize the fitting routines

procedure geominit (fit, function, xxorder, xyorder, xxterms,
    yxorder, yyorder, yxterms, reject)

pointer	fit		# pointer to the fit structure
int	function	# fitting function
int	xxorder		# order of x fit in x
int	xyorder		# order of x fit in y
int	xxterms		# include cross terms in x fit
int	yxorder		# order of y fit in x
int	yyorder		# order of y fit in y
int	yxterms		# include cross-terms in y fit
real	reject		# rejection threshold in sigma

begin
	# allocate the space
	call malloc (fit, LEN_GEOMAP, TY_STRUCT)

	# set function and order
	GM_FUNCTION(fit) = function
	GM_XXORDER(fit) = xxorder
	GM_XYORDER(fit) = xyorder
	GM_XXTERMS(fit) = xxterms
	GM_YXORDER(fit) = yxorder
	GM_YYORDER(fit) = yyorder
	GM_YXTERMS(fit) = yxterms

	GM_XRMS(fit) = 0.
	GM_YRMS(fit) = 0.

	# rejection parameters
	GM_REJECT(fit) = reject
	GM_NREJECT(fit) = 0
	GM_REJ(fit) = NULL
end

# GEOFREE -- Procedure to release fitting space

procedure geofree (fit)

pointer	fit

begin
	call mfree (fit, TY_STRUCT)
end

# GEOMFIT -- Fit the surface

procedure geomfit (fit, sf1, sf2, x, y, z, wts, resid, npts, xfit)

pointer	fit		# pointer to the fit sturcture
pointer	sf1		# pointer to linear surface
pointer	sf2		# pointer to higher order surface
real	x[npts]		# reference image x values
real	y[npts]		# reference image y values
real	z[npts]	        # z values 
real	wts[npts]	# array of weights
real	resid[npts]	# fitted residuals
int	npts		# number of points
int	xfit		# X fit?

int	ier
pointer	sp, zfit

int	i

begin
	    call smark (sp)
	    call salloc (zfit, npts, TY_REAL)

	    # initalize fit
	    if (sf1 != NULL)
	        call gsfree (sf1)
	    if (sf2 != NULL)
		call gsfree (sf2)

	    if (xfit == YES) {
		if (GM_XXORDER(fit) == 2 && GM_XYORDER(fit) == 2 &&
		    GM_XXTERMS(fit) == NO)
	            call gsinit (sf1, GS_POLYNOMIAL, 2, 2, NO, GM_XMIN(fit),
		        GM_XMAX(fit), GM_YMIN(fit), GM_YMAX(fit))
		else
	            call gsinit (sf1, GM_FUNCTION(fit), 2, 2, NO, GM_XMIN(fit),
		        GM_XMAX(fit), GM_YMIN(fit), GM_YMAX(fit))
		if (GM_XXORDER(fit) > 2 || GM_XYORDER(fit) > 2)
	            call gsinit (sf2, GM_FUNCTION(fit), GM_XXORDER(fit),
		        GM_XYORDER(fit), GM_XXTERMS(fit), GM_XMIN(fit),
			GM_XMAX(fit), GM_YMIN(fit), GM_YMAX(fit))
		else 
		    sf2 = NULL
	     } else {
		if (GM_YXORDER(fit) == 2 && GM_YYORDER(fit) == 2 &&
		    GM_YXTERMS(fit) == NO)
	            call gsinit (sf1, GS_POLYNOMIAL, 2, 2, NO, GM_XMIN(fit),
		        GM_XMAX(fit), GM_YMIN(fit), GM_YMAX(fit))
		else
	            call gsinit (sf1, GM_FUNCTION(fit), 2, 2, NO, GM_XMIN(fit),
		        GM_XMAX(fit), GM_YMIN(fit), GM_YMAX(fit))
		if (GM_YXORDER(fit) > 2 || GM_YYORDER(fit) > 2)
	            call gsinit (sf2, GM_FUNCTION(fit), GM_YXORDER(fit),
		        GM_YYORDER(fit), GM_YXTERMS(fit), GM_XMIN(fit),
			GM_XMAX(fit), GM_YMIN(fit), GM_YMAX(fit))
		else 
		    sf2 = NULL
	    }

	    # fit linear function
	    call gsfit (sf1, x, y, z, wts, npts, WTS_USER, ier)
	    if (ier == NO_DEG_FREEDOM) {
		if (xfit == YES)
		    call error (0, "GEOMFIT: Too few data points for X fit.")
		else
		    call error (0, "GEOMFIT: Too few data points for Y fit.")
	    } else if (ier == SINGULAR) {
		if (xfit == YES)
		    call eprintf ("GEOMFIT: Warning singular X solution.\n")
		else
		    call eprintf ("GEOMFIT: Warning singular Y solution.\n")
	    }
	    call gsvector (sf1, x, y, resid, npts)
	    call asubr (z, resid, resid, npts)

	    # calculate higher order fit
	    if (sf2 != NULL) {
		call gsfit (sf2, x, y, resid, wts, npts, WTS_USER, ier)
		if (ier == NO_DEG_FREEDOM) {
		    if (xfit == YES)
		       call error (0, "GEOMFIT: Too few data points for X fit.")
		    else
		       call error (0, "GEOMFIT: Too few data points for Y fit.")
		} else if (ier == SINGULAR) {
		    if (xfit == YES)
			call eprintf ("GEOMFIT: Warning singular X solution.\n")
		    else
			call eprintf ("GEOMFIT: Warning singular Y solution.\n")
		}
		call gsvector (sf2, x, y, Memr[zfit], npts)
		call asubr (resid, Memr[zfit], resid, npts)
	    }

	    # compute the number of zero weighted points
	    GM_NWTS0(fit) = 0
	    do i = 1, npts {
		if (wts[i] <= 0.0)
		    GM_NWTS0(fit) = GM_NWTS0(fit) + 1
	    }

	    # calculate the rms of the fit
	    if (xfit == YES) {
		GM_XRMS(fit) = 0.0
		do i = 1, npts
	            GM_XRMS(fit) = GM_XRMS(fit) + wts[i] * resid[i] ** 2
	    } else {
		GM_YRMS(fit) = 0.0
		do i = 1, npts
		    GM_YRMS(fit) = GM_YRMS(fit) + wts[i] * resid[i] ** 2
	    }

	    GM_NPTS(fit) = npts

	    call sfree (sp)
end

# GEOFIT -- Procedure to fit surface in batch

procedure geofit (fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin, wts, npts)

pointer	fit		# pointer to fitting structure
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to higher order correction
real	xref[ARB]	# x reference array
real	yref[ARB]	# y reference array
real	xin[ARB]	# x array
real	yin[ARB]	# y array
real	wts[ARB]	# weight array
int	npts

pointer	sp, xresidual, yresidual

errchk	geomfit, geomreject

begin
	call smark (sp)
	call salloc (xresidual, npts, TY_REAL)
	call salloc (yresidual, npts, TY_REAL)

	call geomfit (fit, sx1, sx2, xref, yref, xin, wts, Memr[xresidual],
	    npts, YES)
	call geomfit (fit, sy1, sy2, xref, yref, yin, wts, Memr[yresidual],
	    npts, NO)
	if (GM_REJECT(fit) > 0.0)
	    call geomreject (fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin,
		wts, Memr[xresidual], Memr[yresidual], npts)
	else
	    GM_NREJECT(fit) = 0

	call sfree (sp)
end

# GEOMREJECT -- Procedure to reject points from the fit

procedure geomreject (fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin, wts,
    xresid, yresid, npts) 

pointer	fit		# pointer to the fit structure
pointer	sx1, sy1	# pointers to the linear surface
pointer sx2, sy2	# pointers to the higher order surface
real	xref[npts]	# reference image x values
real	yref[npts]	# yreference values
real	xin[npts]	# x values
real	yin[npts]	# yvalues
real	wts[npts]	# weights
real	xresid[npts]	# residuals
real	yresid[npts]	# yresiduals
int	npts		# number of data points

int	i
int	nreject, ier
pointer	sp, xfit, yfit
real	cutx, cuty

real	gseval()

begin
	# allocate working space
	call smark (sp)
	call salloc (xfit, npts, TY_REAL)
	call salloc (yfit, npts, TY_REAL)

	# allocate space for residuals
	if (GM_REJ(fit) != NULL)
	    call mfree (GM_REJ(fit), TY_INT)
	call malloc (GM_REJ(fit), npts, TY_INT)

	# compute the rejection limits
	if ((npts - GM_NWTS0(fit)) > 1) {
	    cutx = GM_REJECT(fit) * sqrt (GM_XRMS(fit) / (npts -
	        GM_NWTS0(fit) - 1))
	    cuty = GM_REJECT(fit) * sqrt (GM_YRMS(fit) / (npts -
	        GM_NWTS0(fit) - 1))
	} else {
	    cutx = MAX_REAL
	    cuty = MAX_REAL
	}

	# reject points from the fit
	nreject = 0
	do i = 1, npts {
	    if (wts[i] > 0.0 && ((abs (xresid[i]) > cutx) || (abs (yresid[i]) >
	        cuty))) {
		call gsrej (sx1, xref[i], yref[i], xin[i], wts[i], WTS_USER)
		if (sx2 != NULL)
		    call gsrej (sx2, xref[i], yref[i], (xin[i] -
		        gseval (sx1, xref[i], yref[i])), wts[i], WTS_USER)
		call gsrej (sy1, xref[i], yref[i], yin[i], wts[i], WTS_USER)
		if (sy2 != NULL)
		    call gsrej (sy2, xref[i], yref[i], (yin[i] -
		    gseval (sy1, xref[i], yref[i])), wts[i], WTS_USER)
		nreject = nreject + 1
		Memi[GM_REJ(fit)+nreject-1] = i
	    }
	}
	GM_NREJECT(fit) = nreject

	# resolve and calculate new residuals
	if (nreject > 0) {

	    # number of deleted points
	    GM_NWTS0(fit) = 0
	    do i = 1, npts {
		if (wts[i] <= 0.0)
		    GM_NWTS0(fit) = GM_NWTS0(fit) + 1
	    }

	    # xfit
	    call gssolve (sx1, ier)
	    if (ier == NO_DEG_FREEDOM)
		call error (0, "GEOMREJECT: Error computing x fit.")
	    else if (ier == SINGULAR)
		call eprintf ("GEOMREJECT: Warning singular x solution.\n")
	    call gsvector (sx1, xref, yref, xresid, npts)
	    call asubr (xin, xresid, xresid, npts)
	    if (sx2 != NULL) {
	        call gssolve (sx2, ier)
	        if (ier == NO_DEG_FREEDOM)
		    call error (0, "GEOMREJECT: Error computing x fit.")
		else if (ier == SINGULAR)
		    call eprintf ("GEOMREJECT: Warning singular x solution.\n")
		call gsvector (sx2, xref, yref, Memr[xfit], npts)
		call asubr (xresid, Memr[xfit], xresid, npts)
	    }

	    # x fit rms
	    GM_XRMS(fit) = 0.0
	    do i = 1, npts
	        GM_XRMS(fit) = GM_XRMS(fit) + wts[i] * xresid[i] ** 2

	    # yfit
	    call gssolve (sy1, ier)
	    if (ier == NO_DEG_FREEDOM)
		call error (0, "GEOMREJECT: Error computing y fit.")
	    else if (ier == SINGULAR)
	       call eprintf ("GEOMREJECT: Error warning singular y solution.\n")
	    call gsvector (sy1, xref, yref, yresid, npts)
	    call asubr (yin, yresid, yresid, npts)
	    if (sy2 != NULL) {
		call gssolve (sy2, ier)
	        if (ier == NO_DEG_FREEDOM)
		    call error (0, "GEOMREJECT: Error computing y fit.")
		else if (ier == SINGULAR)
		    call eprintf ("GEOMREJECT: Warning singular y solution.\n")
		call gsvector (sy2, xref, yref, Memr[yfit], npts)
		call asubr (yresid, Memr[yfit], yresid, npts)
	    }

	    # y fit rms
	    GM_YRMS(fit) = 0.0
	    do i = 1, npts
	        GM_YRMS(fit) = GM_YRMS(fit) + wts[i] * yresid[i] ** 2

	    do i = 1, GM_NREJECT(fit) {
		GM_XRMS(fit) = GM_XRMS(fit) - wts[Memi[GM_REJ(fit)+i-1]] *
		   xresid[Memi[GM_REJ(fit)+i-1]] ** 2
		GM_YRMS(fit) = GM_YRMS(fit) - wts[Memi[GM_REJ(fit)+i-1]] *
		    yresid[Memi[GM_REJ(fit)+i-1]] ** 2
	    }
	}

	call sfree (sp)
end

# GEOMFREE - procedure to free space used by GEOFIT

procedure geomfree (sx1, sy1, sx2, sy2)

pointer	sx1		# pointer to the x fits
pointer	sy1		# pointer to the y fit
pointer	sx2		# pointer to the higher order x fit
pointer	sy2		# pointer to the higher order y fit

begin
	if (sx1 != NULL)
	    call gsfree (sx1)
	if (sy1 != NULL)
	    call gsfree (sy1)
	if (sx2 != NULL)
	    call gsfree (sx2)
	if (sy2 != NULL)
	    call gsfree (sy2)
end

# GEODMFREE - procedure to free space used by GEOFIT

procedure geodmfree (sx1, sy1, sx2, sy2)

pointer	sx1		# pointer to the x fits
pointer	sy1		# pointer to the y fit
pointer	sx2		# pointer to the higher order x fit
pointer	sy2		# pointer to the higher order y fit

begin
	if (sx1 != NULL)
	    call dgsfree (sx1)
	if (sy1 != NULL)
	    call dgsfree (sy1)
	if (sx2 != NULL)
	    call dgsfree (sx2)
	if (sy2 != NULL)
	    call dgsfree (sy2)
end

# GEOMFITD -- Fit the surface

procedure geomfitd (fit, sf1, sf2, x, y, z, wts, resid, npts, xfit)

pointer	fit		# pointer to the fit sturcture
pointer	sf1		# pointer to linear surface
pointer	sf2		# pointer to higher order surface
double	x[npts]		# reference image x values
double	y[npts]		# reference image y values
double	z[npts]	        # z values 
double	wts[npts]	# array of weights
double	resid[npts]	# fitted residuals
int	npts		# number of points
int	xfit		# X fit?

int	i, ier
pointer	sp, zfit

begin
	# allocate the space
	call smark (sp)
	call salloc (zfit, npts, TY_DOUBLE)

	# initalize fit
	if (sf1 != NULL)
	    call dgsfree (sf1)
	if (sf2 != NULL)
	    call dgsfree (sf2)

	# fit the function
	if (xfit == YES) {
	    if (GM_XXORDER(fit) == 2 && GM_XYORDER(fit) == 2 &&
	        GM_XXTERMS(fit) == NO)
	        call dgsinit (sf1, GS_POLYNOMIAL, 2, 2, NO,
	            double (GM_XMIN(fit)), double (GM_XMAX(fit)),
		    double (GM_YMIN(fit)), double (GM_YMAX(fit)))
	    else
	        call dgsinit (sf1, GM_FUNCTION(fit), 2, 2, NO,
	            double (GM_XMIN(fit)), double (GM_XMAX(fit)),
		    double (GM_YMIN(fit)), double (GM_YMAX(fit)))
	    if (GM_XXORDER(fit) > 2 || GM_XYORDER(fit) > 2)
	        call dgsinit (sf2, GM_FUNCTION(fit), GM_XXORDER(fit),
		    GM_XYORDER(fit), GM_XXTERMS(fit), double (GM_XMIN(fit)),
		    double (GM_XMAX(fit)), double (GM_YMIN(fit)),
		    double (GM_YMAX(fit)))
	    else 
		sf2 = NULL
	 } else {
	    if (GM_YXORDER(fit) == 2 && GM_YYORDER(fit) == 2 &&
	        GM_YXTERMS(fit) == NO)
	        call dgsinit (sf1, GS_POLYNOMIAL, 2, 2, NO,
		    double (GM_XMIN(fit)), double (GM_XMAX(fit)),
		    double (GM_YMIN(fit)), double (GM_YMAX(fit)))
	    else
	        call dgsinit (sf1, GM_FUNCTION(fit), 2, 2, NO,
		    double (GM_XMIN(fit)), double (GM_XMAX(fit)),
		    double (GM_YMIN(fit)), double (GM_YMAX(fit)))
	    if (GM_YXORDER(fit) > 2 || GM_YYORDER(fit) > 2)
	        call dgsinit (sf2, GM_FUNCTION(fit), GM_YXORDER(fit),
		    GM_YYORDER(fit), GM_YXTERMS(fit), double (GM_XMIN(fit)),
		    double (GM_XMAX(fit)), double (GM_YMIN(fit)),
		    double (GM_YMAX(fit)))
	    else 
		sf2 = NULL
	}

	# fit linear function
	call dgsfit (sf1, x, y, z, wts, npts, WTS_USER, ier)
	if (ier == NO_DEG_FREEDOM) {
	    if (xfit == YES)
	        call error (0, "GEOMFIT: Too few data points for X fit.")
	    else
		call error (0, "GEOMFIT: Too few data points for Y fit.")
	} else if (ier == SINGULAR) {
	    if (xfit == YES)
		call eprintf ("GEOMFIT: Warning singular X solution.")
	    else
		call eprintf ("GEOMFIT: Warning singular Y solution.")
	}
	call dgsvector (sf1, x, y, resid, npts)
	call asubd (z, resid, resid, npts)

	# calculate higher order fit
	if (sf2 != NULL) {
	    call dgsfit (sf2, x, y, resid, wts, npts, WTS_USER, ier)
	    if (ier == NO_DEG_FREEDOM) {
	        if (xfit == YES)
	            call error (0, "GEOMFIT: Too few data points for X fit.")
	        else
		    call error (0, "GEOMFIT: Too few data points for Y fit.")
	    } else if (ier == SINGULAR) {
	        if (xfit == YES)
		    call eprintf ("GEOMFIT: Warning singular X solution.\n")
	        else
		    call eprintf ("GEOMFIT: Warning singular Y solution.\n")
	    }
	    call dgsvector (sf2, x, y, Memd[zfit], npts)
	    call asubd (resid, Memd[zfit], resid, npts)
	}

	# compute the number of zero weighted points
	GM_NWTS0(fit) = 0
	do i = 1, npts {
	    if (wts[i] <= 0.0d0)
		GM_NWTS0(fit) = GM_NWTS0(fit) + 1
	}

	# calculate the rms of the fit
	if (xfit == YES) {
	    GM_XRMS(fit) = 0.0
	     do i = 1, npts
		GM_XRMS(fit) = GM_XRMS(fit) + wts[i] * resid[i] ** 2
	} else {
	    GM_YRMS(fit) = 0.0
	     do i = 1, npts
		GM_YRMS(fit) = GM_YRMS(fit) + wts[i] * resid[i] ** 2
	}


	GM_NPTS(fit) = npts

	call sfree (sp)
end

# GEOFITD -- Procedure to fit surface in batch

procedure geofitd (fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin, wts, npts)

pointer	fit		# pointer to fitting structure
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to higher order correction
double	xref[ARB]	# x reference array
double	yref[ARB]	# y reference array
double	xin[ARB]	# x array
double	yin[ARB]	# y array
double	wts[ARB]	# weight array
int	npts

pointer	sp, xresidual, yresidual

errchk	geomfitd, geomrejectd
begin
	call smark (sp)
	call salloc (xresidual, npts, TY_DOUBLE)
	call salloc (yresidual, npts, TY_DOUBLE)

	call geomfitd (fit, sx1, sx2, xref, yref, xin, wts, Memd[xresidual],
	    npts, YES)
	call geomfitd (fit, sy1, sy2, xref, yref, yin, wts, Memd[yresidual],
	    npts, NO)
	if (GM_REJECT(fit) > 0.)
	    call geomrejectd (fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin,
	        wts, Memd[xresidual], Memd[yresidual], npts)
	else
	    GM_NREJECT(fit) = 0

	call sfree (sp)
end

# GEOMREJECTD -- Procedure to reject points from the fit

procedure geomrejectd (fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin, wts,
    xresid, yresid, npts) 

pointer	fit		# pointer to the fit structure
pointer	sx1, sy1	# pointers to the linear surface
pointer sx2, sy2	# pointers to the higher order surface
double	xref[npts]	# reference image x values
double	yref[npts]	# yreference values
double	xin[npts]	# x values
double	yin[npts]	# yvalues
double	wts[npts]	# weights
double	xresid[npts]	# residuals
double	yresid[npts]	# yresiduals
int	npts		# number of data points

int	i
int	nreject, ier
pointer	sp, xfit, yfit
double	cutx, cuty

double	dgseval()

begin
	# allocate working space
	call smark (sp)
	call salloc (xfit, npts, TY_DOUBLE)
	call salloc (yfit, npts, TY_DOUBLE)

	# allocate space for residuals
	if (GM_REJ(fit) != NULL)
	    call mfree (GM_REJ(fit), TY_INT)
	call malloc (GM_REJ(fit), npts, TY_INT)

	# compute the sigmas
	if ((npts - GM_NWTS0(fit)) > 1) {
	    cutx = GM_REJECT(fit) * sqrt (GM_XRMS(fit) / (npts -
		GM_NWTS0(fit) - 1))
	    cuty = GM_REJECT(fit) * sqrt (GM_YRMS(fit) / (npts -
		GM_NWTS0(fit) - 1))
	} else {
	    cutx = MAX_REAL
	    cuty = MAX_REAL
	}

	nreject = 0
	do i = 1, npts {
	    if (wts[i] > 0.0d0 && (abs (xresid[i]) > cutx ||
	        abs (yresid[i]) > cuty)) {
		call dgsrej (sx1, xref[i], yref[i], xin[i], wts[i], WTS_USER)
		call dgsrej (sy1, xref[i], yref[i], yin[i], wts[i], WTS_USER)
		if (sx2 != NULL)
		    call dgsrej (sx2, xref[i], yref[i], (xin[i] -
		        dgseval (sx1, xref[i], yref[i])), wts[i], WTS_USER)
		if (sy2 != NULL)
		    call dgsrej (sy2, xref[i], yref[i], (yin[i] -
		    dgseval (sy1, xref[i], yref[i])), wts[i], WTS_USER)
		nreject = nreject + 1
		Memi[GM_REJ(fit)+nreject-1] = i
	    }
	}
	GM_NREJECT(fit) = nreject

	# resolve and calculate new residuals
	if (nreject > 0) {

	    # compute zero weight objects
	    GM_NWTS0(fit) = 0
	    do i = 1, npts {
		if (wts[i] <= 0.0d0)
		    GM_NWTS0(fit) = GM_NWTS0(fit) + 1
	    }

	    # xfit
	    call dgssolve (sx1, ier)
	    if (ier == NO_DEG_FREEDOM)
		call error (0, "GEOMREJECT: Error computing x fit.")
	    else if (ier == SINGULAR)
		call eprintf ("GEOMREJECTD: Warning singular x solution.\n")
	    call dgsvector (sx1, xref, yref, xresid, npts)
	    call asubd (xin, xresid, xresid, npts)
	    if (sx2 != NULL) {
	        call dgssolve (sx2, ier)
	        if (ier == NO_DEG_FREEDOM)
		    call error (0, "GEOMREJECT: Error computing x fit.")
	        else if (ier == SINGULAR)
		    call eprintf ("GEOMREJECTD: Warning singular x solution.\n")
		call dgsvector (sx2, xref, yref, Memd[xfit], npts)
		call asubd (xresid, Memd[xfit], xresid, npts)
	    }
	    GM_XRMS(fit) = 0.
	    do i = 1, npts
		GM_XRMS(fit) = GM_XRMS(fit) + wts[i] * xresid[i] ** 2

	    # yfit
	    call dgssolve (sy1, ier)
	    if (ier == NO_DEG_FREEDOM)
		call error (0, "GEOMREJECT: Error computing y fit.")
	    else if (ier == SINGULAR)
		call eprintf ("GEOMREJECTD: Warning singular y solution.\n")
	    call dgsvector (sy1, xref, yref, yresid, npts)
	    call asubd (yin, yresid, yresid, npts)
	    if (sy2 != NULL) {
		call dgssolve (sy2, ier)
	        if (ier == NO_DEG_FREEDOM)
		    call error (0, "GEORMEJECT: Error computing y fit.")
	        else if (ier == SINGULAR)
		    call eprintf ("GEOMREJECTD: Warning singular y solution.\n")
		call dgsvector (sy2, xref, yref, Memd[yfit], npts)
		call asubd (yresid, Memd[yfit], yresid, npts)
	    }
	    GM_YRMS(fit) = 0.
	    do i = 1, npts
		GM_YRMS(fit) = GM_YRMS(fit) + wts[i] * yresid[i] ** 2

	    # reject points
	    do i = 1, GM_NREJECT(fit) {
		GM_XRMS(fit) = GM_XRMS(fit) - wts[Memi[GM_REJ(fit)+i-1]] *
		    xresid[Memi[GM_REJ(fit)+i-1]] ** 2
		GM_YRMS(fit) = GM_YRMS(fit) - wts[Memi[GM_REJ(fit)+i-1]] *
		    yresid[Memi[GM_REJ(fit)+i-1]] ** 2
	    }

	}

	call sfree (sp)
end
