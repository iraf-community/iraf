# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include <pkg/gtools.h>
include <mach.h>
include <math.h>
include <gset.h>
include "geomap.h"
include "geogmap.h"

# GEOCOLON -- Process colon commands

procedure geocolon (gd, fit, gfit, cmdstr, newgraph)

pointer	gd		# graphics stream
pointer	fit		# pointer to fit structure
pointer	gfit		# pointer to the gfit structure
char	cmdstr[ARB]	# Command string
int	newgraph	# plot newgraph

bool	bval
char	cmd[SZ_LINE]
int	ncmd, ival
real	rval

bool	itob()
int	nscan(), strdic(), btoi()

string	cmds	"|show|function|xxorder|xyorder|yxorder|yyorder\
|xxterms|yxterms|reject|"
string	funcs	"|chebyshev|legendre|polynomial|"

begin
	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	if (nscan() == 0)
	    return

	ncmd = strdic (cmd, cmd, SZ_LINE, cmds)
	switch (ncmd) {
	case 1:
	    call gdeactivate (gd, AW_CLEAR)
	    call printf ("Current Fitting Parameters\n\n")
	    call printf ("\tfunction = %s\n")
	    switch (GM_FUNCTION(fit)) {
	    case GS_LEGENDRE:
		call pargstr ("legendre")
	    case GS_CHEBYSHEV:
		call pargstr ("chebyshev")
	    case GS_POLYNOMIAL:
		call pargstr ("polynomial")
	    }
	    call printf ("\txxorder = %d\n")
		call pargi (GM_XXORDER(fit))
	    call printf ("\txyorder = %d\n")
		call pargi (GM_XYORDER(fit))
	    call printf ("\txxterms = %b\n")
		call pargb (itob (GM_XXTERMS(fit)))
	    call printf ("\tyxorder = %d\n")
		call pargi (GM_YXORDER(fit))
	    call printf ("\tyyorder = %d\n")
		call pargi (GM_YYORDER(fit))
	    call printf ("\tyxterms = %b\n")
		call pargb (itob (GM_YXTERMS(fit)))
	    call printf ("\treject = %f\n")
		call pargr (GM_REJECT(fit))
	    call greactivate (gd, AW_PAUSE)
	case 2:
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan () == 1) {
	        call printf ("function = %s\n")
		switch (GM_FUNCTION(fit)) {
		case GS_LEGENDRE:
		    call pargstr ("legendre")
		case GS_CHEBYSHEV:
		    call pargstr ("chebyshev")
		case GS_POLYNOMIAL:
		    call pargstr ("polynomial")
		}
	    } else {
		GM_FUNCTION(fit) = strdic (cmd, cmd, SZ_LINE, funcs)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }
	case 3:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("xxorder = %d\n")
		    call pargi (GM_XXORDER(fit))
	    } else {
		GM_XXORDER(fit) = max (ival, 2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }
	case 4:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("xyorder = %d\n")
		    call pargi (GM_XYORDER(fit))
	    } else {
		GM_XYORDER(fit) = max (ival,2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }
	case 5:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("yxorder = %d\n")
		    call pargi (GM_YXORDER(fit))
	    } else {
		GM_YXORDER(fit) = max (ival, 2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }
	case 6:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("yyorder = %d\n")
		    call pargi (GM_YYORDER(fit))
	    } else {
		GM_YYORDER(fit) = max (ival, 2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }
	case 7:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("xxterms = %b\n")
		    call pargb (itob (GM_XXTERMS(fit)))
	    } else {
		GM_XXTERMS(fit) = btoi (bval)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }
	case 8:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("yxterms = %b\n")
		    call pargb (itob (GM_YXTERMS(fit)))
	    } else {
		GM_YXTERMS(fit) = btoi (bval)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }
	case 9:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("reject = %f\n")
		    call pargr (GM_REJECT(fit))
	    } else {
		GM_REJECT(fit) = rval
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }
	}
end

# GEODELETE1 -- Delete a point from the fit

procedure geodelete1 (gd, xin, yin, wts, userwts, npts, wx, wy, delete)

pointer	gd		# pointer to graphics descriptor
real	xin[ARB]	# x array
real	yin[ARB]	# y array
real	wts[ARB]	# array of weights
real	userwts[ARB]	# array of user weights
int	npts		# number of points
real	wx, wy		# world coordinates
int	delete		# delete points ?

int	i, j, pmltype
real	r2min, r2, x0, y0

int	gstati()

begin
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	r2min = MAX_REAL
	j = 0

	if (delete == YES) {

	    # search for nearest point that has not been deleted
	    do i = 1, npts {
	        if (wts[i] <= 0.0)
		    next
	        call gctran (gd, xin[i], yin[i], x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # mark point and set weights to 0
	    if (j != 0) {
		call gscur (gd, xin[j], yin[j])
		call gmark (gd, xin[j], yin[j], GM_CROSS, 2., 2.)
		wts[j] = 0.
	    }

	} else {

	    # search for the nearest deleted point
	    do i = 1, npts {
	        if (wts[i] > 0.0)
		    next
	        call gctran (gd, xin[i], yin[i], x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # erase cross and remark with a plus
	    if (j != 0) {
		call gscur (gd, xin[j], yin[j])
		pmltype = gstati (gd, G_PMLTYPE)
		call gseti (gd, G_PMLTYPE, 0)
		call gmark (gd, xin[j], yin[j], GM_CROSS, 2., 2.)
		call gseti (gd, G_PMLTYPE, pmltype)
		call gmark (gd, xin[j], yin[j], GM_PLUS, 2., 2.)
		wts[j] = userwts[j]
	    }
	}
end


# GEODELETE2 -- Delete residuals

procedure geodelete2 (gd, x, resid, wts, userwts, npts, wx, wy, delete)

pointer	gd		# pointer to graphics descriptor
real x[ARB]		# reference x values
real resid[ARB]		# residuals
real wts[ARB]		# weight array
real userwts[ARB]	# user weight array
int npts		# number of points
real wx			# world x coordinate
real wy			# world y coordinate
int  delete		# delete point

int	i, j, pmltype
real	r2, r2min, x0, y0

int	gstati()

begin
	# delete the point
        call gctran (gd, wx, wy, wx, wy, 1, 0)
        r2min = MAX_REAL
        j = 0

	# delete or add a points
        if (delete == YES) {

	    # find the nearest undeleted point
	    do i = 1, npts {
	        if (wts[i] <= 0.0)
		    next
	        call gctran (gd, x[i], resid[i], x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # mark the point with a cross and set weight to zero
	    if (j != 0) {
	        call gscur (gd, x[j], resid[j])
	        call gmark (gd, x[j], resid[j], GM_CROSS, 2., 2.)
	        wts[j] = 0.
	    }

        } else {

	    # find the nearest deleted point
	    do i = 1, npts {
	        if (wts[i] > 0.0)
		    next
	        call gctran (gd, x[i], resid[i], x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # erase the cross and remark with a plus
	    if (j != 0) {
	        call gscur (gd, x[j], resid[j])
	        pmltype = gstati (gd, G_PMLTYPE)
	        call gseti (gd, G_PMLTYPE, 0)
	        call gmark (gd, x[j], resid[j], GM_CROSS, 2., 2.)
	        call gseti (gd, G_PMLTYPE, pmltype)
	        call gmark (gd, x[j], resid[j], GM_PLUS, 2., 2.)
	        wts[j] = userwts[j]
	    }
        }
end

define	NINTERVALS	5
define	NGRAPH		100

# GEOGRAPH1 - Procedure to graph the distribution of the data in the x-y
# plane. Rejected points are marked by a ' ' and deleted points are marked
# by a ' '. The shift in position of the data points are indicated by
# vectors. Sample fits of constant x and y are marked on the plots.

procedure geograph1 (gd, gt, fit, gfit, xref, yref, xin, yin, wts, npts)

pointer	gd		# pointer to the graphics device
pointer	gt		# pointer to the plot descriptor
pointer	fit		# pointer to the geofit structure
pointer	gfit		# pointer to the plot structure
real	xref[ARB]	# x reference values
real	yref[ARB]	# y reference values
real	xin[ARB]	# x values
real	yin[ARB]	# y values
real	wts[ARB]	# array of weights
int	npts		# number of points

int	i, j

begin
	# if previous plot different type don't overplot
	if (GG_PLOTTYPE(gfit) != FIT)
	    GG_OVERPLOT(gfit) = NO

	# if not overplottting start new plot
	if (GG_OVERPLOT(gfit) == NO) {

	    # set scale and axes
	    call gclear (gd)
	    call gascale (gd, xin, npts, 1)
	    call gascale (gd, yin, npts, 2)
	    call gt_swind (gd, gt)
	    call gtlabax (gd, gt)

	    # mark the data and deleted points
	    do i = 1, npts {
		if (wts[i] == 0.)
		    call gmark (gd, xin[i], yin[i], GM_CROSS, 2., 2.)
		else
		    call gmark (gd, xin[i], yin[i], GM_PLUS, 2., 2.)
	    }

	    call gflush (gd)
	}

	# mark the rejected points
	do i = 1, GM_NREJECT(fit) {
	    j = Memi[GM_REJ(fit)+i-1]
	    call gmark (gd, xin[j], yin[j], GM_CIRCLE, 2., 2.)
	}

	call gflush (gd)

	# reset the status flags
	GG_OVERPLOT(gfit) = NO
end


# GEOCONXY -- Procedure to plot a set of default lines of xref = const and
# yref = const

procedure geoconxy (gd, fit, sx1, sy1, sx2, sy2)

pointer	gd		# graphics file descriptor
pointer	fit		# fit descriptor
pointer	sx1, sy1	# pointer to the linear x and y surface fits
pointer sx2, sy2	# pointer to the linear x and y surface fits

int	i
pointer	sp, xtemp, ytemp, xfit1, yfit1, xfit2, yfit2
real	xint, yint, dx, dy

begin
	# allocate temporary space
	call smark (sp)
	call salloc (xtemp, NGRAPH, TY_REAL)
	call salloc (ytemp, NGRAPH, TY_REAL)
	call salloc (xfit1, NGRAPH, TY_REAL)
	call salloc (yfit1, NGRAPH, TY_REAL)
	call salloc (xfit2, NGRAPH, TY_REAL)
	call salloc (yfit2, NGRAPH, TY_REAL)

	# calculate intervals in x and y
	dx = (GM_XMAX(fit) - GM_XMIN(fit)) / NINTERVALS
	dy = (GM_YMAX(fit) - GM_YMIN(fit)) / (NGRAPH - 1)

	# set up an array of y values
	Memr[ytemp] = GM_YMIN(fit)
	do i = 2, NGRAPH
	    Memr[ytemp+i-1] = Memr[ytemp+i-2] + dy

	# mark lines of constant x
	xint = GM_XMIN(fit)
	for (i = 1; i <= NINTERVALS + 1; i = i + 1) {

	    # set the x value
	    call amovkr (xint, Memr[xtemp], NGRAPH)

	    # xfit
	    call gsvector (sx1, Memr[xtemp], Memr[ytemp], Memr[xfit1], NGRAPH)
	    if (sx2 != NULL) {
	        call gsvector (sx2, Memr[xtemp], Memr[ytemp], Memr[xfit2],
		    NGRAPH)
	        call aaddr (Memr[xfit1], Memr[xfit2], Memr[xfit1], NGRAPH)
	    }

	    # yfit
	    call gsvector (sy1, Memr[xtemp], Memr[ytemp], Memr[yfit1], NGRAPH)
	    if (sy2 != NULL) {
	        call gsvector (sy2, Memr[xtemp], Memr[ytemp], Memr[yfit2],
		NGRAPH)
	        call aaddr (Memr[yfit1], Memr[yfit2], Memr[yfit1], NGRAPH)
	    }

	    # plot line of constant x
	    call gpline (gd, Memr[xfit1], Memr[yfit1], NGRAPH)

	    # update the x value
	    xint = xint + dx
	}

	call gflush (gd)

	# calculate x and y intervals
	dx = (GM_XMAX(fit) - GM_XMIN(fit)) / (NGRAPH - 1)
	dy = (GM_YMAX(fit) - GM_YMIN(fit)) / NINTERVALS

	# set up array of x values
	Memr[xtemp] = GM_XMIN(fit)
	do i = 2, NGRAPH
	    Memr[xtemp+i-1] = Memr[xtemp+i-2] + dx

	# mark lines of constant y
	yint = GM_YMIN(fit)
	for (i = 1; i <= NINTERVALS + 1; i = i + 1) {

	    # set the y value
	    call amovkr (yint, Memr[ytemp], NGRAPH)

	    # xfit
	    call gsvector (sx1, Memr[xtemp], Memr[ytemp], Memr[xfit1], NGRAPH)
	    if (sx2 != NULL) {
	        call gsvector (sx2, Memr[xtemp], Memr[ytemp], Memr[xfit2],
		    NGRAPH)
	        call aaddr (Memr[xfit1], Memr[xfit2], Memr[xfit1], NGRAPH)
	    }


	    # yfit
	    call gsvector (sy1, Memr[xtemp], Memr[ytemp], Memr[yfit1], NGRAPH)
	    if (sy2 != NULL) {
	        call gsvector (sy2, Memr[xtemp], Memr[ytemp], Memr[yfit2],
		    NGRAPH)
	        call aaddr (Memr[yfit1], Memr[yfit2], Memr[yfit1], NGRAPH)
	    }

	    # plot line of constant y
	    call gpline (gd, Memr[xfit1], Memr[yfit1], NGRAPH)

	    # update the y value
	    yint = yint + dy
	}

	call gflush (gd)

	call sfree (sp)
end

# GEOCONXYD -- Procedure to plot a set of default lines of xref = const and
# yref = const

procedure geoconxyd (gd, fit, sx1, sy1, sx2, sy2)

pointer	gd		# graphics file descriptor
pointer	fit		# fit descriptor
pointer	sx1, sy1	# pointer to the linear x and y surface fits
pointer sx2, sy2	# pointer to the linear x and y surface fits

int	i
pointer	sp, xtemp, ytemp, xfit1, yfit1, xfit2, yfit2, xbuf, ybuf
double	xint, yint, dx, dy

begin
	# allocate temporary space
	call smark (sp)
	call salloc (xtemp, NGRAPH, TY_DOUBLE)
	call salloc (ytemp, NGRAPH, TY_DOUBLE)
	call salloc (xfit1, NGRAPH, TY_DOUBLE)
	call salloc (yfit1, NGRAPH, TY_DOUBLE)
	call salloc (xfit2, NGRAPH, TY_DOUBLE)
	call salloc (yfit2, NGRAPH, TY_DOUBLE)
	call salloc (xbuf, NGRAPH, TY_REAL)
	call salloc (ybuf, NGRAPH, TY_REAL)

	# calculate intervals in x and y
	dx = (GM_XMAX(fit) - GM_XMIN(fit)) / NINTERVALS
	dy = (GM_YMAX(fit) - GM_YMIN(fit)) / (NGRAPH - 1)

	# set up an array of y values
	Memd[ytemp] = GM_YMIN(fit)
	do i = 2, NGRAPH
	    Memd[ytemp+i-1] = Memd[ytemp+i-2] + dy

	# mark lines of constant x
	xint = GM_XMIN(fit)
	for (i = 1; i <= NINTERVALS + 1; i = i + 1) {

	    # set the x value
	    call amovkd (xint, Memd[xtemp], NGRAPH)

	    # xfit
	    call dgsvector (sx1, Memd[xtemp], Memd[ytemp], Memd[xfit1], NGRAPH)
	    if (sx2 != NULL) {
	        call dgsvector (sx2, Memd[xtemp], Memd[ytemp], Memd[xfit2],
		    NGRAPH)
	        call aaddd (Memd[xfit1], Memd[xfit2], Memd[xfit1], NGRAPH)
	    }

	    # yfit
	    call dgsvector (sy1, Memd[xtemp], Memd[ytemp], Memd[yfit1], NGRAPH)
	    if (sy2 != NULL) {
	        call dgsvector (sy2, Memd[xtemp], Memd[ytemp], Memd[yfit2],
		NGRAPH)
	        call aaddd (Memd[yfit1], Memd[yfit2], Memd[yfit1], NGRAPH)
	    }

	    # plot line of constant x
	    call achtdr (Memd[xfit1], Memr[xbuf], NGRAPH)
	    call achtdr (Memd[yfit1], Memr[ybuf], NGRAPH)
	    call gpline (gd, Memr[xbuf], Memr[ybuf], NGRAPH)

	    # update the x value
	    xint = xint + dx
	}

	call gflush (gd)

	# calculate x and y intervals
	dx = (GM_XMAX(fit) - GM_XMIN(fit)) / (NGRAPH - 1)
	dy = (GM_YMAX(fit) - GM_YMIN(fit)) / NINTERVALS

	# set up array of x values
	Memd[xtemp] = GM_XMIN(fit)
	do i = 2, NGRAPH
	    Memd[xtemp+i-1] = Memd[xtemp+i-2] + dx

	# mark lines of constant y
	yint = GM_YMIN(fit)
	for (i = 1; i <= NINTERVALS + 1; i = i + 1) {

	    # set the y value
	    call amovkd (yint, Memd[ytemp], NGRAPH)

	    # xfit
	    call dgsvector (sx1, Memd[xtemp], Memd[ytemp], Memd[xfit1], NGRAPH)
	    if (sx2 != NULL) {
	        call dgsvector (sx2, Memd[xtemp], Memd[ytemp], Memd[xfit2],
		    NGRAPH)
	        call aaddd (Memd[xfit1], Memd[xfit2], Memd[xfit1], NGRAPH)
	    }


	    # yfit
	    call dgsvector (sy1, Memd[xtemp], Memd[ytemp], Memd[yfit1], NGRAPH)
	    if (sy2 != NULL) {
	        call dgsvector (sy2, Memd[xtemp], Memd[ytemp], Memd[yfit2],
		    NGRAPH)
	        call aaddd (Memd[yfit1], Memd[yfit2], Memd[yfit1], NGRAPH)
	    }

	    # plot line of constant y
	    call achtdr (Memd[xfit1], Memr[xbuf], NGRAPH)
	    call achtdr (Memd[yfit1], Memr[ybuf], NGRAPH)
	    call gpline (gd, Memr[xbuf], Memr[ybuf], NGRAPH)

	    # update the y value
	    yint = yint + dy
	}

	call gflush (gd)

	call sfree (sp)
end

# GEOGRAPH2 -- Procedure to graph the x and y fit residuals versus x or y .

procedure geograph2 (gd, gt, fit, gfit, x, resid, wts, npts)

pointer	gd		# pointer to the graphics device
pointer	gt		# pointer to the plot descriptor
pointer	fit		# pointer to geomap structure
pointer	gfit		# pointer to the plot structure
real	x[ARB]		# x reference values
real	resid[ARB]	# residual
real	wts[ARB]	# array of weights
int	npts		# number of points

int	i, j
pointer	sp, zero

begin
	# allocate space
	call smark (sp)
	call salloc (zero, npts, TY_REAL)
	call amovkr (0., Memr[zero], npts)

	# calculate the residuals
	if (GG_PLOTTYPE(gfit) == FIT)
	    GG_OVERPLOT(gfit) = NO

	if (GG_OVERPLOT(gfit) == NO) {

	    call gclear (gd)

	    # set scale and axes
	    call gascale (gd, x, npts, 1)
	    call gascale (gd, resid, npts, 2)
	    call gt_swind (gd, gt)
	    call gtlabax (gd, gt)

	    call gpline (gd, x, Memr[zero], npts)
	}

	# graph residuals and mark deleted points
	if (GG_OVERPLOT(gfit) == NO || GG_NEWFUNCTION(gfit) == YES) {
	    do i = 1, npts {
		if (wts[i] == 0.)
		    call gmark (gd, x[i], resid[i], GM_CROSS, 2., 2.)
		else
		    call gmark (gd, x[i], resid[i], GM_PLUS, 2., 2.)
	    }
	}

	# plot rejected points
	if (GM_NREJECT(fit) > 0) {
	    do i = 1, GM_NREJECT(fit) {
		j = Memi[GM_REJ(fit)+i-1]
		call gmark (gd, x[j], resid[j], GM_CIRCLE, 2., 2.)
	    }
	}

	# reset the status flag
	GG_OVERPLOT(gfit) = NO

	call gflush (gd)
	call sfree (sp)
end

# GEOLINXY -- Procedure to draw a line of constant x-y

procedure geolinxy (gd, fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin, npts, 
    wx, wy)

pointer	gd		# pointer to graphics descriptor
pointer	fit		# pointer to the fit parameters
pointer	sx1		# pointer to the linear x fit
pointer	sy1		# pointer to the linear y fit
pointer	sx2		# pointer to the higher order x fit
pointer	sy2		# pointer to the higher order y fit
real	xref[ARB]	# pointer to reference x values
real	yref[ARB]	# pointer to reference y values
real	xin[ARB]	# pointer to x input values
real	yin[ARB]	# pointer to y input values
int	npts		# number of data points
real	wx, wy		# pointer to world coordinates

int	i, j
pointer	sp, xtemp, ytemp, xfit1, yfit1, xfit2, yfit2
real	x0, y0, r2, r2min, delta, deltax, deltay

real	gseval()

begin
	# world coordinates
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	r2min = MAX_REAL
	j = 0

	# find the nearest data point
	do i = 1, npts {
	    call gctran (gd, xin[i], yin[i], x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
	        r2min = r2
	        j = i
	    }
	}

	# fit the line
	if (j != 0) {

	    # allocate temporary space
	    call smark (sp)
	    call salloc (xtemp, NGRAPH, TY_REAL)
	    call salloc (ytemp, NGRAPH, TY_REAL)
	    call salloc (xfit1, NGRAPH, TY_REAL)
	    call salloc (yfit1, NGRAPH, TY_REAL)
	    call salloc (xfit2, NGRAPH, TY_REAL)
	    call salloc (yfit2, NGRAPH, TY_REAL)

	    # compute the deltas
	    deltax = xin[j] - gseval (sx1, xref[j], yref[j])
	    if (sx2 != NULL)
	        deltax = deltax - gseval (sx2, xref[j], yref[j])
	    deltay = yin[j] - gseval (sy1, xref[j], yref[j])
	    if (sy2 != NULL)
		deltay = deltay - gseval (sy2, xref[j], yref[j])

	    # set up line of constant x
	    call amovkr (xref[j], Memr[xtemp], NGRAPH)
	    delta = (GM_YMAX(fit) - GM_YMIN(fit)) / (NGRAPH - 1)
	    Memr[ytemp] = GM_YMIN(fit)
	    do i = 2, NGRAPH
	        Memr[ytemp+i-1] = Memr[ytemp+i-2] + delta

	    # x solution
	    call gsvector (sx1, Memr[xtemp], Memr[ytemp], Memr[xfit1], NGRAPH)
	    if (sx2 != NULL) {
	        call gsvector (sx2, Memr[xtemp], Memr[ytemp], Memr[xfit2],
		    NGRAPH)
	        call aaddr (Memr[xfit1], Memr[xfit2], Memr[xfit1], NGRAPH)
	    }
	    call aaddkr (Memr[xfit1], deltax, Memr[xfit1], NGRAPH)

	    # y solution
	    call gsvector (sy1, Memr[xtemp], Memr[ytemp], Memr[yfit1], NGRAPH)
	    if (sy2 != NULL) {
	        call gsvector (sy2, Memr[xtemp], Memr[ytemp], Memr[yfit2],
		    NGRAPH)
	        call aaddr (Memr[yfit1], Memr[yfit2], Memr[yfit1], NGRAPH)
	    }
	    call aaddkr (Memr[yfit1], deltay, Memr[yfit1], NGRAPH)

	    # plot line of constant x
	    call gpline (gd, Memr[xfit1], Memr[yfit1], NGRAPH)
	    call gflush (gd)

	    # set up line of constant y
	    call amovkr (yref[j], Memr[ytemp], NGRAPH)
	    delta = (GM_XMAX(fit) - GM_XMIN(fit)) / (NGRAPH - 1)
	    Memr[xtemp] = GM_XMIN(fit)
	    do i = 2, NGRAPH
	        Memr[xtemp+i-1] = Memr[xtemp+i-2] + delta

	    # xfit
	    call gsvector (sx1, Memr[xtemp], Memr[ytemp], Memr[xfit1], NGRAPH)
	    if (sx2 != NULL) {
	        call gsvector (sx2, Memr[xtemp], Memr[ytemp], Memr[xfit2],
		    NGRAPH)
	        call aaddr (Memr[xfit1], Memr[xfit2], Memr[xfit1], NGRAPH)
	    }
	    call aaddkr (Memr[xfit1], deltax, Memr[xfit1], NGRAPH)

	    # yfit
	    call gsvector (sy1, Memr[xtemp], Memr[ytemp], Memr[yfit1], NGRAPH)
	    if (sy2 != NULL) {
	        call gsvector (sy2, Memr[xtemp], Memr[ytemp], Memr[yfit2],
		    NGRAPH)
	        call aaddr (Memr[yfit1], Memr[yfit2], Memr[yfit1], NGRAPH)
	    }
	    call aaddkr (Memr[yfit1], deltay, Memr[yfit1], NGRAPH)

	    # plot line of constant y
	    call gpline (gd, Memr[xfit1], Memr[yfit1], NGRAPH)
	    call gflush (gd)

	    # free space
	    call sfree (sp)
	}
end

# GEOLINXYD -- Procedure to draw a line of constant x-y

procedure geolinxyd (gd, fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin, npts, 
    wx, wy)

pointer	gd		# pointer to graphics descriptor
pointer	fit		# pointer to the fit parameters
pointer	sx1		# pointer to the linear x fit
pointer	sy1		# pointer to the linear y fit
pointer	sx2		# pointer to the higher order x fit
pointer	sy2		# pointer to the higher order y fit
double	xref[ARB]	# pointer to reference x values
double	yref[ARB]	# pointer to reference y values
double	xin[ARB]	# pointer to x input values
double	yin[ARB]	# pointer to y input values
int	npts		# number of data points
real	wx, wy		# pointer to world coordinates

double	delta, deltax, deltay
int	i, j
pointer	sp, xtemp, ytemp, xfit1, yfit1, xfit2, yfit2, xbuf, ybuf
real	x0, y0, r2, r2min

double	dgseval()

begin
	# world coordinates
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	r2min = MAX_REAL
	j = 0

	# find the nearest data point
	do i = 1, npts {
	    call gctran (gd, real (xin[i]), real (yin[i]), x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
	        r2min = r2
	        j = i
	    }
	}

	# fit the line
	if (j != 0) {

	    # allocate temporary space
	    call smark (sp)
	    call salloc (xtemp, NGRAPH, TY_DOUBLE)
	    call salloc (ytemp, NGRAPH, TY_DOUBLE)
	    call salloc (xfit1, NGRAPH, TY_DOUBLE)
	    call salloc (yfit1, NGRAPH, TY_DOUBLE)
	    call salloc (xfit2, NGRAPH, TY_DOUBLE)
	    call salloc (yfit2, NGRAPH, TY_DOUBLE)
	    call salloc (xbuf, NGRAPH, TY_REAL)
	    call salloc (ybuf, NGRAPH, TY_REAL)

	    # compute corrections to output frame
	    deltax = xin[j] - dgseval (sx1, xref[j], yref[j])
	    if (sx2 != NULL)
		deltax = deltax - dgseval (sx2, xref[j], yref[j])
	    deltay = yin[j] - dgseval (sy1, xref[j], yref[j])
	    if (sy2 != NULL)
		deltay = deltay - dgseval (sy2, xref[j], yref[j])

	    # set up line of constant x
	    call amovkd (xref[j], Memd[xtemp], NGRAPH)
	    delta = (GM_YMAX(fit) - GM_YMIN(fit)) / (NGRAPH - 1)
	    Memd[ytemp] = GM_YMIN(fit)
	    do i = 2, NGRAPH
	        Memd[ytemp+i-1] = Memd[ytemp+i-2] + delta

	    # x solution
	    call dgsvector (sx1, Memd[xtemp], Memd[ytemp], Memd[xfit1], NGRAPH)
	    if (sx2 != NULL) {
	        call dgsvector (sx2, Memd[xtemp], Memd[ytemp], Memd[xfit2],
		    NGRAPH)
	        call aaddd (Memd[xfit1], Memd[xfit2], Memd[xfit1], NGRAPH)
	    }
	    call aaddkd (Memd[xfit1], deltax, Memd[xfit1], NGRAPH)

	    # y solution
	    call dgsvector (sy1, Memd[xtemp], Memd[ytemp], Memd[yfit1], NGRAPH)
	    if (sy2 != NULL) {
	        call dgsvector (sy2, Memd[xtemp], Memd[ytemp], Memd[yfit2],
		    NGRAPH)
	        call aaddd (Memd[yfit1], Memd[yfit2], Memd[yfit1], NGRAPH)
	    }
	    call aaddkd (Memd[yfit1], deltay, Memd[yfit1], NGRAPH)

	    # plot line of constant x
	    call achtdr (Memd[xfit1], Memr[xbuf], NGRAPH)
	    call achtdr (Memd[yfit1], Memr[ybuf], NGRAPH)
	    call gpline (gd, Memr[xbuf], Memr[ybuf], NGRAPH)
	    call gflush (gd)

	    # set up line of constant y
	    call amovkd (yref[j], Memd[ytemp], NGRAPH)
	    delta = (GM_XMAX(fit) - GM_XMIN(fit)) / (NGRAPH - 1)
	    Memd[xtemp] = GM_XMIN(fit)
	    do i = 2, NGRAPH
	        Memd[xtemp+i-1] = Memd[xtemp+i-2] + delta

	    # xfit
	    call dgsvector (sx1, Memd[xtemp], Memd[ytemp], Memd[xfit1], NGRAPH)
	    if (sx2 != NULL) {
	        call dgsvector (sx2, Memd[xtemp], Memd[ytemp], Memd[xfit2],
		    NGRAPH)
	        call aaddd (Memd[xfit1], Memd[xfit2], Memd[xfit1], NGRAPH)
	    }
	    call aaddkd (Memd[xfit1], deltax, Memd[xfit1], NGRAPH)

	    # yfit
	    call dgsvector (sy1, Memd[xtemp], Memd[ytemp], Memd[yfit1], NGRAPH)
	    if (sy2 != NULL) {
	        call dgsvector (sy2, Memd[xtemp], Memd[ytemp], Memd[yfit2],
		    NGRAPH)
	        call aaddd (Memd[yfit1], Memd[yfit2], Memd[yfit1], NGRAPH)
	    }
	    call aaddkd (Memd[yfit1], deltay, Memd[yfit1], NGRAPH)

	    # plot line of constant y
	    call achtdr (Memd[xfit1], Memr[xbuf], NGRAPH)
	    call achtdr (Memd[yfit1], Memr[ybuf], NGRAPH)
	    call gpline (gd, Memr[xbuf], Memr[ybuf], NGRAPH)
	    call gflush (gd)

	    # free space
	    call sfree (sp)
	}
end

define	MAX_PARAMS	(10 * SZ_LINE)	

# GEOLABEL -- Produce the plotype annotation

procedure geolabel (plot_type, gt, fit)

int	plot_type	# type of plot
pointer	gt		# gtools descriptor
pointer	fit		# geomap fit parameters

char	params[MAX_PARAMS]
int	npts
real	xrms, yrms

bool	itob()
int	strlen()

begin
	npts = max (0, GM_NPTS(fit) - GM_NREJECT(fit) - GM_NWTS0(fit))
	xrms = max (0.0, GM_XRMS(fit))
	yrms = max (0.0, GM_YRMS(fit))
	if (npts > 1) {
	    xrms = sqrt (xrms / (npts - 1))
	    yrms = sqrt (yrms / (npts - 1))
	} else {
	    xrms = 0.0
	    yrms = 0.0
	}

	# print data parameters
	call sprintf (params, MAX_PARAMS,
	    "GEOMAP: function = %s npts = %d sigrej = %g nrej = %d\n")

	    switch (GM_FUNCTION(fit)) {
	    case GS_LEGENDRE:
		call pargstr ("legendre")
	    case GS_CHEBYSHEV:
		call pargstr ("chebyshev")
	    case GS_POLYNOMIAL:
		call pargstr ("polynomial")
	    }
	    call pargi (GM_NPTS(fit))
	    call pargr (GM_REJECT(fit))
	    call pargi (GM_NREJECT(fit) + GM_NWTS0(fit))

	# print fit parameters
	switch (plot_type) {
	case FIT:

	    call sprintf (params[strlen(params)+1], MAX_PARAMS,
	        "Xfit: xorder = %d yorder = %d xterms = %b rms = %8.3g\n")
		call pargi (GM_XXORDER(fit))
		call pargi (GM_XYORDER(fit))
		call pargb (itob (GM_XXTERMS(fit)))
		call pargr (xrms)

	    call sprintf (params[strlen(params)+1], MAX_PARAMS,
	        "Yfit: xorder = %d yorder = %d xterms = %b rms = %8.3g\n")
		call pargi (GM_YXORDER(fit))
		call pargi (GM_YYORDER(fit))
		call pargb (itob (GM_YXTERMS(fit)))
		call pargr (yrms)

	case XXRESID, XYRESID:

	    call sprintf (params[strlen(params)+1], MAX_PARAMS,
	        "Xfit: xorder = %d yorder = %d xterms = %b rms = %8.3g\n")
		call pargi (GM_XXORDER(fit))
		call pargi (GM_XYORDER(fit))
		call pargb (itob (GM_XXTERMS(fit)))
		call pargr (xrms)

	case YXRESID, YYRESID:

	    call sprintf (params[strlen(params)+1], MAX_PARAMS,
	        "Yfit: xorder = %d yorder = %d xterms = %b rms = %8.3g\n")
		call pargi (GM_YXORDER(fit))
		call pargi (GM_YYORDER(fit))
		call pargb (itob (GM_YXTERMS(fit)))
		call pargr (yrms)

	default:

	    # do nothing gracefully
	}

	call gt_sets (gt, GTPARAMS, params)
end


# GEOGTSET -- procedure to label axes and titles

procedure geogtset (plot_type, gt, fit)

int	plot_type	# plot type
pointer	gt		# plot descriptor
pointer	fit		# fit descriptor

char	str[SZ_LINE]
int	nchars

int	gstrcpy()

begin
	nchars =  gstrcpy (GM_NAME(fit), str, SZ_LINE)

	switch (plot_type) {
	case FIT:

	    call strcpy (": Geometric Transformation", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    call gt_sets (gt, GTXLABEL, "X(in)")
	    call gt_sets (gt, GTYLABEL, "Y(in)") 

	case XXRESID:

	    call strcpy (": X fit Residuals", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    call gt_sets (gt, GTXLABEL, "X(ref)")
	    call gt_sets (gt, GTYLABEL, "X(in) Residuals") 

	case XYRESID:

	    call strcpy (": X fit Residuals", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    call gt_sets (gt, GTXLABEL, "Y(ref)")
	    call gt_sets (gt, GTYLABEL, "X(in) Residuals") 

	case YXRESID:

	    call strcpy (": Y fit Residuals", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    call gt_sets (gt, GTXLABEL, "X(ref)")
	    call gt_sets (gt, GTYLABEL, "Y(in) Residuals") 

	case YYRESID:

	    call strcpy (": Y fit Residuals", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    call gt_sets (gt, GTXLABEL, "Y(ref)")
	    call gt_sets (gt, GTYLABEL, "Y(in) Residuals") 

	default:
	    
	    # do nothing gracefully
	}
end

# LINCOEFF -- Procedure to print the coefficents of the linear portion of the
# fit, xshift, yshift, xexpansion, yexpansion, x and y rotations.

procedure lincoeff (fit, sx, sy, xshift, yshift, xscale, yscale, xrot, yrot)

pointer	fit		# pointer to the fit structure
pointer	sx		# pointer to the x surface fit
pointer	sy		# pointer to the y surface fit
real	xshift		# output x shift
real	yshift		# output y shift
real	xscale		# output x scale
real	yscale		# output y scale
real	xrot		# rotation of point on x axis
real	yrot		# rotation of point on y axis

int	nxxcoeff, nxycoeff, nyxcoeff, nyycoeff
pointer	sp, xcoeff, ycoeff
real	xxrange, xyrange, xxmaxmin, xymaxmin
real	yxrange, yyrange, yxmaxmin, yymaxmin
real	a, b, c, d

bool	fp_equalr()
int	gsgeti()
real	gsgetr()

begin
	# allocate working space
	call smark (sp)
	call salloc (xcoeff, gsgeti (sx, GSNCOEFF), TY_REAL)
	call salloc (ycoeff, gsgeti (sy, GSNCOEFF), TY_REAL)

	# get coefficients and numbers of coefficients
	call gscoeff (sx, Memr[xcoeff], nxxcoeff)
	call gscoeff (sy, Memr[ycoeff], nyycoeff)
	nxxcoeff = gsgeti (sx, GSNXCOEFF)
	nxycoeff = gsgeti (sx, GSNYCOEFF)
	nyxcoeff = gsgeti (sy, GSNXCOEFF)
	nyycoeff = gsgeti (sy, GSNYCOEFF)

	# get the data range
	if (gsgeti (sx, GSTYPE) != GS_POLYNOMIAL) {
	    xxrange = (gsgetr (sx, GSXMAX) - gsgetr (sx, GSXMIN)) / 2.0
	    xxmaxmin = - (gsgetr (sx, GSXMAX) + gsgetr (sx, GSXMIN)) / 2.0
	    xyrange = (gsgetr (sx, GSYMAX) - gsgetr (sx, GSYMIN)) / 2.0
	    xymaxmin = - (gsgetr (sx, GSYMAX) + gsgetr (sx, GSYMIN)) / 2.0
	} else {
	    xxrange = 1.0
	    xxmaxmin = 0.0
	    xyrange = 1.0
	    xymaxmin = 0.0
	}
	if (gsgeti (sy, GSTYPE) != GS_POLYNOMIAL) {
	    yxrange = (gsgetr (sy, GSXMAX) - gsgetr (sy, GSXMIN)) / 2.0
	    yxmaxmin = - (gsgetr (sy, GSXMAX) + gsgetr (sy, GSXMIN)) / 2.0
	    yyrange = (gsgetr (sy, GSYMAX) - gsgetr (sy, GSYMIN)) / 2.0
	    yymaxmin = - (gsgetr (sy, GSYMAX) + gsgetr (sy, GSYMIN)) / 2.0
	} else {
	    yxrange = 1.0
	    yxmaxmin = 0.0
	    yyrange = 1.0
	    yymaxmin = 0.0
	}

	# get the shifts
	xshift = Memr[xcoeff] + Memr[xcoeff+1] * xxmaxmin / xxrange +
	    Memr[xcoeff+2] * xymaxmin / xyrange
	yshift = Memr[ycoeff] + Memr[ycoeff+1] * yxmaxmin / yxrange +
	    Memr[ycoeff+2] * yymaxmin / yyrange

        # Get the rotation and scaling parameters and correct for normalization.
        if (nxxcoeff > 1)
            a = Memr[xcoeff+1] / xxrange
        else
            a = 0.0
        if (nxycoeff > 1)
            b = Memr[xcoeff+nxxcoeff] / xyrange
        else
            b = 0.0
        if (nyxcoeff > 1)
            c = Memr[ycoeff+1] / yxrange
        else
            c = 0.0
        if (nyycoeff > 1)
            d = Memr[ycoeff+nyxcoeff] / yyrange
        else
            d = 0.0

        # Get the magnification factors.
        xscale = sqrt (a * a + c * c)
        yscale = sqrt (b * b + d * d)

        # Get the x and y axes rotation factors.
        if (fp_equalr (a, 0.0) && fp_equalr (c, 0.0))
            xrot = 0.0
        else
            xrot = RADTODEG (atan2 (-c, a))
        if (xrot < 0.0)
            xrot = xrot + 360.0

        if (fp_equalr (b, 0.0) && fp_equalr (d, 0.0))
            yrot = 0.0
        else
            yrot = RADTODEG (atan2 (b, d))
        if (yrot < 0.0)
            yrot = yrot + 360.0

	call sfree (sp)
end

# LINCOEFFD -- Procedure to print the coefficents of the linear portion of the
# fit, xshift, yshift, xexpansion, yexpansion, x and y rotations.

procedure lincoeffd (fit, sx, sy, xshift, yshift, xscale, yscale, xrot, yrot)

pointer	fit		# pointer to the fit structure
pointer	sx		# pointer to the x surface fit
pointer	sy		# pointer to the y surface fit
double	xshift		# output x shift
double	yshift		# output y shift
double	xscale		# output x scale
double	yscale		# output y scale
double	xrot		# rotation of point on x axis
double	yrot		# rotation of point on y axis

int	nxxcoeff, nxycoeff, nyxcoeff, nyycoeff
pointer	sp, xcoeff, ycoeff
double	xxrange, xyrange, xxmaxmin, xymaxmin
double	yxrange, yyrange, yxmaxmin, yymaxmin
double	a, b, c, d

bool	fp_equald()
int	dgsgeti()
double	dgsgetd()

begin
	# allocate working space
	call smark (sp)
	call salloc (xcoeff, dgsgeti (sx, GSNCOEFF), TY_DOUBLE)
	call salloc (ycoeff, dgsgeti (sy, GSNCOEFF), TY_DOUBLE)

	# get coefficients and numbers of coefficients
	call dgscoeff (sx, Memd[xcoeff], nxxcoeff)
	call dgscoeff (sy, Memd[ycoeff], nyycoeff)
	nxxcoeff = dgsgeti (sx, GSNXCOEFF)
	nxycoeff = dgsgeti (sx, GSNYCOEFF)
	nyxcoeff = dgsgeti (sy, GSNXCOEFF)
	nyycoeff = dgsgeti (sy, GSNYCOEFF)

	# get the data range
	if (dgsgeti (sx, GSTYPE) != GS_POLYNOMIAL) {
	    xxrange = (dgsgetd (sx, GSXMAX) - dgsgetd (sx, GSXMIN)) / 2.0d0
	    xxmaxmin = - (dgsgetd (sx, GSXMAX) + dgsgetd (sx, GSXMIN)) / 2.0d0
	    xyrange = (dgsgetd (sx, GSYMAX) - dgsgetd (sx, GSYMIN)) / 2.0d0
	    xymaxmin = (dgsgetd (sx, GSYMAX) + dgsgetd (sx, GSYMIN)) / 2.0d0
	} else {
	    xxrange = 1.0d0
	    xxmaxmin = 0.0d0
	    xyrange = 1.0d0
	    xymaxmin = 0.0d0
	}
	if (dgsgeti (sy, GSTYPE) != GS_POLYNOMIAL) {
	    yxrange = (dgsgetd (sx, GSXMAX) - dgsgetd (sx, GSXMIN)) / 2.0d0
	    yxmaxmin = - (dgsgetd (sx, GSXMAX) + dgsgetd (sx, GSXMIN)) / 2.0d0
	    yyrange = (dgsgetd (sx, GSYMAX) - dgsgetd (sx, GSYMIN)) / 2.0d0
	    yymaxmin = (dgsgetd (sx, GSYMAX) + dgsgetd (sx, GSYMIN)) / 2.0d0
	} else {
	    yxrange = 1.0d0
	    yxmaxmin = 0.0d0
	    yyrange = 1.0d0
	    yymaxmin = 0.0d0
	}

	# get the shifts
	xshift = Memd[xcoeff] + Memd[xcoeff+1] * xxmaxmin / xxrange -
	    Memd[xcoeff+2] * xymaxmin / xyrange
	yshift = Memd[ycoeff] + Memd[ycoeff+1] * yxmaxmin / yxrange -
	    Memd[ycoeff+2] * yymaxmin / yyrange

        # Get the rotation and scaling parameters and correct for normalization.
        if (nxxcoeff > 1)
            a = Memd[xcoeff+1] / xxrange
        else
            a = 0.0d0
        if (nxycoeff > 1)
            b = Memd[xcoeff+nxxcoeff] / xyrange
        else
            b = 0.0d0
        if (nyxcoeff > 1)
            c = Memd[ycoeff+1] / yxrange
        else
            c = 0.0d0
        if (nyycoeff > 1)
            d = Memd[ycoeff+nyxcoeff] / yyrange
        else
            d = 0.0d0

        # Get the magnification factors.
        xscale = sqrt (a * a + c * c)
        yscale = sqrt (b * b + d * d)

        # Get the x and y axes rotation factors.
        if (fp_equald (a, 0.0d0) && fp_equald (c, 0.0d0))
            xrot = 0.0d0
        else
            xrot = RADTODEG (atan2 (-c, a))
        if (xrot < 0.0d0)
            xrot = xrot + 360.0d0

        if (fp_equald (b, 0.0d0) && fp_equald (d, 0.0d0))
            yrot = 0.0d0
        else
            yrot = RADTODEG (atan2 (b, d))
        if (yrot < 0.0d0)
            yrot = yrot + 360.0d0

	call sfree (sp)
end

# GEOMKCOF -- Procedure to alter the coefficients of the surface

procedure geomkcof (sx1, sy1, xscale, yscale, xrotation, yrotation)

pointer	sx1, sy1		# pointer to the linear surface
real	xscale, yscale		# x and y scales
real	xrotation,yrotation	# rotation angle radians

int	ncoeff
pointer	sp, xcoeff, ycoeff
real	cosx, sinx, cosy, siny, xrange, yrange

int	gsgeti()
real	gsgetr()

begin
	# extract original coefficients
	call smark (sp)
	ncoeff = max (gsgeti (sx1, GSNSAVE), gsgeti (sy1, GSNSAVE))
	call salloc (xcoeff, ncoeff, TY_REAL)
	call salloc (ycoeff, ncoeff, TY_REAL)
	call gssave (sx1, Memr[xcoeff])
	call gssave (sy1, Memr[ycoeff])

	# define the scaling parameters
	cosx = cos (DEGTORAD (xrotation))
	sinx = sin (DEGTORAD (xrotation))
	cosy = cos (DEGTORAD (yrotation))
	siny = sin (DEGTORAD (yrotation))

	# calculate coefficients
	Memr[xcoeff+GS_SAVECOEFF+1] =  xscale * cosx
	Memr[xcoeff+GS_SAVECOEFF+2] =  yscale * siny
	Memr[ycoeff+GS_SAVECOEFF+1] = -xscale * sinx
	Memr[ycoeff+GS_SAVECOEFF+2] =  yscale * cosy

	# normalize coefficients for non polynomial functions
	xrange = gsgetr (sx1, GSXMAX) - gsgetr (sx1, GSXMIN)
	yrange = gsgetr (sy1, GSYMAX) - gsgetr (sy1, GSYMIN)
	if (gsgeti (sx1, GSTYPE) != GS_POLYNOMIAL) {
	    Memr[xcoeff+GS_SAVECOEFF+1] = Memr[xcoeff+GS_SAVECOEFF+1] *
	        xrange / 2.
	    Memr[xcoeff+GS_SAVECOEFF+2] = Memr[xcoeff+GS_SAVECOEFF+2] *
	        yrange / 2.
	}
	if (gsgeti (sy1, GSTYPE) != GS_POLYNOMIAL) {
	    Memr[ycoeff+GS_SAVECOEFF+1] = Memr[ycoeff+GS_SAVECOEFF+1] *
	        xrange / 2.
	    Memr[ycoeff+GS_SAVECOEFF+2] = Memr[ycoeff+GS_SAVECOEFF+2] *
	        yrange / 2.
	}

	# free original fit
	call gsfree (sx1)
	call gsfree (sy1)

	# restore fit
	call gsrestore (sx1, Memr[xcoeff])
	call gsrestore (sy1, Memr[ycoeff])

	call sfree (sp)
end

# GEOMKCOFD -- Procedure to alter the coefficients of the surface

procedure geomkcofd (sx1, sy1, xscale, yscale, xrotation, yrotation)

pointer	sx1, sy1		# pointer to the linear surface
double	xscale, yscale		# x and y scales
double	xrotation,yrotation	# rotation angle radians

int	ncoeff
pointer	sp, xcoeff, ycoeff
double	cosx, sinx, cosy, siny, xrange, yrange

int	dgsgeti()
double	dgsgetd()

begin
	# extract original coefficients
	call smark (sp)
	ncoeff = max (dgsgeti (sx1, GSNSAVE), dgsgeti (sy1, GSNSAVE))
	call salloc (xcoeff, ncoeff, TY_DOUBLE)
	call salloc (ycoeff, ncoeff, TY_DOUBLE)
	call dgssave (sx1, Memd[xcoeff])
	call dgssave (sy1, Memd[ycoeff])

	# define the scaling parameters
	cosx = cos (DEGTORAD (xrotation))
	sinx = sin (DEGTORAD (xrotation))
	cosy = cos (DEGTORAD (yrotation))
	siny = sin (DEGTORAD (yrotation))

	# calculate coefficients
	Memd[xcoeff+GS_SAVECOEFF+1] =  xscale * cosx
	Memd[xcoeff+GS_SAVECOEFF+2] =  yscale * siny
	Memd[ycoeff+GS_SAVECOEFF+1] = -xscale * sinx
	Memd[ycoeff+GS_SAVECOEFF+2] =  yscale * cosy

	# normalize coefficients for non polynomial functions
	xrange = dgsgetd (sx1, GSXMAX) - dgsgetd (sx1, GSXMIN)
	yrange = dgsgetd (sy1, GSYMAX) - dgsgetd (sy1, GSYMIN)
	if (dgsgeti (sx1, GSTYPE) != GS_POLYNOMIAL) {
	    Memd[xcoeff+GS_SAVECOEFF+1] = Memd[xcoeff+GS_SAVECOEFF+1] *
	        xrange / 2.0d0
	    Memd[xcoeff+GS_SAVECOEFF+2] = Memd[xcoeff+GS_SAVECOEFF+2] *
	        yrange / 2.0d0
	}
	if (dgsgeti (sy1, GSTYPE) != GS_POLYNOMIAL) {
	    Memd[ycoeff+GS_SAVECOEFF+1] = Memd[ycoeff+GS_SAVECOEFF+1] *
	        xrange / 2.0d0
	    Memd[ycoeff+GS_SAVECOEFF+2] = Memd[ycoeff+GS_SAVECOEFF+2] *
	        yrange / 2.0d0
	}

	# free original fit
	call dgsfree (sx1)
	call dgsfree (sy1)

	# restore fit
	call dgsrestore (sx1, Memd[xcoeff])
	call dgsrestore (sy1, Memd[ycoeff])

	call sfree (sp)
end
