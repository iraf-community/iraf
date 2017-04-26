# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include <pkg/gtools.h>
include <mach.h>
include <math.h>
include <gset.h>
include "geomap.h"
include "geogmap.h"

define	MAX_PARAMS	(10 * SZ_LINE)	
define	NINTERVALS	5
define	NGRAPH		100



# GEO_LABEL -- Annotate the plot.

procedure geo_label (plot_type, gt, fit)

int	plot_type	#I type of plot
pointer	gt		#I gtools descriptor
pointer	fit		#I geomap fit parameters

int	npts
pointer	sp, params, xtermlab, ytermlab
real	xrms, yrms, rej
int	strlen(), rg_wrdstr()

begin
	call smark (sp)
	call salloc (params, MAX_PARAMS, TY_CHAR)
	call salloc (xtermlab, SZ_FNAME, TY_CHAR)
	call salloc (ytermlab, SZ_FNAME, TY_CHAR)

	npts = max (0, GM_NPTS(fit) - GM_NWTS0(fit))
	xrms = max (0.0d0, GM_XRMS(fit))
	yrms = max (0.0d0, GM_YRMS(fit))
	if (npts > 1) {
	    xrms = sqrt (xrms / (npts - 1))
	    yrms = sqrt (yrms / (npts - 1))
	} else {
	    xrms = 0.0
	    yrms = 0.0
	}
	if (IS_INDEFD(GM_REJECT(fit)))
	    rej = INDEFR
	else if (GM_REJECT(fit) > MAX_REAL)
	    rej = INDEFR
	else
	    rej = GM_REJECT(fit)

	# Print data parameters.
	if (GM_PROJECTION(fit) == GM_NONE)
	    call sprintf (Memc[params], MAX_PARAMS,
	        "GEOMAP: function = %s npts = %d reject = %g nrej = %d\n")
	else
	    call sprintf (Memc[params], MAX_PARAMS,
	        "CCMAP: function = %s npts = %d reject = %g nrej = %d\n")

	    switch (GM_FUNCTION(fit)) {
	    case GS_LEGENDRE:
		call pargstr ("legendre")
	    case GS_CHEBYSHEV:
		call pargstr ("chebyshev")
	    case GS_POLYNOMIAL:
		call pargstr ("polynomial")
	    }
	    call pargi (GM_NPTS(fit))
	    call pargr (rej)
	    call pargi (GM_NWTS0(fit))

	# Print fit parameters.
	switch (plot_type) {
	case FIT:

	    if (rg_wrdstr ((GM_XXTERMS(fit) + 1), Memc[xtermlab], SZ_FNAME,
	        GM_XFUNCS) <= 0)
		call strcpy ("none", Memc[xtermlab], SZ_FNAME)
	    if (rg_wrdstr ((GM_YXTERMS(fit) + 1), Memc[ytermlab], SZ_FNAME,
	        GM_XFUNCS) <= 0)
		call strcpy ("none", Memc[ytermlab], SZ_FNAME)
	    if (GM_PROJECTION(fit) == GM_NONE)
	        call sprintf (Memc[params+strlen(Memc[params])], MAX_PARAMS,
	        "X fit: xorder = %d yorder = %d xterms = %s stdev = %8.3g\n")
	    else
	        call sprintf (Memc[params+strlen(Memc[params])], MAX_PARAMS,
	   "XI fit: xorder = %d yorder = %d xterms = %s stdev = %8.3g arcsec\n")
		call pargi (GM_XXORDER(fit))
		call pargi (GM_XYORDER(fit))
		call pargstr (Memc[xtermlab])
		call pargr (xrms)

	    if (GM_PROJECTION(fit) == GM_NONE)
	        call sprintf (Memc[params+strlen(Memc[params])], MAX_PARAMS,
	        "Y fit: xorder = %d yorder = %d xterms = %s stdev = %8.3g\n")
	    else
	        call sprintf (Memc[params+strlen(Memc[params])], MAX_PARAMS,
	  "ETA fit: xorder = %d yorder = %d xterms = %s stdev = %8.3g arcsec\n")
		call pargi (GM_YXORDER(fit))
		call pargi (GM_YYORDER(fit))
		call pargstr (Memc[ytermlab])
		call pargr (yrms)

	case XXRESID, XYRESID:

	    if (rg_wrdstr ((GM_XXTERMS(fit) + 1), Memc[xtermlab], SZ_FNAME,
	        GM_XFUNCS) <= 0)
		call strcpy ("none", Memc[xtermlab], SZ_FNAME)
	    if (GM_PROJECTION(fit) == GM_NONE)
	        call sprintf (Memc[params+strlen(Memc[params])], MAX_PARAMS,
	        "X fit: xorder = %d yorder = %d xterms = %s rms = %8.3g\n")
	    else
	        call sprintf (Memc[params+strlen(Memc[params])], MAX_PARAMS,
	    "XI fit: xorder = %d yorder = %d xterms = %s rms = %8.3g arcsec\n")
		call pargi (GM_XXORDER(fit))
		call pargi (GM_XYORDER(fit))
		call pargstr (Memc[xtermlab])
		call pargr (xrms)

	case YXRESID, YYRESID:

	    if (rg_wrdstr ((GM_YXTERMS(fit) + 1), Memc[ytermlab], SZ_FNAME,
	        GM_XFUNCS) <= 0)
		call strcpy ("none", Memc[ytermlab], SZ_FNAME)
	    if (GM_PROJECTION(fit) == GM_NONE)
	        call sprintf (Memc[params+strlen(Memc[params])], MAX_PARAMS,
	        "Y fit: xorder = %d yorder = %d xterms = %s rms = %8.3g\n")
	    else
	        call sprintf (Memc[params+strlen(Memc[params])], MAX_PARAMS,
	    "ETA fit: xorder = %d yorder = %d xterms = %s rms = %8.3g arcsec\n")
		call pargi (GM_YXORDER(fit))
		call pargi (GM_YYORDER(fit))
		call pargstr (Memc[ytermlab])
		call pargr (yrms)

	default:

	    # do nothing gracefully
	}

	call gt_sets (gt, GTPARAMS, Memc[params])

	call sfree (sp)
end


# GEO_GTSET -- Write title and labels.

procedure geo_gtset (plot_type, gt, fit)

int	plot_type	#I plot type
pointer	gt		#I plot descriptor
pointer	fit		#I fit descriptor

char	str[SZ_LINE]
int	nchars
int	gstrcpy()

begin
	nchars =  gstrcpy (GM_RECORD(fit), str, SZ_LINE)

	switch (plot_type) {
	case FIT:

	    if (GM_PROJECTION(fit) == GM_NONE)
	        call strcpy (": Coordinate Transformation", str[nchars+1],
		    SZ_LINE)
	    else
	        call strcpy (": Celestial Coordinate Transformation",
		    str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    if (GM_PROJECTION(fit) == GM_NONE) {
	        call gt_sets (gt, GTXLABEL, "X (in units)")
	        call gt_sets (gt, GTYLABEL, "Y (in units)") 
	    } else {
	        call gt_sets (gt, GTXLABEL, "XI (arcsec)")
	        call gt_sets (gt, GTYLABEL, "ETA (arcsec)") 
	    }

	case XXRESID:

	    if (GM_PROJECTION(fit) == GM_NONE)
	        call strcpy (": X fit Residuals", str[nchars+1], SZ_LINE)
	    else
	        call strcpy (": XI fit Residuals", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    if (GM_PROJECTION(fit) == GM_NONE) {
	        call gt_sets (gt, GTXLABEL, "X (ref units)")
	        call gt_sets (gt, GTYLABEL, "X Residuals (in units)") 
	    } else {
	        call gt_sets (gt, GTXLABEL, "X (pixels)")
	        call gt_sets (gt, GTYLABEL, "XI Residuals (arcsec)") 
	    }

	case XYRESID:

	    if (GM_PROJECTION(fit) == GM_NONE)
	        call strcpy (": X fit Residuals", str[nchars+1], SZ_LINE)
	    else
	        call strcpy (": XI fit Residuals", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    if (GM_PROJECTION(fit) == GM_NONE) {
	        call gt_sets (gt, GTXLABEL, "Y (ref units)")
	        call gt_sets (gt, GTYLABEL, "X Residuals (in units)") 
	    } else {
	        call gt_sets (gt, GTXLABEL, "Y (pixels)")
	        call gt_sets (gt, GTYLABEL, "XI Residuals (arcsec)") 
	    }

	case YXRESID:

	    if (GM_PROJECTION(fit) == GM_NONE)
	        call strcpy (": Y fit Residuals", str[nchars+1], SZ_LINE)
	    else
	        call strcpy (": ETA fit Residuals", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    if (GM_PROJECTION(fit) == GM_NONE) {
	        call gt_sets (gt, GTXLABEL, "X (ref units)")
	        call gt_sets (gt, GTYLABEL, "Y (Residuals (in units)") 
	    } else {
	        call gt_sets (gt, GTXLABEL, "X (pixels)")
	        call gt_sets (gt, GTYLABEL, "ETA Residuals (arcsec)") 
	    }

	case YYRESID:

	    if (GM_PROJECTION(fit) == GM_NONE)
	        call strcpy (": Y fit Residuals", str[nchars+1], SZ_LINE)
	    else
	        call strcpy (": ETA fit Residuals", str[nchars+1], SZ_LINE)
	    call gt_sets (gt, GTTITLE, str)
	    if (GM_PROJECTION(fit) == GM_NONE) {
	        call gt_sets (gt, GTXLABEL, "Y (ref units)")
	        call gt_sets (gt, GTYLABEL, "Y Residuals (in units)") 
	    } else {
	        call gt_sets (gt, GTXLABEL, "Y (pixels)")
	        call gt_sets (gt, GTYLABEL, "ETA Residuals (arcsec)") 
	    }

	default:
	    
	    # do nothing gracefully
	}
end


# GEO_COLON -- Process the colon commands.

procedure geo_colon (gd, fit, gfit, cmdstr, newgraph)

pointer	gd		#I graphics stream
pointer	fit		#I pointer to fit structure
pointer	gfit		#I pointer to the gfit structure
char	cmdstr[ARB]	#I command string
int	newgraph	#I plot new graph

int	ncmd, ival
pointer	sp, str, cmd
real	rval
int	nscan(), strdic(), rg_wrdstr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (nscan() == 0) {
	    call sfree (sp)
	    return
	}

	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GM_CMDS)
	switch (ncmd) {
	case GMCMD_SHOW:
	    call gdeactivate (gd, AW_CLEAR)
	    call printf ("Current Fitting Parameters\n\n")
	    if (GM_PROJECTION(fit) != GM_NONE) {
	        if (rg_wrdstr (GM_PROJECTION(fit), Memc[str], SZ_FNAME,
	            GM_PROJLIST) <= 0)
		    ;
	        call printf ("\tprojection = %s\n")
		    call pargstr (Memc[str])
	        call printf ("\tlngref = %h\n")
		    call pargd (GM_XREFPT(fit))
	        call printf ("\tlatref = %h\n")
		    call pargd (GM_YREFPT(fit))
	    }
	    if (rg_wrdstr (GM_FIT(fit), Memc[str], SZ_FNAME,
	        GM_GEOMETRIES) <= 0)
		call strcpy ("general", Memc[str], SZ_FNAME)
	    call printf ("\tfitgeometry = %s\n")
		call pargstr (Memc[str])
	    if (rg_wrdstr (GM_FUNCTION(fit), Memc[str], SZ_FNAME,
	        GM_FUNCS) <= 0)
		call strcpy ("polynomial", Memc[str], SZ_FNAME)
	    call printf ("\tfunction = %s\n")
		Call pargstr (Memc[str])
	    call printf ("\txxorder = %d\n")
		call pargi (GM_XXORDER(fit))
	    call printf ("\txyorder = %d\n")
		call pargi (GM_XYORDER(fit))
	    if (rg_wrdstr ((GM_XXTERMS(fit) + 1), Memc[str], SZ_FNAME,
	        GM_XFUNCS) <= 0)
		call strcpy ("none", Memc[str], SZ_FNAME)
	    call printf ("\txxterms = %s\n")
		call pargstr (Memc[str])
	    call printf ("\tyxorder = %d\n")
		call pargi (GM_YXORDER(fit))
	    call printf ("\tyyorder = %d\n")
		call pargi (GM_YYORDER(fit))
	    if (rg_wrdstr ((GM_YXTERMS(fit) + 1), Memc[str], SZ_FNAME,
	        GM_XFUNCS) <= 0)
		call strcpy ("none", Memc[str], SZ_FNAME)
	    call printf ("\tyxterms = %s\n")
		call pargstr (Memc[str])
	    if (IS_INDEFD(GM_REJECT(fit)))
		rval = INDEFR
	    else if (GM_REJECT(fit) > MAX_REAL)
		rval = INDEFR
	    else
		rval = GM_REJECT(fit)
	    call printf ("\treject = %f\n")
		call pargr (rval)
	    call greactivate (gd, AW_PAUSE)

	case GMCMD_PROJECTION:
	    if (rg_wrdstr (GM_PROJECTION(fit), Memc[str], SZ_FNAME,
	        GM_PROJLIST) <= 0)
	        call strcpy ("INDEF", Memc[str], SZ_FNAME)
	    call printf ("projection = %s\n")
	        call pargstr (Memc[str])

	case GMCMD_REFPOINT:
	    call printf ("lngref = %h latref = %h\n")
		call pargd (GM_XREFPT(fit))
		call pargd (GM_YREFPT(fit))

	case GMCMD_GEOMETRY:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan () == 1) {
	        if (rg_wrdstr (GM_FIT(fit), Memc[str], SZ_FNAME,
	            GM_GEOMETRIES) <= 0)
		    call strcpy ("general", Memc[str], SZ_FNAME)
	        call printf ("fitgeometry = %s\n")
		    call pargstr (Memc[str])
	    } else {
		ival = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GM_GEOMETRIES)
		if (ival > 0) {
		    GM_FIT(fit) = ival
		    GG_NEWFUNCTION(gfit) = YES
		    GG_FITERROR(gfit) = NO
		}
	    }

	case GMCMD_FUNCTION:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan () == 1) {
	        if (rg_wrdstr (GM_FUNCTION(fit), Memc[str], SZ_FNAME,
	            GM_FUNCS) <= 0)
		    call strcpy ("polynomial", Memc[str], SZ_FNAME)
	        call printf ("function = %s\n")
		    call pargstr (Memc[str])
	    } else {
		ival = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GM_FUNCS)
		if (ival > 0) {
		    GM_FUNCTION(fit) = ival
		    GG_NEWFUNCTION(gfit) = YES
		    GG_FITERROR(gfit) = NO
		}
	    }

	case GMCMD_ORDER:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf (
		    "xxorder = %d xyorder = %d yxorder = %d yyorder = %d\n")
		    call pargi (GM_XXORDER(fit))
		    call pargi (GM_XYORDER(fit))
		    call pargi (GM_YXORDER(fit))
		    call pargi (GM_YYORDER(fit))
	    } else {
		GM_XXORDER(fit) = max (ival, 2)
		GM_XYORDER(fit) = max (ival, 2)
		GM_YXORDER(fit) = max (ival, 2)
		GM_YYORDER(fit) = max (ival, 2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }

	case GMCMD_XXORDER:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("xxorder = %d\n")
		    call pargi (GM_XXORDER(fit))
	    } else {
		GM_XXORDER(fit) = max (ival, 2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }

	case GMCMD_XYORDER:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("xyorder = %d\n")
		    call pargi (GM_XYORDER(fit))
	    } else {
		GM_XYORDER(fit) = max (ival,2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }

	case GMCMD_YXORDER:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("yxorder = %d\n")
		    call pargi (GM_YXORDER(fit))
	    } else {
		GM_YXORDER(fit) = max (ival, 2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }

	case GMCMD_YYORDER:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("yyorder = %d\n")
		    call pargi (GM_YYORDER(fit))
	    } else {
		GM_YYORDER(fit) = max (ival, 2)
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }

	case GMCMD_XXTERMS:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan () == 1) {
	        if (rg_wrdstr ((GM_XXTERMS(fit) + 1), Memc[str], SZ_FNAME,
	            GM_XFUNCS) <= 0)
		    call strcpy ("none", Memc[str], SZ_FNAME)
	        call printf ("xxterms = %s\n")
		    call pargstr (Memc[str])
	    } else {
		ival = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GM_XFUNCS)
		if (ival > 0) {
		    GM_XXTERMS(fit) = ival - 1
		    GG_NEWFUNCTION(gfit) = YES
		    GG_FITERROR(gfit) = NO
		}
	    }

	case GMCMD_YXTERMS:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan () == 1) {
	        if (rg_wrdstr ((GM_YXTERMS(fit) + 1), Memc[str], SZ_FNAME,
	            GM_XFUNCS) <= 0)
		    call strcpy ("none", Memc[str], SZ_FNAME)
	        call printf ("yxterms = %s\n")
		    call pargstr (Memc[str])
	    } else {
		ival = strdic (Memc[cmd], Memc[cmd], SZ_LINE, GM_XFUNCS)
		if (ival > 0) {
		    GM_YXTERMS(fit) = ival - 1
		    GG_NEWFUNCTION(gfit) = YES
		    GG_FITERROR(gfit) = NO
		}
	    }

	case GMCMD_REJECT:
	    call gargr (rval)
	    if (nscan() == 1) {
		if (IS_INDEFD(GM_REJECT(fit)))
		    rval = INDEFR
		else if (GM_REJECT(fit) > MAX_REAL)
		    rval = INDEFR
		else
		    rval = GM_REJECT(fit)
		call printf ("reject = %f\n")
		    call pargr (rval)
	    } else {
		GM_REJECT(fit) = rval
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }

	case GMCMD_MAXITER:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("maxiter = %d\n")
		    call pargi (GM_MAXITER(fit))
	    } else {
		GM_MAXITER(fit) = ival
		GG_NEWFUNCTION(gfit) = YES
		GG_FITERROR(gfit) = NO
	    }

	}

	call sfree (sp)
end






# GEO_1DELETE -- Delete a point from the fit.

procedure geo_1deleter (gd, xin, yin, wts, userwts, npts, wx, wy, delete)

pointer	gd		#I pointer to graphics descriptor
real	xin[ARB]	#I x array
real	yin[ARB]	#I y array
real	wts[ARB]	#I array of weights
real	userwts[ARB]	#I array of user weights
int	npts		#I number of points
real	wx, wy		#I world coordinates
int	delete		#I delete points ?

int	i, j, pmltype
real	r2min, r2, x0, y0
int	gstati()

begin
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	r2min = MAX_REAL
	j = 0

	if (delete == YES) {

	    # Search for nearest point that has not been deleted.
	    do i = 1, npts {
	        if (wts[i] <= real(0.0))
		    next
	        call gctran (gd, xin[i], yin[i], x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Mark point and set weights to 0.
	    if (j != 0) {
		call gscur (gd, xin[j], yin[j])
		call gmark (gd, xin[j], yin[j], GM_CROSS, 2., 2.)
		wts[j] = real(0.0)
	    }

	} else {

	    # Search for the nearest deleted point.
	    do i = 1, npts {
	        if (wts[i] > real(0.0))
		    next
	        call gctran (gd, xin[i], yin[i], x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Erase cross and remark with a plus.
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


# GEO_2DELETE -- Delete the residuals.

procedure geo_2deleter (gd, x, resid, wts, userwts, npts, wx, wy, delete)

pointer	gd		#I pointer to graphics descriptor
real	x[ARB]		#I reference x values
real	resid[ARB]	#I residuals
real	wts[ARB]	#I weight array
real	userwts[ARB]	#I user weight array
int	npts		#I number of points
real	wx		#I world x coordinate
real	wy		#I world y coordinate
int	delete		#I delete point

int	i, j, pmltype
real	r2, r2min, x0, y0
int	gstati()

begin
	# Delete the point.
        call gctran (gd, wx, wy, wx, wy, 1, 0)
        r2min = MAX_REAL
        j = 0

	# Delete or add a point.
        if (delete == YES) {

	    # Find the nearest undeleted point.
	    do i = 1, npts {
	        if (wts[i] <= real(0.0))
		    next
	        call gctran (gd, x[i], resid[i], x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Mark the point with a cross and set weight to zero.
	    if (j != 0) {
	        call gscur (gd, x[j], resid[j])
	        call gmark (gd, x[j], resid[j], GM_CROSS, 2., 2.)
	        wts[j] = real(0.0)
	    }

        } else {

	    # Find the nearest deleted point.
	    do i = 1, npts {
	        if (wts[i] > real(0.0))
		    next
	        call gctran (gd, x[i], resid[i], x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Erase the cross and remark with a plus.
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


# GEO_1GRAPH - Procedure to graph the distribution of the data in the x-y
# plane. Rejected points are marked by a ' ' and deleted points are marked
# by a ' '. The shift in position of the data points are indicated by
# vectors. Sample fits of constant x and y are marked on the plots.

procedure geo_1graphr (gd, gt, fit, gfit, xref, yref, xin, yin, wts, npts)

pointer	gd		#I pointer to the graphics device
pointer	gt		#I pointer to the plot descriptor
pointer	fit		#I pointer to the geofit structure
pointer	gfit		#I pointer to the plot structure
real	xref[ARB]	#I x reference values
real	yref[ARB]	#I y reference values
real	xin[ARB]	#I x values
real	yin[ARB]	#I y values
real	wts[ARB]	#I array of weights
int	npts		#I number of points

int	i, j

begin
	# If previous plot different type don't overplot.
	if (GG_PLOTTYPE(gfit) != FIT)
	    GG_OVERPLOT(gfit) = NO

	# If not overplottting start new plot.
	if (GG_OVERPLOT(gfit) == NO) {

	    # Set scale and axes.
	    call gclear (gd)
	    call gascale (gd, xin, npts, 1)
	    call gascale (gd, yin, npts, 2)
	    call gt_swind (gd, gt)
	    call gtlabax (gd, gt)

	    # Mark the data and deleted points.
	    do i = 1, npts {
		if (wts[i] == real(0.0))
		    call gmark (gd, xin[i], yin[i], GM_CROSS, 2., 2.)
		else
		    call gmark (gd, xin[i], yin[i], GM_PLUS, 2., 2.)
	    }

	    call gflush (gd)
	}

	# Mark the rejected points.
	do i = 1, GM_NREJECT(fit) {
	    j = Memi[GM_REJ(fit)+i-1]
	    call gmark (gd, xin[j], yin[j], GM_CIRCLE, 2., 2.)
	}

	call gflush (gd)

	# Reset the status flags
	GG_OVERPLOT(gfit) = NO
end


# GEO_2GRAPH -- Graph the x and y fit residuals versus x or y .

procedure geo_2graphr (gd, gt, fit, gfit, x, resid, wts, npts)

pointer	gd		#I pointer to the graphics device
pointer	gt		#I pointer to the plot descriptor
pointer	fit		#I pointer to geomap structure
pointer	gfit		#I pointer to the plot structure
real	x[ARB]		#I x reference values
real	resid[ARB]	#I residual
real	wts[ARB]	#I array of weights
int	npts		#I number of points

int	i, j
pointer	sp, zero

begin
	# Allocate space.
	call smark (sp)
	call salloc (zero, npts, TY_REAL)
	call amovkr (0.0, Memr[zero], npts)

	# Calculate the residuals.
	if (GG_PLOTTYPE(gfit) == FIT)
	    GG_OVERPLOT(gfit) = NO

	if (GG_OVERPLOT(gfit) == NO) {

	    call gclear (gd)

	    # Set scale and axes.
	    call gascale (gd, x, npts, 1)
	    call gascale (gd, resid, npts, 2)
	    call gt_swind (gd, gt)
	    call gtlabax (gd, gt)

	    call gpline (gd, x, Memr[zero], npts)
	}

	# Graph residuals and mark deleted points.
	if (GG_OVERPLOT(gfit) == NO || GG_NEWFUNCTION(gfit) == YES) {
	    do i = 1, npts {
		if (wts[i] == real(0.0))
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

	# Reset the status flag.
	GG_OVERPLOT(gfit) = NO

	call gflush (gd)
	call sfree (sp)
end


# GEO_CONXY -- Plot a set of default lines of xref = const and yref = const.

procedure geo_conxyr (gd, fit, sx1, sy1, sx2, sy2)

pointer	gd		#I graphics file descriptor
pointer	fit		#I fit descriptor
pointer	sx1, sy1	#I pointer to the linear x and y surface fits
pointer sx2, sy2	#I pointer to the linear x and y surface fits

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

	# Calculate intervals in x and y.
	dx = (GM_XMAX(fit) - GM_XMIN(fit)) / NINTERVALS
	dy = (GM_YMAX(fit) - GM_YMIN(fit)) / (NGRAPH - 1)

	# Set up an array of y values.
	Memr[ytemp] = GM_YMIN(fit)
	do i = 2, NGRAPH
	    Memr[ytemp+i-1] = Memr[ytemp+i-2] + dy

	# Mark lines of constant x.
	xint = GM_XMIN(fit)
	for (i = 1; i <= NINTERVALS + 1; i = i + 1) {

	    # Set the x value.
	    call amovkr (xint, Memr[xtemp], NGRAPH)

	    # X fit.
	    call gsvector (sx1, Memr[xtemp], Memr[ytemp], Memr[xfit1],
	        NGRAPH)
	    if (sx2 != NULL) {
	        call gsvector (sx2, Memr[xtemp], Memr[ytemp], Memr[xfit2],
		    NGRAPH)
	        call aaddr (Memr[xfit1], Memr[xfit2], Memr[xfit1], NGRAPH)
	    }

	    # Y fit.
	    call gsvector (sy1, Memr[xtemp], Memr[ytemp], Memr[yfit1],
	        NGRAPH)
	    if (sy2 != NULL) {
	        call gsvector (sy2, Memr[xtemp], Memr[ytemp], Memr[yfit2],
		    NGRAPH)
	        call aaddr (Memr[yfit1], Memr[yfit2], Memr[yfit1], NGRAPH)
	    }

	    # Plot line of constant x.
	    call gpline (gd, Memr[xfit1], Memr[yfit1], NGRAPH)

	    # Update the x value.
	    xint = xint + dx
	}

	call gflush (gd)

	# Calculate x and y intervals.
	dx = (GM_XMAX(fit) - GM_XMIN(fit)) / (NGRAPH - 1)
	dy = (GM_YMAX(fit) - GM_YMIN(fit)) / NINTERVALS

	# Set up array of x values.
	Memr[xtemp] = GM_XMIN(fit)
	do i = 2, NGRAPH
	    Memr[xtemp+i-1] = Memr[xtemp+i-2] + dx

	# Mark lines of constant y.
	yint = GM_YMIN(fit)
	for (i = 1; i <= NINTERVALS + 1; i = i + 1) {

	    # set the y value
	    call amovkr (yint, Memr[ytemp], NGRAPH)

	    # X fit.
	    call gsvector (sx1, Memr[xtemp], Memr[ytemp], Memr[xfit1],
	        NGRAPH)
	    if (sx2 != NULL) {
	        call gsvector (sx2, Memr[xtemp], Memr[ytemp], Memr[xfit2],
		    NGRAPH)
	        call aaddr (Memr[xfit1], Memr[xfit2], Memr[xfit1], NGRAPH)
	    }


	    # Y fit.
	    call gsvector (sy1, Memr[xtemp], Memr[ytemp], Memr[yfit1],
	        NGRAPH)
	    if (sy2 != NULL) {
	        call gsvector (sy2, Memr[xtemp], Memr[ytemp], Memr[yfit2],
		    NGRAPH)
	        call aaddr (Memr[yfit1], Memr[yfit2], Memr[yfit1], NGRAPH)
	    }

	    # Plot line of constant y.
	    call gpline (gd, Memr[xfit1], Memr[yfit1], NGRAPH)

	    # Update the y value.
	    yint = yint + dy
	}

	call gflush (gd)

	call sfree (sp)
end


# GEO_LXY -- Draw a line of constant x-y.

procedure geo_lxyr (gd, fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin, npts, 
        wx, wy)

pointer	gd		#I pointer to graphics descriptor
pointer	fit		#I pointer to the fit parameters
pointer	sx1		#I pointer to the linear x fit
pointer	sy1		#I pointer to the linear y fit
pointer	sx2		#I pointer to the higher order x fit
pointer	sy2		#I pointer to the higher order y fit
real	xref[ARB]	#I x reference values
real	yref[ARB]	#I y reference values
real	xin[ARB]	#I x input values
real	yin[ARB]	#I y input values
int	npts		#I number of data points
real	wx, wy		#I x and y world coordinates

int	i, j
pointer	sp, xtemp, ytemp, xfit1, yfit1, xfit2, yfit2
real	x0, y0, r2, r2min
real	delta, deltax, deltay
real	gseval()

begin
	# Transform world coordinates.
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	r2min = MAX_REAL
	j = 0

	# Find the nearest data point.
	do i = 1, npts {
	    call gctran (gd, xin[i], yin[i], x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
	        r2min = r2
	        j = i
	    }
	}

	# Fit the line
	if (j != 0) {

	    # Allocate temporary space.
	    call smark (sp)
	    call salloc (xtemp, NGRAPH, TY_REAL)
	    call salloc (ytemp, NGRAPH, TY_REAL)
	    call salloc (xfit1, NGRAPH, TY_REAL)
	    call salloc (yfit1, NGRAPH, TY_REAL)
	    call salloc (xfit2, NGRAPH, TY_REAL)
	    call salloc (yfit2, NGRAPH, TY_REAL)

	    # Compute the deltas.
	    deltax = xin[j] - gseval (sx1, xref[j], yref[j])
	    if (sx2 != NULL)
	        deltax = deltax - gseval (sx2, xref[j], yref[j])
	    deltay = yin[j] - gseval (sy1, xref[j], yref[j])
	    if (sy2 != NULL)
		deltay = deltay - gseval (sy2, xref[j], yref[j])

	    # Set up line of constant x.
	    call amovkr (xref[j], Memr[xtemp], NGRAPH)
	    delta = (GM_YMAX(fit) - GM_YMIN(fit)) / (NGRAPH - 1)
	    Memr[ytemp] = GM_YMIN(fit)
	    do i = 2, NGRAPH
	        Memr[ytemp+i-1] = Memr[ytemp+i-2] + delta

	    # X solution.
	    call gsvector (sx1, Memr[xtemp], Memr[ytemp], Memr[xfit1],
	        NGRAPH)
	    if (sx2 != NULL) {
	        call gsvector (sx2, Memr[xtemp], Memr[ytemp], Memr[xfit2],
		    NGRAPH)
	        call aaddr (Memr[xfit1], Memr[xfit2], Memr[xfit1], NGRAPH)
	    }
	    call aaddkr (Memr[xfit1], deltax, Memr[xfit1], NGRAPH)

	    # Y solution.
	    call gsvector (sy1, Memr[xtemp], Memr[ytemp], Memr[yfit1],
	        NGRAPH)
	    if (sy2 != NULL) {
	        call gsvector (sy2, Memr[xtemp], Memr[ytemp], Memr[yfit2],
		    NGRAPH)
	        call aaddr (Memr[yfit1], Memr[yfit2], Memr[yfit1], NGRAPH)
	    }
	    call aaddkr (Memr[yfit1], deltay, Memr[yfit1], NGRAPH)

	    # Plot line of constant x.
	    call gpline (gd, Memr[xfit1], Memr[yfit1], NGRAPH)
	    call gflush (gd)

	    # Set up line of constant y.
	    call amovkr (yref[j], Memr[ytemp], NGRAPH)
	    delta = (GM_XMAX(fit) - GM_XMIN(fit)) / (NGRAPH - 1)
	    Memr[xtemp] = GM_XMIN(fit)
	    do i = 2, NGRAPH
	        Memr[xtemp+i-1] = Memr[xtemp+i-2] + delta

	    # X fit.
	    call gsvector (sx1, Memr[xtemp], Memr[ytemp], Memr[xfit1],
	        NGRAPH)
	    if (sx2 != NULL) {
	        call gsvector (sx2, Memr[xtemp], Memr[ytemp], Memr[xfit2],
		    NGRAPH)
	        call aaddr (Memr[xfit1], Memr[xfit2], Memr[xfit1], NGRAPH)
	    }
	    call aaddkr (Memr[xfit1], deltax, Memr[xfit1], NGRAPH)

	    # Y fit.
	    call gsvector (sy1, Memr[xtemp], Memr[ytemp], Memr[yfit1],
	        NGRAPH)
	    if (sy2 != NULL) {
	        call gsvector (sy2, Memr[xtemp], Memr[ytemp], Memr[yfit2],
		    NGRAPH)
	        call aaddr (Memr[yfit1], Memr[yfit2], Memr[yfit1], NGRAPH)
	    }
	    call aaddkr (Memr[yfit1], deltay, Memr[yfit1], NGRAPH)

	    # Plot line of constant y.
	    call gpline (gd, Memr[xfit1], Memr[yfit1], NGRAPH)
	    call gflush (gd)

	    # Free space.
	    call sfree (sp)
	}
end


# GEO_GCOEFF -- Print the coefficents of the linear portion of the
# fit, xshift, yshift, 

procedure geo_gcoeffr (sx, sy, xshift, yshift, a, b, c, d)

pointer	sx		#I pointer to the x surface fit
pointer	sy		#I pointer to the y surface fit
real	xshift		#O output x shift
real	yshift		#O output y shift
real	a		#O output x coefficient of x fit
real	b		#O output y coefficient of x fit
real	c		#O output x coefficient of y fit
real	d		#O output y coefficient of y fit

int	nxxcoeff, nxycoeff, nyxcoeff, nyycoeff
pointer	sp, xcoeff, ycoeff
real	xxrange, xyrange, xxmaxmin, xymaxmin
real	yxrange, yyrange, yxmaxmin, yymaxmin

int	gsgeti()
real	gsgetr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (xcoeff, gsgeti (sx, GSNCOEFF), TY_REAL)
	call salloc (ycoeff, gsgeti (sy, GSNCOEFF), TY_REAL)

	# Get coefficients and numbers of coefficients.
	call gscoeff (sx, Memr[xcoeff], nxxcoeff)
	call gscoeff (sy, Memr[ycoeff], nyycoeff)
	nxxcoeff = gsgeti (sx, GSNXCOEFF)
	nxycoeff = gsgeti (sx, GSNYCOEFF)
	nyxcoeff = gsgeti (sy, GSNXCOEFF)
	nyycoeff = gsgeti (sy, GSNYCOEFF)

	# Get the data range.
	if (gsgeti (sx, GSTYPE) != GS_POLYNOMIAL) {
	    xxrange = (gsgetr (sx, GSXMAX) - gsgetr (sx, GSXMIN)) / 2.0
	    xxmaxmin = - (gsgetr (sx, GSXMAX) + gsgetr (sx, GSXMIN)) / 2.0
	    xyrange = (gsgetr (sx, GSYMAX) - gsgetr (sx, GSYMIN)) / 2.0
	    xymaxmin = - (gsgetr (sx, GSYMAX) + gsgetr (sx, GSYMIN)) / 2.0
	} else {
	    xxrange = real(1.0)
	    xxmaxmin = real(0.0)
	    xyrange = real(1.0)
	    xymaxmin = real(0.0)
	}

	if (gsgeti (sy, GSTYPE) != GS_POLYNOMIAL) {
	    yxrange = (gsgetr (sy, GSXMAX) - gsgetr (sy, GSXMIN)) / 2.0
	    yxmaxmin = - (gsgetr (sy, GSXMAX) + gsgetr (sy, GSXMIN)) / 2.0
	    yyrange = (gsgetr (sy, GSYMAX) - gsgetr (sy, GSYMIN)) / 2.0
	    yymaxmin = - (gsgetr (sy, GSYMAX) + gsgetr (sy, GSYMIN)) / 2.0
	} else {
	    yxrange = real(1.0)
	    yxmaxmin = real(0.0)
	    yyrange = real(1.0)
	    yymaxmin = real(0.0)
	}

	# Get the shifts.
	xshift = Memr[xcoeff] + Memr[xcoeff+1] * xxmaxmin / xxrange +
	    Memr[xcoeff+2] * xymaxmin / xyrange
	yshift = Memr[ycoeff] + Memr[ycoeff+1] * yxmaxmin / yxrange +
	    Memr[ycoeff+2] * yymaxmin / yyrange

	# Get the rotation and scaling parameters and correct for normalization.
	if (nxxcoeff > 1)
	    a = Memr[xcoeff+1] / xxrange
	else
	    a = real(0.0)
	if (nxycoeff > 1)
	    b = Memr[xcoeff+nxxcoeff] / xyrange
	else
	    b = real(0.0)
	if (nyxcoeff > 1)
	    c = Memr[ycoeff+1] / yxrange
	else
	    c = real(0.0)
	if (nyycoeff > 1)
	    d = Memr[ycoeff+nyxcoeff] / yyrange
	else
	    d = real(0.0)

	call sfree (sp)
end



# GEO_1DELETE -- Delete a point from the fit.

procedure geo_1deleted (gd, xin, yin, wts, userwts, npts, wx, wy, delete)

pointer	gd		#I pointer to graphics descriptor
double	xin[ARB]	#I x array
double	yin[ARB]	#I y array
double	wts[ARB]	#I array of weights
double	userwts[ARB]	#I array of user weights
int	npts		#I number of points
real	wx, wy		#I world coordinates
int	delete		#I delete points ?

int	i, j, pmltype
real	r2min, r2, x0, y0
int	gstati()

begin
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	r2min = MAX_REAL
	j = 0

	if (delete == YES) {

	    # Search for nearest point that has not been deleted.
	    do i = 1, npts {
	        if (wts[i] <= double(0.0))
		    next
	        call gctran (gd, real (xin[i]), real (yin[i]), x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Mark point and set weights to 0.
	    if (j != 0) {
		call gscur (gd, real(xin[j]), real(yin[j]))
		call gmark (gd, real(xin[j]), real(yin[j]), GM_CROSS, 2., 2.)
		wts[j] = double(0.0)
	    }

	} else {

	    # Search for the nearest deleted point.
	    do i = 1, npts {
	        if (wts[i] > double(0.0))
		    next
	        call gctran (gd, real(xin[i]), real(yin[i]), x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Erase cross and remark with a plus.
	    if (j != 0) {
		call gscur (gd, real(xin[j]), real(yin[j]))
		pmltype = gstati (gd, G_PMLTYPE)
		call gseti (gd, G_PMLTYPE, 0)
		call gmark (gd, real(xin[j]), real(yin[j]), GM_CROSS, 2., 2.)
		call gseti (gd, G_PMLTYPE, pmltype)
		call gmark (gd, real(xin[j]), real(yin[j]), GM_PLUS, 2., 2.)
		wts[j] = userwts[j]
	    }
	}
end


# GEO_2DELETE -- Delete the residuals.

procedure geo_2deleted (gd, x, resid, wts, userwts, npts, wx, wy, delete)

pointer	gd		#I pointer to graphics descriptor
double	x[ARB]		#I reference x values
double	resid[ARB]	#I residuals
double	wts[ARB]	#I weight array
double	userwts[ARB]	#I user weight array
int	npts		#I number of points
real	wx		#I world x coordinate
real	wy		#I world y coordinate
int	delete		#I delete point

int	i, j, pmltype
real	r2, r2min, x0, y0
int	gstati()

begin
	# Delete the point.
        call gctran (gd, wx, wy, wx, wy, 1, 0)
        r2min = MAX_REAL
        j = 0

	# Delete or add a point.
        if (delete == YES) {

	    # Find the nearest undeleted point.
	    do i = 1, npts {
	        if (wts[i] <= double(0.0))
		    next
	        call gctran (gd, real(x[i]), real(resid[i]), x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Mark the point with a cross and set weight to zero.
	    if (j != 0) {
	        call gscur (gd, real(x[j]), real(resid[j]))
	        call gmark (gd, real(x[j]), real(resid[j]), GM_CROSS, 2., 2.)
	        wts[j] = double(0.0)
	    }

        } else {

	    # Find the nearest deleted point.
	    do i = 1, npts {
	        if (wts[i] > double(0.0))
		    next
	        call gctran (gd, real(x[i]), real(resid[i]), x0, y0, 1, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Erase the cross and remark with a plus.
	    if (j != 0) {
	        call gscur (gd, real(x[j]), real(resid[j]))
	        pmltype = gstati (gd, G_PMLTYPE)
	        call gseti (gd, G_PMLTYPE, 0)
	        call gmark (gd, real(x[j]), real(resid[j]), GM_CROSS, 2., 2.)
	        call gseti (gd, G_PMLTYPE, pmltype)
	        call gmark (gd, real(x[j]), real(resid[j]), GM_PLUS, 2., 2.)
	        wts[j] = userwts[j]
	    }
        }
end


# GEO_1GRAPH - Procedure to graph the distribution of the data in the x-y
# plane. Rejected points are marked by a ' ' and deleted points are marked
# by a ' '. The shift in position of the data points are indicated by
# vectors. Sample fits of constant x and y are marked on the plots.

procedure geo_1graphd (gd, gt, fit, gfit, xref, yref, xin, yin, wts, npts)

pointer	gd		#I pointer to the graphics device
pointer	gt		#I pointer to the plot descriptor
pointer	fit		#I pointer to the geofit structure
pointer	gfit		#I pointer to the plot structure
double	xref[ARB]	#I x reference values
double	yref[ARB]	#I y reference values
double	xin[ARB]	#I x values
double	yin[ARB]	#I y values
double	wts[ARB]	#I array of weights
int	npts		#I number of points

int	i, j
pointer	sp, rxin, ryin

begin
	# If previous plot different type don't overplot.
	if (GG_PLOTTYPE(gfit) != FIT)
	    GG_OVERPLOT(gfit) = NO

	# If not overplottting start new plot.
	if (GG_OVERPLOT(gfit) == NO) {

	    # Set scale and axes.
	    call gclear (gd)
	    call smark (sp)
	    call salloc (rxin, npts, TY_REAL)
	    call salloc (ryin, npts, TY_REAL)
	    call achtdr (xin, Memr[rxin], npts)
	    call achtdr (yin, Memr[ryin], npts)
	    call gascale (gd, Memr[rxin], npts, 1)
	    call gascale (gd, Memr[ryin], npts, 2)
	    call sfree (sp)
	    call gt_swind (gd, gt)
	    call gtlabax (gd, gt)

	    # Mark the data and deleted points.
	    do i = 1, npts {
		if (wts[i] == double(0.0))
		    call gmark (gd, real(xin[i]), real(yin[i]), GM_CROSS,
		        2., 2.)
		else
		    call gmark (gd, real(xin[i]), real(yin[i]), GM_PLUS,
		        2., 2.)
	    }

	    call gflush (gd)
	}

	# Mark the rejected points.
	do i = 1, GM_NREJECT(fit) {
	    j = Memi[GM_REJ(fit)+i-1]
	    call gmark (gd, real(xin[j]), real(yin[j]), GM_CIRCLE, 2., 2.)
	}

	call gflush (gd)

	# Reset the status flags
	GG_OVERPLOT(gfit) = NO
end


# GEO_2GRAPH -- Graph the x and y fit residuals versus x or y .

procedure geo_2graphd (gd, gt, fit, gfit, x, resid, wts, npts)

pointer	gd		#I pointer to the graphics device
pointer	gt		#I pointer to the plot descriptor
pointer	fit		#I pointer to geomap structure
pointer	gfit		#I pointer to the plot structure
double	x[ARB]		#I x reference values
double	resid[ARB]	#I residual
double	wts[ARB]	#I array of weights
int	npts		#I number of points

int	i, j
pointer	sp, zero
pointer	rxin, ryin

begin
	# Allocate space.
	call smark (sp)
	call salloc (zero, npts, TY_REAL)
	call amovkr (0.0, Memr[zero], npts)

	# Calculate the residuals.
	if (GG_PLOTTYPE(gfit) == FIT)
	    GG_OVERPLOT(gfit) = NO

	if (GG_OVERPLOT(gfit) == NO) {

	    call gclear (gd)

	    # Set scale and axes.
	    call salloc (rxin, npts, TY_REAL)
	    call salloc (ryin, npts, TY_REAL)
	    call achtdr (x, Memr[rxin], npts)
	    call achtdr (resid, Memr[ryin], npts)
	    call gascale (gd, Memr[rxin], npts, 1)
	    call gascale (gd, Memr[ryin], npts, 2)
	    call gt_swind (gd, gt)
	    call gtlabax (gd, gt)

	    call gpline (gd, Memr[rxin], Memr[zero], npts)
	}

	# Graph residuals and mark deleted points.
	if (GG_OVERPLOT(gfit) == NO || GG_NEWFUNCTION(gfit) == YES) {
	    do i = 1, npts {
		if (wts[i] == double(0.0))
		    call gmark (gd, Memr[rxin+i-1], Memr[ryin+i-1],
		        GM_CROSS, 2., 2.)
		else
		    call gmark (gd, Memr[rxin+i-1], Memr[ryin+i-1],
		        GM_PLUS, 2., 2.)
	    }
	}

	# plot rejected points
	if (GM_NREJECT(fit) > 0) {
	    do i = 1, GM_NREJECT(fit) {
		j = Memi[GM_REJ(fit)+i-1]
		call gmark (gd, Memr[rxin+j-1], Memr[ryin+j-1], GM_CIRCLE,
		    2., 2.)
	    }
	}

	# Reset the status flag.
	GG_OVERPLOT(gfit) = NO

	call gflush (gd)
	call sfree (sp)
end


# GEO_CONXY -- Plot a set of default lines of xref = const and yref = const.

procedure geo_conxyd (gd, fit, sx1, sy1, sx2, sy2)

pointer	gd		#I graphics file descriptor
pointer	fit		#I fit descriptor
pointer	sx1, sy1	#I pointer to the linear x and y surface fits
pointer sx2, sy2	#I pointer to the linear x and y surface fits

int	i
pointer	sp, xtemp, ytemp, xfit1, yfit1, xfit2, yfit2
pointer	xbuf, ybuf
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

	# Calculate intervals in x and y.
	dx = (GM_XMAX(fit) - GM_XMIN(fit)) / NINTERVALS
	dy = (GM_YMAX(fit) - GM_YMIN(fit)) / (NGRAPH - 1)

	# Set up an array of y values.
	Memd[ytemp] = GM_YMIN(fit)
	do i = 2, NGRAPH
	    Memd[ytemp+i-1] = Memd[ytemp+i-2] + dy

	# Mark lines of constant x.
	xint = GM_XMIN(fit)
	for (i = 1; i <= NINTERVALS + 1; i = i + 1) {

	    # Set the x value.
	    call amovkd (xint, Memd[xtemp], NGRAPH)

	    # X fit.
	    call dgsvector (sx1, Memd[xtemp], Memd[ytemp], Memd[xfit1],
	        NGRAPH)
	    if (sx2 != NULL) {
	        call dgsvector (sx2, Memd[xtemp], Memd[ytemp], Memd[xfit2],
		    NGRAPH)
	        call aaddd (Memd[xfit1], Memd[xfit2], Memd[xfit1], NGRAPH)
	    }

	    # Y fit.
	    call dgsvector (sy1, Memd[xtemp], Memd[ytemp], Memd[yfit1],
	        NGRAPH)
	    if (sy2 != NULL) {
	        call dgsvector (sy2, Memd[xtemp], Memd[ytemp], Memd[yfit2],
		    NGRAPH)
	        call aaddd (Memd[yfit1], Memd[yfit2], Memd[yfit1], NGRAPH)
	    }

	    # Plot line of constant x.
	    call achtdr (Memd[xfit1], Memr[xbuf], NGRAPH)
	    call achtdr (Memd[yfit1], Memr[ybuf], NGRAPH)
	    call gpline (gd, Memr[xbuf], Memr[ybuf], NGRAPH)

	    # Update the x value.
	    xint = xint + dx
	}

	call gflush (gd)

	# Calculate x and y intervals.
	dx = (GM_XMAX(fit) - GM_XMIN(fit)) / (NGRAPH - 1)
	dy = (GM_YMAX(fit) - GM_YMIN(fit)) / NINTERVALS

	# Set up array of x values.
	Memd[xtemp] = GM_XMIN(fit)
	do i = 2, NGRAPH
	    Memd[xtemp+i-1] = Memd[xtemp+i-2] + dx

	# Mark lines of constant y.
	yint = GM_YMIN(fit)
	for (i = 1; i <= NINTERVALS + 1; i = i + 1) {

	    # set the y value
	    call amovkd (yint, Memd[ytemp], NGRAPH)

	    # X fit.
	    call dgsvector (sx1, Memd[xtemp], Memd[ytemp], Memd[xfit1],
	        NGRAPH)
	    if (sx2 != NULL) {
	        call dgsvector (sx2, Memd[xtemp], Memd[ytemp], Memd[xfit2],
		    NGRAPH)
	        call aaddd (Memd[xfit1], Memd[xfit2], Memd[xfit1], NGRAPH)
	    }


	    # Y fit.
	    call dgsvector (sy1, Memd[xtemp], Memd[ytemp], Memd[yfit1],
	        NGRAPH)
	    if (sy2 != NULL) {
	        call dgsvector (sy2, Memd[xtemp], Memd[ytemp], Memd[yfit2],
		    NGRAPH)
	        call aaddd (Memd[yfit1], Memd[yfit2], Memd[yfit1], NGRAPH)
	    }

	    # Plot line of constant y.
	    call achtdr (Memd[xfit1], Memr[xbuf], NGRAPH)
	    call achtdr (Memd[yfit1], Memr[ybuf], NGRAPH)
	    call gpline (gd, Memr[xbuf], Memr[ybuf], NGRAPH)

	    # Update the y value.
	    yint = yint + dy
	}

	call gflush (gd)

	call sfree (sp)
end


# GEO_LXY -- Draw a line of constant x-y.

procedure geo_lxyd (gd, fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin, npts, 
        wx, wy)

pointer	gd		#I pointer to graphics descriptor
pointer	fit		#I pointer to the fit parameters
pointer	sx1		#I pointer to the linear x fit
pointer	sy1		#I pointer to the linear y fit
pointer	sx2		#I pointer to the higher order x fit
pointer	sy2		#I pointer to the higher order y fit
double	xref[ARB]	#I x reference values
double	yref[ARB]	#I y reference values
double	xin[ARB]	#I x input values
double	yin[ARB]	#I y input values
int	npts		#I number of data points
real	wx, wy		#I x and y world coordinates

int	i, j
pointer	sp, xtemp, ytemp, xfit1, yfit1, xfit2, yfit2
pointer	xbuf, ybuf
real	x0, y0, r2, r2min
double	delta, deltax, deltay
double	dgseval()

begin
	# Transform world coordinates.
	call gctran (gd, wx, wy, wx, wy, 1, 0)
	r2min = MAX_REAL
	j = 0

	# Find the nearest data point.
	do i = 1, npts {
	    call gctran (gd, real(xin[i]), real(yin[i]), x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
	        r2min = r2
	        j = i
	    }
	}

	# Fit the line
	if (j != 0) {

	    # Allocate temporary space.
	    call smark (sp)
	    call salloc (xtemp, NGRAPH, TY_DOUBLE)
	    call salloc (ytemp, NGRAPH, TY_DOUBLE)
	    call salloc (xfit1, NGRAPH, TY_DOUBLE)
	    call salloc (yfit1, NGRAPH, TY_DOUBLE)
	    call salloc (xfit2, NGRAPH, TY_DOUBLE)
	    call salloc (yfit2, NGRAPH, TY_DOUBLE)
	    call salloc (xbuf, NGRAPH, TY_REAL)
	    call salloc (ybuf, NGRAPH, TY_REAL)

	    # Compute the deltas.
	    deltax = xin[j] - dgseval (sx1, xref[j], yref[j])
	    if (sx2 != NULL)
	        deltax = deltax - dgseval (sx2, xref[j], yref[j])
	    deltay = yin[j] - dgseval (sy1, xref[j], yref[j])
	    if (sy2 != NULL)
		deltay = deltay - dgseval (sy2, xref[j], yref[j])

	    # Set up line of constant x.
	    call amovkd (xref[j], Memd[xtemp], NGRAPH)
	    delta = (GM_YMAX(fit) - GM_YMIN(fit)) / (NGRAPH - 1)
	    Memd[ytemp] = GM_YMIN(fit)
	    do i = 2, NGRAPH
	        Memd[ytemp+i-1] = Memd[ytemp+i-2] + delta

	    # X solution.
	    call dgsvector (sx1, Memd[xtemp], Memd[ytemp], Memd[xfit1],
	        NGRAPH)
	    if (sx2 != NULL) {
	        call dgsvector (sx2, Memd[xtemp], Memd[ytemp], Memd[xfit2],
		    NGRAPH)
	        call aaddd (Memd[xfit1], Memd[xfit2], Memd[xfit1], NGRAPH)
	    }
	    call aaddkd (Memd[xfit1], deltax, Memd[xfit1], NGRAPH)

	    # Y solution.
	    call dgsvector (sy1, Memd[xtemp], Memd[ytemp], Memd[yfit1],
	        NGRAPH)
	    if (sy2 != NULL) {
	        call dgsvector (sy2, Memd[xtemp], Memd[ytemp], Memd[yfit2],
		    NGRAPH)
	        call aaddd (Memd[yfit1], Memd[yfit2], Memd[yfit1], NGRAPH)
	    }
	    call aaddkd (Memd[yfit1], deltay, Memd[yfit1], NGRAPH)

	    # Plot line of constant x.
	    call achtdr (Memd[xfit1], Memr[xbuf], NGRAPH)
	    call achtdr (Memd[yfit1], Memr[ybuf], NGRAPH)
	    call gpline (gd, Memr[xbuf], Memr[ybuf], NGRAPH)
	    call gflush (gd)

	    # Set up line of constant y.
	    call amovkd (yref[j], Memd[ytemp], NGRAPH)
	    delta = (GM_XMAX(fit) - GM_XMIN(fit)) / (NGRAPH - 1)
	    Memd[xtemp] = GM_XMIN(fit)
	    do i = 2, NGRAPH
	        Memd[xtemp+i-1] = Memd[xtemp+i-2] + delta

	    # X fit.
	    call dgsvector (sx1, Memd[xtemp], Memd[ytemp], Memd[xfit1],
	        NGRAPH)
	    if (sx2 != NULL) {
	        call dgsvector (sx2, Memd[xtemp], Memd[ytemp], Memd[xfit2],
		    NGRAPH)
	        call aaddd (Memd[xfit1], Memd[xfit2], Memd[xfit1], NGRAPH)
	    }
	    call aaddkd (Memd[xfit1], deltax, Memd[xfit1], NGRAPH)

	    # Y fit.
	    call dgsvector (sy1, Memd[xtemp], Memd[ytemp], Memd[yfit1],
	        NGRAPH)
	    if (sy2 != NULL) {
	        call dgsvector (sy2, Memd[xtemp], Memd[ytemp], Memd[yfit2],
		    NGRAPH)
	        call aaddd (Memd[yfit1], Memd[yfit2], Memd[yfit1], NGRAPH)
	    }
	    call aaddkd (Memd[yfit1], deltay, Memd[yfit1], NGRAPH)

	    # Plot line of constant y.
	    call achtdr (Memd[xfit1], Memr[xbuf], NGRAPH)
	    call achtdr (Memd[yfit1], Memr[ybuf], NGRAPH)
	    call gpline (gd, Memr[xbuf], Memr[ybuf], NGRAPH)
	    call gflush (gd)

	    # Free space.
	    call sfree (sp)
	}
end


# GEO_GCOEFF -- Print the coefficents of the linear portion of the
# fit, xshift, yshift, 

procedure geo_gcoeffd (sx, sy, xshift, yshift, a, b, c, d)

pointer	sx		#I pointer to the x surface fit
pointer	sy		#I pointer to the y surface fit
double	xshift		#O output x shift
double	yshift		#O output y shift
double	a		#O output x coefficient of x fit
double	b		#O output y coefficient of x fit
double	c		#O output x coefficient of y fit
double	d		#O output y coefficient of y fit

int	nxxcoeff, nxycoeff, nyxcoeff, nyycoeff
pointer	sp, xcoeff, ycoeff
double	xxrange, xyrange, xxmaxmin, xymaxmin
double	yxrange, yyrange, yxmaxmin, yymaxmin

int	dgsgeti()
double	dgsgetd()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (xcoeff, dgsgeti (sx, GSNCOEFF), TY_DOUBLE)
	call salloc (ycoeff, dgsgeti (sy, GSNCOEFF), TY_DOUBLE)

	# Get coefficients and numbers of coefficients.
	call dgscoeff (sx, Memd[xcoeff], nxxcoeff)
	call dgscoeff (sy, Memd[ycoeff], nyycoeff)
	nxxcoeff = dgsgeti (sx, GSNXCOEFF)
	nxycoeff = dgsgeti (sx, GSNYCOEFF)
	nyxcoeff = dgsgeti (sy, GSNXCOEFF)
	nyycoeff = dgsgeti (sy, GSNYCOEFF)

	# Get the data range.
	if (dgsgeti (sx, GSTYPE) != GS_POLYNOMIAL) {
	    xxrange = (dgsgetd (sx, GSXMAX) - dgsgetd (sx, GSXMIN)) / 2.0d0
	    xxmaxmin = - (dgsgetd (sx, GSXMAX) + dgsgetd (sx, GSXMIN)) / 2.0d0
	    xyrange = (dgsgetd (sx, GSYMAX) - dgsgetd (sx, GSYMIN)) / 2.0d0
	    xymaxmin = - (dgsgetd (sx, GSYMAX) + dgsgetd (sx, GSYMIN)) / 2.0d0
	} else {
	    xxrange = double(1.0)
	    xxmaxmin = double(0.0)
	    xyrange = double(1.0)
	    xymaxmin = double(0.0)
	}

	if (dgsgeti (sy, GSTYPE) != GS_POLYNOMIAL) {
	    yxrange = (dgsgetd (sy, GSXMAX) - dgsgetd (sy, GSXMIN)) / 2.0d0
	    yxmaxmin = - (dgsgetd (sy, GSXMAX) + dgsgetd (sy, GSXMIN)) / 2.0d0
	    yyrange = (dgsgetd (sy, GSYMAX) - dgsgetd (sy, GSYMIN)) / 2.0d0
	    yymaxmin = - (dgsgetd (sy, GSYMAX) + dgsgetd (sy, GSYMIN)) / 2.0d0
	} else {
	    yxrange = double(1.0)
	    yxmaxmin = double(0.0)
	    yyrange = double(1.0)
	    yymaxmin = double(0.0)
	}

	# Get the shifts.
	xshift = Memd[xcoeff] + Memd[xcoeff+1] * xxmaxmin / xxrange +
	    Memd[xcoeff+2] * xymaxmin / xyrange
	yshift = Memd[ycoeff] + Memd[ycoeff+1] * yxmaxmin / yxrange +
	    Memd[ycoeff+2] * yymaxmin / yyrange

	# Get the rotation and scaling parameters and correct for normalization.
	if (nxxcoeff > 1)
	    a = Memd[xcoeff+1] / xxrange
	else
	    a = double(0.0)
	if (nxycoeff > 1)
	    b = Memd[xcoeff+nxxcoeff] / xyrange
	else
	    b = double(0.0)
	if (nyxcoeff > 1)
	    c = Memd[ycoeff+1] / yxrange
	else
	    c = double(0.0)
	if (nyycoeff > 1)
	    d = Memd[ycoeff+nyxcoeff] / yyrange
	else
	    d = double(0.0)

	call sfree (sp)
end


