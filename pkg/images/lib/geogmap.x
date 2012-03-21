# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <math.h>
include <math/gsurfit.h>
include	<gset.h>
include "geomap.h"
include "geogmap.h"

define	GHELPFILE 	"images$lib/geomap.key"
define	CHELPFILE 	"images$lib/coomap.key"



# GEO_MGFIT -- Fit the surface using interactive graphics.

procedure geo_mgfitr (gd, fit, sx1, sy1, sx2, sy2, xref, yref, xin,
        yin, wts, npts, xerrmsg, yerrmsg, maxch)

pointer	gd		#I graphics file descriptor
pointer	fit		#I pointer to the fit structure
pointer	sx1		#I pointer to the linear x surface fit
pointer	sy1		#I pointer to the linear y surface fit
pointer	sx2		#I pointer to higher order x surface fit
pointer	sy2		#I pointer to higher order y surface fit
real	xref[npts]	#I the x reference coordinates
real	yref[npts]	#I the y reference coordinates
real	xin[npts]	#I input x coordinates
real	yin[npts]	#I input y coordinates
real	wts[npts]	#I array of weights
int	npts		#I number of data points
char	xerrmsg[ARB]	#O the output x fit error message 
char	yerrmsg[ARB]	#O the output x fit error message 
int	maxch		#I the size of the error messages

char	errstr[SZ_LINE]
int	newgraph, delete, wcs, key, errcode
pointer	sp, w, gfit, xresid, yresid, cmd
pointer	gt1, gt2, gt3, gt4, gt5
real	wx, wy
real	xshift, yshift, xscale, yscale, thetax, thetay

int	clgcur(), errget()
pointer	gt_init()

errchk	geo_fxyr(), geo_mrejectr(), geo_fthetar()
errchk	geo_fmagnifyr(), geo_flinearr()

begin
	# Initialize gfit structure and working space.
	call smark (sp)
	call salloc (gfit, LEN_GEOGRAPH, TY_STRUCT)
	call salloc (xresid, npts, TY_REAL)
	call salloc (yresid, npts, TY_REAL)
	call salloc (w, npts, TY_REAL)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Do initial fit.
	iferr {
	    switch (GM_FIT(fit)) {
	    case GM_ROTATE:
	        call geo_fthetar (fit, sx1, sy1, xref, yref, xin, yin, wts,
	            Memr[xresid], Memr[yresid], npts, xerrmsg, maxch,
		    yerrmsg, maxch)
		    sx2 = NULL
		    sy2 = NULL
	    case GM_RSCALE:
	        call geo_fmagnifyr (fit, sx1, sy1, xref, yref, xin, yin, wts,
	            Memr[xresid], Memr[yresid], npts, xerrmsg, maxch,
		    yerrmsg, maxch)
		    sx2 = NULL
		    sy2 = NULL
	    case GM_RXYSCALE:
	        call geo_flinearr (fit, sx1, sy1, xref, yref, xin, yin, wts,
	            Memr[xresid], Memr[yresid], npts, xerrmsg, maxch,
		    yerrmsg, maxch)
		    sx2 = NULL
		    sy2 = NULL
	    default:
	        call geo_fxyr (fit, sx1, sx2, xref, yref, xin, wts,
		    Memr[xresid], npts, YES, xerrmsg, maxch)
	        call geo_fxyr (fit, sy1, sy2, xref, yref, yin, wts,
		    Memr[yresid], npts, NO, yerrmsg, maxch)
	    }
	    if (GM_MAXITER(fit) <= 0 || IS_INDEFD(GM_REJECT(fit)))
	        GM_NREJECT(fit) = 0
	    else
	        call geo_mrejectr (fit, sx1, sy1, sx2, sy2, xref, yref, xin,
		    yin, wts, Memr[xresid], Memr[yresid], npts, xerrmsg,
		    maxch, yerrmsg, maxch)
	} then {
	    call sfree (sp)
	    if (GM_PROJECTION(fit) == GM_NONE)
	        call error (2, "Too few points for X and Y fits.")
	    else
	        call error (2, "Too few points for XI and ETA fits.")
	}

	GG_NEWFUNCTION(gfit) = NO
	GG_FITERROR(gfit) = NO
	errcode = OK

	# Set up plotting defaults.
	GG_PLOTTYPE(gfit) = FIT
	GG_OVERPLOT(gfit) = NO
	GG_CONSTXY(gfit) = YES
	newgraph = NO

	# Allocate graphics tools.
	gt1 = gt_init ()
	gt2 = gt_init ()
	gt3 = gt_init ()
	gt4 = gt_init ()
	gt5 = gt_init ()

	# Set the plot title and x and y axis labels.
	call geo_gtset (FIT, gt1, fit)
	call geo_gtset (XXRESID, gt2, fit)
	call geo_gtset (XYRESID, gt3, fit)
	call geo_gtset (YXRESID, gt4, fit)
	call geo_gtset (YYRESID, gt5, fit)

	# Make the first plot.
	call gclear (gd)
	call geo_label (FIT, gt1, fit)
	call geo_1graphr (gd, gt1, fit, gfit, xref, yref, xin, yin, wts,
	    npts)
	if (GG_CONSTXY(gfit) == YES)
	    call geo_conxyr (gd, fit, sx1, sy1, sx2, sy2)
	call printf ("%s  %s\n")
	    call pargstr (xerrmsg)
	    call pargstr (yerrmsg)

	# Read the cursor commands.
	call amovr (wts, Memr[w], npts)
	while (clgcur ("cursor", wx, wy, wcs, key, Memc[cmd], SZ_LINE) != EOF) {

	    switch (key) {

	    case 'q':
		call amovr (Memr[w], wts, npts)
		break

	    case '?':
		if (GM_PROJECTION(fit) == GM_NONE)
		    call gpagefile (gd, GHELPFILE, "")
		else
		    call gpagefile (gd, CHELPFILE, "")

	    case ':':
		call geo_colon (gd, fit, gfit, Memc[cmd], newgraph)
		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call gt_colon (Memc[cmd], gd, gt1, newgraph)
		case XXRESID:
		    call gt_colon (Memc[cmd], gd, gt2, newgraph)
		case XYRESID:
		    call gt_colon (Memc[cmd], gd, gt3, newgraph)
		case YXRESID:
		    call gt_colon (Memc[cmd], gd, gt4, newgraph)
		case YYRESID:
		    call gt_colon (Memc[cmd], gd, gt5, newgraph)
		}

	    case 'l':
		if (GG_FITERROR(gfit) == NO) {
		    call geo_lcoeffr (sx1, sy1, xshift, yshift, xscale, yscale,
		        thetax, thetay)
		    call printf ("xshift: %.2f yshift: %.2f ")
		        call pargr (xshift)
		        call pargr (yshift)
		    call printf ("xmag: %0.3g ymag: %0.3g ")
		        call pargr (xscale)
		        call pargr (yscale)
		    call printf ("xrot: %.2f yrot: %.2f\n")
		        call pargr (thetax)
		        call pargr (thetay)
		}

	    case 't':
		if (GG_FITERROR(gfit) == NO && GG_PLOTTYPE(gfit) == FIT)
		    call geo_lxyr (gd, fit, sx1, sy1, sx2, sy2, xref, yref,
		        xin, yin, npts, wx, wy)

	    case 'c':
		if (GG_CONSTXY(gfit) == YES)
		    GG_CONSTXY(gfit) = NO
		else if (GG_CONSTXY(gfit) == NO)
		    GG_CONSTXY(gfit) = YES

	    case 'd', 'u':
		if (key == 'd')
		    delete = YES
		else
		    delete = NO

		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call geo_1deleter (gd, xin, yin, Memr[w], wts, npts, wx,
		        wy, delete)
		case XXRESID:
		    call geo_2deleter (gd, xref, Memr[xresid], Memr[w], wts,
		        npts, wx, wy, delete)
		case XYRESID:
		    call geo_2deleter (gd, yref, Memr[xresid], Memr[w], wts,
		        npts, wx, wy, delete)
		case YXRESID:
		    call geo_2deleter (gd, xref, Memr[yresid], Memr[w], wts,
		        npts, wx, wy, delete)
		case YYRESID:
		    call geo_2deleter (gd, yref, Memr[yresid], Memr[w], wts,
		        npts, wx, wy, delete)
		}

		GG_NEWFUNCTION(gfit) = YES

	    case 'g':
		if (GG_PLOTTYPE(gfit) != FIT)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = FIT

	    case 'x':
		if (GG_PLOTTYPE(gfit) != XXRESID)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = XXRESID

	    case 'r':
		if (GG_PLOTTYPE(gfit) != XYRESID)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = XYRESID

	    case 'y':
		if (GG_PLOTTYPE(gfit) != YXRESID)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = YXRESID

	    case 's':
		if (GG_PLOTTYPE(gfit) != YYRESID)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = YYRESID

	    case 'f':
		# do fit
		if (GG_NEWFUNCTION(gfit) == YES) {
		    iferr {
			switch (GM_FIT(fit)) {
			case GM_ROTATE:
		            call geo_fthetar (fit, sx1, sy1, xref, yref, xin,
			        yin, Memr[w], Memr[xresid], Memr[yresid],
				npts, xerrmsg, maxch, yerrmsg, maxch) 
				sx2 = NULL
				sy2 = NULL
			case GM_RSCALE:
		            call geo_fmagnifyr (fit, sx1, sy1, xref, yref, xin,
			        yin, Memr[w], Memr[xresid], Memr[yresid],
				npts, xerrmsg, maxch, yerrmsg, maxch) 
				sx2 = NULL
				sy2 = NULL
			case GM_RXYSCALE:
		            call geo_flinearr (fit, sx1, sy1, xref, yref, xin,
			        yin, Memr[w], Memr[xresid], Memr[yresid],
				npts, xerrmsg, maxch, yerrmsg, maxch) 
				sx2 = NULL
				sy2 = NULL
			default:
		            call geo_fxyr (fit, sx1, sx2, xref, yref, xin,
			        Memr[w], Memr[xresid], npts, YES,
				xerrmsg, maxch) 
		            call geo_fxyr (fit, sy1, sy2, xref, yref, yin,
			        Memr[w], Memr[yresid], npts, NO,
				yerrmsg, maxch) 
			}
		        if (GM_MAXITER(fit) <= 0 || IS_INDEFD(GM_REJECT(fit)))
			    GM_NREJECT(fit) = 0
		        else
			    call geo_mrejectr (fit, sx1, sy1, sx2, sy2, xref,
			        yref, xin, yin, Memr[w], Memr[xresid],
				Memr[yresid], npts, xerrmsg, maxch,
				yerrmsg, maxch)
			GG_NEWFUNCTION(gfit) = NO
			GG_FITERROR(gfit) = NO
			errcode = OK
		    } then {
	    		errcode = errget (errstr, SZ_LINE)
	    		call printf ("%s\n")
			    call pargstr (errstr)
			GG_FITERROR(gfit) = YES
		    }
		}

		# plot new graph
		if (GG_FITERROR(gfit) == YES)
		    newgraph = NO
		else
		    newgraph = YES

	    case 'o':
		GG_OVERPLOT(gfit) = YES

	    default:
		call printf ("\07")

	    }

	    if (newgraph == YES) {
		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call geo_label (FIT, gt1, fit)
	    	    call geo_1graphr (gd, gt1, fit, gfit, xref, yref, xin, yin,
		        Memr[w], npts)
		    if (GG_CONSTXY(gfit) == YES)
			call geo_conxyr (gd, fit, sx1, sy1, sx2, sy2)
		case XXRESID:
		    call geo_label (XXRESID, gt2, fit)
	    	    call geo_2graphr (gd, gt2, fit, gfit, xref, Memr[xresid],
		        Memr[w], npts)
		case XYRESID:
		    call geo_label (XYRESID, gt3, fit)
	    	    call geo_2graphr (gd, gt3, fit, gfit, yref, Memr[xresid],
		         Memr[w], npts)
		case YXRESID:
		    call geo_label (YXRESID, gt4, fit)
	    	    call geo_2graphr (gd, gt4, fit, gfit, xref, Memr[yresid],
		         Memr[w], npts)
		case YYRESID:
		    call geo_label (YYRESID, gt5, fit)
	    	    call geo_2graphr (gd, gt5, fit, gfit, yref, Memr[yresid],
		         Memr[w], npts)
		}
	        call printf ("%s  %s\n")
	    	    call pargstr (xerrmsg)
	    	    call pargstr (yerrmsg)
		newgraph = NO
	    }
	}

	# Free space.
	call gt_free (gt1)
	call gt_free (gt2)
	call gt_free (gt3)
	call gt_free (gt4)
	call gt_free (gt5)
	call sfree (sp)

	# Call an error if appropriate.
	if (errcode > 0)
	    call error (2, errstr)
end

# GEO_LCOEFF -- Print the coefficents of the linear portion of the
# fit, xshift, yshift, xexpansion, yexpansion, x and y rotations.

procedure geo_lcoeffr (sx, sy, xshift, yshift, xscale, yscale, xrot, yrot)

pointer sx              #I pointer to the x surface fit
pointer sy              #I pointer to the y surface fit
real   xshift          #O output x shift
real   yshift          #O output y shift
real   xscale          #O output x scale
real   yscale          #O output y scale
real   xrot            #O rotation of point on x axis
real   yrot            #O rotation of point on y axis

int     nxxcoeff, nxycoeff, nyxcoeff, nyycoeff
pointer sp, xcoeff, ycoeff
real   xxrange, xyrange, xxmaxmin, xymaxmin
real   yxrange, yyrange, yxmaxmin, yymaxmin
real   a, b, c, d

bool    fp_equalr()
int     gsgeti()
real    gsgetr()

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

        # Get the magnification factors.
        xscale = sqrt (a * a + c * c)
        yscale = sqrt (b * b + d * d)

        # Get the x and y axes rotation factors.
        if (fp_equalr (a, real(0.0)) && fp_equalr (c, real(0.0)))
            xrot = real(0.0)
        else
            xrot = RADTODEG (atan2 (-c, a))
        if (xrot < real(0.0))
            xrot = xrot + real(360.0)

        if (fp_equalr (b, real(0.0)) && fp_equalr (d, real(0.0)))
            yrot = real(0.0)
        else
            yrot = RADTODEG (atan2 (b, d))
        if (yrot < real(0.0))
            yrot = yrot + real(360.0)

        call sfree (sp)
end



# GEO_MGFIT -- Fit the surface using interactive graphics.

procedure geo_mgfitd (gd, fit, sx1, sy1, sx2, sy2, xref, yref, xin,
        yin, wts, npts, xerrmsg, yerrmsg, maxch)

pointer	gd		#I graphics file descriptor
pointer	fit		#I pointer to the fit structure
pointer	sx1		#I pointer to the linear x surface fit
pointer	sy1		#I pointer to the linear y surface fit
pointer	sx2		#I pointer to higher order x surface fit
pointer	sy2		#I pointer to higher order y surface fit
double	xref[npts]	#I the x reference coordinates
double	yref[npts]	#I the y reference coordinates
double	xin[npts]	#I input x coordinates
double	yin[npts]	#I input y coordinates
double	wts[npts]	#I array of weights
int	npts		#I number of data points
char	xerrmsg[ARB]	#O the output x fit error message 
char	yerrmsg[ARB]	#O the output x fit error message 
int	maxch		#I the size of the error messages

char	errstr[SZ_LINE]
int	newgraph, delete, wcs, key, errcode
pointer	sp, w, gfit, xresid, yresid, cmd
pointer	gt1, gt2, gt3, gt4, gt5
real	wx, wy
double	xshift, yshift, xscale, yscale, thetax, thetay

int	clgcur(), errget()
pointer	gt_init()

errchk	geo_fxyd(), geo_mrejectd(), geo_fthetad()
errchk	geo_fmagnifyd(), geo_flineard()

begin
	# Initialize gfit structure and working space.
	call smark (sp)
	call salloc (gfit, LEN_GEOGRAPH, TY_STRUCT)
	call salloc (xresid, npts, TY_DOUBLE)
	call salloc (yresid, npts, TY_DOUBLE)
	call salloc (w, npts, TY_DOUBLE)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Do initial fit.
	iferr {
	    switch (GM_FIT(fit)) {
	    case GM_ROTATE:
	        call geo_fthetad (fit, sx1, sy1, xref, yref, xin, yin, wts,
	            Memd[xresid], Memd[yresid], npts, xerrmsg, maxch,
		    yerrmsg, maxch)
		    sx2 = NULL
		    sy2 = NULL
	    case GM_RSCALE:
	        call geo_fmagnifyd (fit, sx1, sy1, xref, yref, xin, yin, wts,
	            Memd[xresid], Memd[yresid], npts, xerrmsg, maxch,
		    yerrmsg, maxch)
		    sx2 = NULL
		    sy2 = NULL
	    case GM_RXYSCALE:
	        call geo_flineard (fit, sx1, sy1, xref, yref, xin, yin, wts,
	            Memd[xresid], Memd[yresid], npts, xerrmsg, maxch,
		    yerrmsg, maxch)
		    sx2 = NULL
		    sy2 = NULL
	    default:
	        call geo_fxyd (fit, sx1, sx2, xref, yref, xin, wts,
		    Memd[xresid], npts, YES, xerrmsg, maxch)
	        call geo_fxyd (fit, sy1, sy2, xref, yref, yin, wts,
		    Memd[yresid], npts, NO, yerrmsg, maxch)
	    }
	    if (GM_MAXITER(fit) <= 0 || IS_INDEFD(GM_REJECT(fit)))
	        GM_NREJECT(fit) = 0
	    else
	        call geo_mrejectd (fit, sx1, sy1, sx2, sy2, xref, yref, xin,
		    yin, wts, Memd[xresid], Memd[yresid], npts, xerrmsg,
		    maxch, yerrmsg, maxch)
	} then {
	    call sfree (sp)
	    if (GM_PROJECTION(fit) == GM_NONE)
	        call error (2, "Too few points for X and Y fits.")
	    else
	        call error (2, "Too few points for XI and ETA fits.")
	}

	GG_NEWFUNCTION(gfit) = NO
	GG_FITERROR(gfit) = NO
	errcode = OK

	# Set up plotting defaults.
	GG_PLOTTYPE(gfit) = FIT
	GG_OVERPLOT(gfit) = NO
	GG_CONSTXY(gfit) = YES
	newgraph = NO

	# Allocate graphics tools.
	gt1 = gt_init ()
	gt2 = gt_init ()
	gt3 = gt_init ()
	gt4 = gt_init ()
	gt5 = gt_init ()

	# Set the plot title and x and y axis labels.
	call geo_gtset (FIT, gt1, fit)
	call geo_gtset (XXRESID, gt2, fit)
	call geo_gtset (XYRESID, gt3, fit)
	call geo_gtset (YXRESID, gt4, fit)
	call geo_gtset (YYRESID, gt5, fit)

	# Make the first plot.
	call gclear (gd)
	call geo_label (FIT, gt1, fit)
	call geo_1graphd (gd, gt1, fit, gfit, xref, yref, xin, yin, wts,
	    npts)
	if (GG_CONSTXY(gfit) == YES)
	    call geo_conxyd (gd, fit, sx1, sy1, sx2, sy2)
	call printf ("%s  %s\n")
	    call pargstr (xerrmsg)
	    call pargstr (yerrmsg)

	# Read the cursor commands.
	call amovd (wts, Memd[w], npts)
	while (clgcur ("cursor", wx, wy, wcs, key, Memc[cmd], SZ_LINE) != EOF) {

	    switch (key) {

	    case 'q':
		call amovd (Memd[w], wts, npts)
		break

	    case '?':
		if (GM_PROJECTION(fit) == GM_NONE)
		    call gpagefile (gd, GHELPFILE, "")
		else
		    call gpagefile (gd, CHELPFILE, "")

	    case ':':
		call geo_colon (gd, fit, gfit, Memc[cmd], newgraph)
		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call gt_colon (Memc[cmd], gd, gt1, newgraph)
		case XXRESID:
		    call gt_colon (Memc[cmd], gd, gt2, newgraph)
		case XYRESID:
		    call gt_colon (Memc[cmd], gd, gt3, newgraph)
		case YXRESID:
		    call gt_colon (Memc[cmd], gd, gt4, newgraph)
		case YYRESID:
		    call gt_colon (Memc[cmd], gd, gt5, newgraph)
		}

	    case 'l':
		if (GG_FITERROR(gfit) == NO) {
		    call geo_lcoeffd (sx1, sy1, xshift, yshift, xscale, yscale,
		        thetax, thetay)
		    call printf ("xshift: %.2f yshift: %.2f ")
		        call pargd (xshift)
		        call pargd (yshift)
		    call printf ("xmag: %0.3g ymag: %0.3g ")
		        call pargd (xscale)
		        call pargd (yscale)
		    call printf ("xrot: %.2f yrot: %.2f\n")
		        call pargd (thetax)
		        call pargd (thetay)
		}

	    case 't':
		if (GG_FITERROR(gfit) == NO && GG_PLOTTYPE(gfit) == FIT)
		    call geo_lxyd (gd, fit, sx1, sy1, sx2, sy2, xref, yref,
		        xin, yin, npts, wx, wy)

	    case 'c':
		if (GG_CONSTXY(gfit) == YES)
		    GG_CONSTXY(gfit) = NO
		else if (GG_CONSTXY(gfit) == NO)
		    GG_CONSTXY(gfit) = YES

	    case 'd', 'u':
		if (key == 'd')
		    delete = YES
		else
		    delete = NO

		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call geo_1deleted (gd, xin, yin, Memd[w], wts, npts, wx,
		        wy, delete)
		case XXRESID:
		    call geo_2deleted (gd, xref, Memd[xresid], Memd[w], wts,
		        npts, wx, wy, delete)
		case XYRESID:
		    call geo_2deleted (gd, yref, Memd[xresid], Memd[w], wts,
		        npts, wx, wy, delete)
		case YXRESID:
		    call geo_2deleted (gd, xref, Memd[yresid], Memd[w], wts,
		        npts, wx, wy, delete)
		case YYRESID:
		    call geo_2deleted (gd, yref, Memd[yresid], Memd[w], wts,
		        npts, wx, wy, delete)
		}

		GG_NEWFUNCTION(gfit) = YES

	    case 'g':
		if (GG_PLOTTYPE(gfit) != FIT)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = FIT

	    case 'x':
		if (GG_PLOTTYPE(gfit) != XXRESID)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = XXRESID

	    case 'r':
		if (GG_PLOTTYPE(gfit) != XYRESID)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = XYRESID

	    case 'y':
		if (GG_PLOTTYPE(gfit) != YXRESID)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = YXRESID

	    case 's':
		if (GG_PLOTTYPE(gfit) != YYRESID)
		    newgraph = YES
		GG_PLOTTYPE(gfit) = YYRESID

	    case 'f':
		# do fit
		if (GG_NEWFUNCTION(gfit) == YES) {
		    iferr {
			switch (GM_FIT(fit)) {
			case GM_ROTATE:
		            call geo_fthetad (fit, sx1, sy1, xref, yref, xin,
			        yin, Memd[w], Memd[xresid], Memd[yresid],
				npts, xerrmsg, maxch, yerrmsg, maxch) 
				sx2 = NULL
				sy2 = NULL
			case GM_RSCALE:
		            call geo_fmagnifyd (fit, sx1, sy1, xref, yref, xin,
			        yin, Memd[w], Memd[xresid], Memd[yresid],
				npts, xerrmsg, maxch, yerrmsg, maxch) 
				sx2 = NULL
				sy2 = NULL
			case GM_RXYSCALE:
		            call geo_flineard (fit, sx1, sy1, xref, yref, xin,
			        yin, Memd[w], Memd[xresid], Memd[yresid],
				npts, xerrmsg, maxch, yerrmsg, maxch) 
				sx2 = NULL
				sy2 = NULL
			default:
		            call geo_fxyd (fit, sx1, sx2, xref, yref, xin,
			        Memd[w], Memd[xresid], npts, YES,
				xerrmsg, maxch) 
		            call geo_fxyd (fit, sy1, sy2, xref, yref, yin,
			        Memd[w], Memd[yresid], npts, NO,
				yerrmsg, maxch) 
			}
		        if (GM_MAXITER(fit) <= 0 || IS_INDEFD(GM_REJECT(fit)))
			    GM_NREJECT(fit) = 0
		        else
			    call geo_mrejectd (fit, sx1, sy1, sx2, sy2, xref,
			        yref, xin, yin, Memd[w], Memd[xresid],
				Memd[yresid], npts, xerrmsg, maxch,
				yerrmsg, maxch)
			GG_NEWFUNCTION(gfit) = NO
			GG_FITERROR(gfit) = NO
			errcode = OK
		    } then {
	    		errcode = errget (errstr, SZ_LINE)
	    		call printf ("%s\n")
			    call pargstr (errstr)
			GG_FITERROR(gfit) = YES
		    }
		}

		# plot new graph
		if (GG_FITERROR(gfit) == YES)
		    newgraph = NO
		else
		    newgraph = YES

	    case 'o':
		GG_OVERPLOT(gfit) = YES

	    default:
		call printf ("\07")

	    }

	    if (newgraph == YES) {
		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call geo_label (FIT, gt1, fit)
	    	    call geo_1graphd (gd, gt1, fit, gfit, xref, yref, xin, yin,
		        Memd[w], npts)
		    if (GG_CONSTXY(gfit) == YES)
			call geo_conxyd (gd, fit, sx1, sy1, sx2, sy2)
		case XXRESID:
		    call geo_label (XXRESID, gt2, fit)
	    	    call geo_2graphd (gd, gt2, fit, gfit, xref, Memd[xresid],
		        Memd[w], npts)
		case XYRESID:
		    call geo_label (XYRESID, gt3, fit)
	    	    call geo_2graphd (gd, gt3, fit, gfit, yref, Memd[xresid],
		         Memd[w], npts)
		case YXRESID:
		    call geo_label (YXRESID, gt4, fit)
	    	    call geo_2graphd (gd, gt4, fit, gfit, xref, Memd[yresid],
		         Memd[w], npts)
		case YYRESID:
		    call geo_label (YYRESID, gt5, fit)
	    	    call geo_2graphd (gd, gt5, fit, gfit, yref, Memd[yresid],
		         Memd[w], npts)
		}
	        call printf ("%s  %s\n")
	    	    call pargstr (xerrmsg)
	    	    call pargstr (yerrmsg)
		newgraph = NO
	    }
	}

	# Free space.
	call gt_free (gt1)
	call gt_free (gt2)
	call gt_free (gt3)
	call gt_free (gt4)
	call gt_free (gt5)
	call sfree (sp)

	# Call an error if appropriate.
	if (errcode > 0)
	    call error (2, errstr)
end

# GEO_LCOEFF -- Print the coefficents of the linear portion of the
# fit, xshift, yshift, xexpansion, yexpansion, x and y rotations.

procedure geo_lcoeffd (sx, sy, xshift, yshift, xscale, yscale, xrot, yrot)

pointer sx              #I pointer to the x surface fit
pointer sy              #I pointer to the y surface fit
double   xshift          #O output x shift
double   yshift          #O output y shift
double   xscale          #O output x scale
double   yscale          #O output y scale
double   xrot            #O rotation of point on x axis
double   yrot            #O rotation of point on y axis

int     nxxcoeff, nxycoeff, nyxcoeff, nyycoeff
pointer sp, xcoeff, ycoeff
double   xxrange, xyrange, xxmaxmin, xymaxmin
double   yxrange, yyrange, yxmaxmin, yymaxmin
double   a, b, c, d

bool    fp_equald()
int     dgsgeti()
double  dgsgetd()

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

        # Get the magnification factors.
        xscale = sqrt (a * a + c * c)
        yscale = sqrt (b * b + d * d)

        # Get the x and y axes rotation factors.
        if (fp_equald (a, double(0.0)) && fp_equald (c, double(0.0)))
            xrot = double(0.0)
        else
            xrot = RADTODEG (atan2 (-c, a))
        if (xrot < double(0.0))
            xrot = xrot + double(360.0)

        if (fp_equald (b, double(0.0)) && fp_equald (d, double(0.0)))
            yrot = double(0.0)
        else
            yrot = RADTODEG (atan2 (b, d))
        if (yrot < double(0.0))
            yrot = yrot + double(360.0)

        call sfree (sp)
end


