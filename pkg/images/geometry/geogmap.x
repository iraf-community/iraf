# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include "geomap.h"
include "geogmap.h"

define	HELPFILE 	"lib$scr/geomap.key"

# GEOMGFIT -- Fit the surface using interactive graphics

procedure geomgfit (gd, fit, sx1, sy1, sx2, sy2, xref, yref, xin,
    yin, wts, npts)

pointer	gd		# graphics file descriptor
pointer	fit		# pointer to the fit structure
pointer	sx1		# pointer to the linear x surface fit
pointer	sy1		# pointer to the linear y surface fit
pointer	sx2		# pointer to higher order x surface fit
pointer	sy2		# pointer to higher order y surface fit
real	xref[npts]	# the x reference coordinates
real	yref[npts]	# the y reference coordinates
real	xin[npts]	# input x coordinates
real	yin[npts]	# input y coordinates
real	wts[npts]	# array of weights
int	npts		# number of data points

char	errstr[SZ_LINE]
int	newgraph, delete, wcs, key, errcode
pointer	sp, w, gfit, xresid, yresid, cmd, xerrmsg, yerrmsg
pointer	gt1, gt2, gt3, gt4, gt5
real	wx, wy, xshift, yshift, xscale, yscale, thetax, thetay

int	clgcur(), errget()
pointer	gt_init()
errchk	smark(), salloc(), geomfit(), geomreject()

begin
	# initialize gfit structure and working space
	call smark (sp)
	call salloc (gfit, LEN_GEOGRAPH, TY_STRUCT)
	call salloc (xresid, npts, TY_REAL)
	call salloc (yresid, npts, TY_REAL)
	call salloc (w, npts, TY_REAL)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (xerrmsg, SZ_LINE, TY_CHAR)
	call salloc (yerrmsg, SZ_LINE, TY_CHAR)

	# do first fit
	call geomfit (fit, sx1, sx2, xref, yref, xin, wts, Memr[xresid],
	    npts, YES, Memc[xerrmsg], SZ_LINE)
	call geomfit (fit, sy1, sy2, xref, yref, yin, wts, Memr[yresid],
	    npts, NO, Memc[yerrmsg], SZ_LINE)
	if (GM_REJECT(fit) > 0.)
	    call geomreject (fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin,
		wts, Memr[xresid], Memr[yresid], npts, Memc[xerrmsg],
		SZ_LINE, Memc[yerrmsg], SZ_LINE)
	else
	    GM_NREJECT(fit) = 0
	GG_NEWFUNCTION(gfit) = NO
	GG_FITERROR(gfit) = NO
	errcode = OK

	# set up plotting defaults
	GG_PLOTTYPE(gfit) = FIT
	GG_OVERPLOT(gfit) = NO
	GG_CONSTXY(gfit) = YES
	newgraph = NO

	# allocate graphics tools
	gt1 = gt_init ()
	gt2 = gt_init ()
	gt3 = gt_init ()
	gt4 = gt_init ()
	gt5 = gt_init ()

	# set the plot title and x and y axis labels
	call geogtset (FIT, gt1, fit)
	call geogtset (XXRESID, gt2, fit)
	call geogtset (XYRESID, gt3, fit)
	call geogtset (YXRESID, gt4, fit)
	call geogtset (YYRESID, gt5, fit)

	# make the first plot
	call gclear (gd)
	call geolabel (FIT, gt1, fit)
	call geograph1 (gd, gt1, fit, gfit, xref, yref, xin, yin, wts,
	    npts)
	if (GG_CONSTXY(gfit) == YES)
	    call geoconxy (gd, fit, sx1, sy1, sx2, sy2)
	call printf ("%s  %s\n")
	    call pargstr (Memc[xerrmsg])
	    call pargstr (Memc[yerrmsg])

	# read the cursor commands
	call amovr (wts, Memr[w], npts)
	while (clgcur ("cursor", wx, wy, wcs, key, Memc[cmd], SZ_LINE) != EOF) {

	    switch (key) {

	    case 'q':
		break

	    case '?':
		call gpagefile (gd, HELPFILE, "")

	    case ':':
		call geocolon (gd, fit, gfit, Memc[cmd], newgraph)
		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call gt_colon (Memc[cmd], gt1, newgraph)
		case XXRESID:
		    call gt_colon (Memc[cmd], gt2, newgraph)
		case XYRESID:
		    call gt_colon (Memc[cmd], gt3, newgraph)
		case YXRESID:
		    call gt_colon (Memc[cmd], gt4, newgraph)
		case YYRESID:
		    call gt_colon (Memc[cmd], gt5, newgraph)
		}

	    case 'l':
		if (GG_FITERROR(gfit) == NO) {
		    call lincoeff (fit, sx1, sy1, xshift, yshift, xscale,
		        yscale, thetax, thetay)
		    call printf ("xshift: %.2f yshift: %.2f ")
		        call pargr (xshift)
		        call pargr (yshift)
		    call printf ("xmag: %.2f ymag: %.2f ")
		        call pargr (xscale)
		        call pargr (yscale)
		    call printf ("xrot: %.2f yrot: %.2f\n")
		        call pargr (thetax)
		        call pargr (thetay)
		}

	    case 't':
		if (GG_FITERROR(gfit) == NO && GG_PLOTTYPE(gfit) == FIT)
		    call geolinxy (gd, fit, sx1, sy1, sx2, sy2, xref, yref,
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
		    call geodelete1 (gd, xin, yin, Memr[w], wts, npts, wx, wy,
		        delete)
		case XXRESID:
		    call geodelete2 (gd, xref, Memr[xresid], Memr[w], wts, npts,
		        wx, wy, delete)
		case XYRESID:
		    call geodelete2 (gd, yref, Memr[xresid], Memr[w], wts, npts,
		        wx, wy, delete)
		case YXRESID:
		    call geodelete2 (gd, xref, Memr[yresid], Memr[w], wts, npts,
		        wx, wy, delete)
		case YYRESID:
		    call geodelete2 (gd, yref, Memr[yresid], Memr[w], wts, npts,
		        wx, wy, delete)
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
		        call geomfit (fit, sx1, sx2, xref, yref, xin, Memr[w],
			    Memr[xresid], npts, YES, Memc[xerrmsg], SZ_LINE) 
		        call geomfit (fit, sy1, sy2, xref, yref, yin, Memr[w],
			    Memr[yresid], npts, NO, Memc[yerrmsg], SZ_LINE) 
		        if (GM_REJECT(fit) > 0.)
			    call geomreject (fit, sx1, sy1, sx2, sy2, xref,
			        yref, xin, yin, Memr[w], Memr[xresid],
				Memr[yresid], npts, Memc[xerrmsg], SZ_LINE,
				Memc[yerrmsg], SZ_LINE)
		        else
			    GM_NREJECT(fit) = 0
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
		    call geolabel (FIT, gt1, fit)
	    	    call geograph1 (gd, gt1, fit, gfit, xref, yref, xin, yin,
		        Memr[w], npts)
		    if (GG_CONSTXY(gfit) == YES)
			call geoconxy (gd, fit, sx1, sy1, sx2, sy2)
		case XXRESID:
		    call geolabel (XXRESID, gt2, fit)
	    	    call geograph2 (gd, gt2, fit, gfit, xref, Memr[xresid],
		        Memr[w], npts)
		case XYRESID:
		    call geolabel (XYRESID, gt3, fit)
	    	    call geograph2 (gd, gt3, fit, gfit, yref, Memr[xresid],
		         Memr[w], npts)
		case YXRESID:
		    call geolabel (YXRESID, gt4, fit)
	    	    call geograph2 (gd, gt4, fit, gfit, xref, Memr[yresid],
		         Memr[w], npts)
		case YYRESID:
		    call geolabel (YYRESID, gt5, fit)
	    	    call geograph2 (gd, gt5, fit, gfit, yref, Memr[yresid],
		         Memr[w], npts)
		}
	        call printf ("%s  %s\n")
	    	    call pargstr (Memc[xerrmsg])
	    	    call pargstr (Memc[yerrmsg])
		newgraph = NO
	    }
	}

	# free space
	call gt_free (gt1)
	call gt_free (gt2)
	call gt_free (gt3)
	call gt_free (gt4)
	call gt_free (gt5)
	call sfree (sp)

	# call an error if appropriate
	if (errcode > 0)
	    call error (0, errstr)
end


# GEOMGFITD -- Fit the surface using interactive graphics

procedure geomgfitd (gd, fit, sx1, sy1, sx2, sy2, xref, yref, xin,
    yin, wts, npts)

pointer	gd		# graphics file descriptor
pointer	fit		# pointer to the fit structure
pointer	sx1		# pointer to the linear x surface fit
pointer	sy1		# pointer to the linear y surface fit
pointer	sx2		# pointer to higher order x surface fit
pointer	sy2		# pointer to higher order y surface fit
double	xref[npts]	# the x reference coordinates
double	yref[npts]	# the y reference coordinates
double	xin[npts]	# input x coordinates
double	yin[npts]	# input y coordinates
double	wts[npts]	# array of weights
int	npts		# number of data points

char	errstr[SZ_LINE]
double	xshift, yshift, xscale, yscale, thetax, thetay
int	newgraph, delete, wcs, key, errcode
pointer	sp, w, gfit, xresid, yresid, cmd, xerrmsg, yerrmsg
pointer	rxref, ryref, rxin, ryin, rxresid, ryresid, rwts, rw
pointer	gt1, gt2, gt3, gt4, gt5
real	wx, wy

int	gt_gcur(), errget()
pointer	gt_init()
errchk	smark(), salloc(), geomfitd(), geomrejectd()

begin
	# initialize gfit structure and working space
	call smark (sp)
	call salloc (gfit, LEN_GEOGRAPH, TY_STRUCT)
	call salloc (xresid, npts, TY_DOUBLE)
	call salloc (yresid, npts, TY_DOUBLE)
	call salloc (w, npts, TY_DOUBLE)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (xerrmsg, SZ_LINE, TY_CHAR)
	call salloc (yerrmsg, SZ_LINE, TY_CHAR)

	# allocate temporary arrays for graphing programs
	call salloc (rxref, npts, TY_REAL)
	call salloc (ryref, npts, TY_REAL)
	call salloc (rxin, npts, TY_REAL)
	call salloc (ryin, npts, TY_REAL)
	call salloc (rxresid, npts, TY_REAL)
	call salloc (ryresid, npts, TY_REAL)
	call salloc (rwts, npts, TY_REAL)
	call salloc (rw, npts, TY_REAL)


	# do first fit
	call geomfitd (fit, sx1, sx2, xref, yref, xin, wts, Memd[xresid],
	    npts, YES, Memc[xerrmsg], SZ_LINE)
	call geomfitd (fit, sy1, sy2, xref, yref, yin, wts, Memd[yresid],
	    npts, NO, Memc[yerrmsg], SZ_LINE)
	if (GM_REJECT(fit) > 0.)
	    call geomrejectd (fit, sx1, sy1, sx2, sy2, xref, yref, xin, yin,
	        wts, Memd[xresid], Memd[yresid], npts, Memc[xerrmsg],
		SZ_LINE, Memc[yerrmsg], SZ_LINE)
	else
	    GM_NREJECT(fit) = 0
	GG_NEWFUNCTION(gfit) = NO
	GG_FITERROR(gfit) = NO
	errcode = OK

	# set up plotting arrays
	call achtdr (xref, Memr[rxref], npts)
	call achtdr (yref, Memr[ryref], npts)
	call achtdr (xin, Memr[rxin], npts)
	call achtdr (yin, Memr[ryin], npts)
	call achtdr (wts, Memr[rwts], npts)
	call achtdr (Memd[xresid], Memr[rxresid], npts)
	call achtdr (Memd[yresid], Memr[ryresid], npts)

	# set up real weight arrays for plotting
	call amovd (wts, Memd[w], npts)
	call achtdr (Memd[w], Memr[rw], npts)

	# set up plotting defaults
	GG_PLOTTYPE(gfit) = FIT
	GG_OVERPLOT(gfit) = NO
	GG_CONSTXY(gfit) = YES
	newgraph = NO

	# allocate graphics tools
	gt1 = gt_init ()
	gt2 = gt_init ()
	gt3 = gt_init ()
	gt4 = gt_init ()
	gt5 = gt_init ()

	# set the plot title and x and y axis labels
	call geogtset (FIT, gt1, fit)
	call geogtset (XXRESID, gt2, fit)
	call geogtset (XYRESID, gt3, fit)
	call geogtset (YXRESID, gt4, fit)
	call geogtset (YYRESID, gt5, fit)

	# make the first plot
	call gclear (gd)
	call geolabel (FIT, gt1, fit)
	call geograph1 (gd, gt1, fit, gfit, Memr[rxref], Memr[ryref],
	    Memr[rxin], Memr[ryin], Memr[rw], npts)
	if (GG_CONSTXY(gfit) == YES)
	    call geoconxyd (gd, fit, sx1, sy1, sx2, sy2)
	call printf ("%s  %s\n")
	    call pargstr (Memc[xerrmsg])
	    call pargstr (Memc[yerrmsg])

	# read the cursor commands
	while (gt_gcur ("cursor", wx, wy, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {

	    switch (key) {

	    case '?':
		call gpagefile (gd, HELPFILE, "")

	    case ':':
		call geocolon (gd, fit, gfit, Memc[cmd], newgraph)
		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call gt_colon (Memc[cmd], gt1, newgraph)
		case XXRESID:
		    call gt_colon (Memc[cmd], gt2, newgraph)
		case XYRESID:
		    call gt_colon (Memc[cmd], gt3, newgraph)
		case YXRESID:
		    call gt_colon (Memc[cmd], gt4, newgraph)
		case YYRESID:
		    call gt_colon (Memc[cmd], gt5, newgraph)
		}

	    case 'l':
		if (GG_FITERROR(gfit) == NO) {
		    call lincoeffd (fit, sx1, sy1, xshift, yshift, xscale,
		        yscale, thetax, thetay)
		    call printf ("xshift: %.2f yshift: %.2f ")
		        call pargd (xshift)
		        call pargd (yshift)
		    call printf ("xmag: %.2f ymag: %.2f ")
		        call pargd (xscale)
		        call pargd (yscale)
		    call printf ("xrot: %.2f yrot: %.2f\n")
		        call pargd (thetax)
		        call pargd (thetay)
		}

	    case 't':
		if (GG_FITERROR(gfit) == NO && GG_PLOTTYPE(gfit) == FIT)
		    call geolinxyd (gd, fit, sx1, sy1, sx2, sy2, xref,
		        yref, xin, yin, npts, wx, wy)

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
		    call geodelete1 (gd, Memr[rxin], Memr[ryin], Memr[rw],
		        Memr[rwts], npts, wx, wy, delete)
		case XXRESID:
		    call geodelete2 (gd, Memr[rxref], Memr[rxresid],
		        Memr[rw], Memr[rwts], npts, wx, wy, delete)
		case XYRESID:
		    call geodelete2 (gd, Memr[ryref], Memr[rxresid],
		        Memr[rw], Memr[rwts], npts, wx, wy, delete)
		case YXRESID:
		    call geodelete2 (gd, Memr[rxref], Memr[ryresid],
		        Memr[rw], Memr[rwts], npts, wx, wy, delete)
		case YYRESID:
		    call geodelete2 (gd, Memr[ryref], Memr[ryresid],
		        Memr[rw], Memr[rwts], npts, wx, wy, delete)
		}
		call achtrd (Memr[rw], Memd[w], npts)

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

	    case  'f':
		# do the fit
		if (GG_NEWFUNCTION(gfit) == YES) {
		    iferr {
		        call geomfitd (fit, sx1, sx2, xref, yref, xin, Memd[w],
			    Memd[xresid], npts, YES, Memc[xerrmsg], SZ_LINE) 
		        call geomfitd (fit, sy1, sy2, xref, yref, yin, Memd[w],
			    Memd[yresid], npts, NO, Memc[yerrmsg], SZ_LINE) 
			if (GM_REJECT(fit) > 0.)
			    call geomrejectd (fit, sx1, sy1, sx2, sy2, xref,
			        yref, xin, yin, Memd[w], Memd[xresid],
				Memd[yresid], npts, Memc[xerrmsg], SZ_LINE,
				Memc[yerrmsg], SZ_LINE)
			else
			    GM_NREJECT(fit) = 0
		        call achtdr (Memd[xresid], Memr[rxresid], npts)
			call achtdr (Memd[yresid], Memr[ryresid], npts)
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

	    # plot the new graph
	    if (newgraph == YES) {
		switch (GG_PLOTTYPE(gfit)) {
		case FIT:
		    call geolabel (FIT, gt1, fit)
	    	    call geograph1 (gd, gt1, fit, gfit, Memr[rxref],
		        Memr[ryref], Memr[rxin], Memr[ryin], Memr[rw], npts)
		    if (GG_CONSTXY(gfit) == YES)
			call geoconxyd (gd, fit, sx1, sy1, sx2, sy2)
		case XXRESID:
		    call geolabel (XXRESID, gt2, fit)
	    	    call geograph2 (gd, gt2, fit, gfit, Memr[rxref],
		        Memr[rxresid], Memr[rw], npts)
		case XYRESID:
		    call geolabel (XYRESID, gt3, fit)
	    	    call geograph2 (gd, gt3, fit, gfit, Memr[ryref],
		        Memr[rxresid], Memr[rw], npts)
		case YXRESID:
		    call geolabel (YXRESID, gt4, fit)
	    	    call geograph2 (gd, gt4, fit, gfit, Memr[rxref],
		        Memr[ryresid], Memr[rw], npts)
		case YYRESID:
		    call geolabel (YYRESID, gt5, fit)
	    	    call geograph2 (gd, gt5, fit, gfit, Memr[ryref],
		        Memr[ryresid], Memr[rw], npts)
		}

		call printf ("%s  %s\n")
	    	    call pargstr (Memc[xerrmsg])
	    	    call pargstr (Memc[yerrmsg])
		newgraph = NO
	    }
	}

	# free space
	call gt_free (gt1)
	call gt_free (gt2)
	call gt_free (gt3)
	call gt_free (gt4)
	call gt_free (gt5)
	call sfree (sp)

	if (errcode > 0)
	    call error (0, errstr)
end
