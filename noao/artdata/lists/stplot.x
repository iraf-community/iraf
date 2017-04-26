include <mach.h>
include <math.h>
include <gset.h>
include <pkg/gtools.h>
include "starlist.h"

define	HELPFILE1  "artdata$lists/starlist.key"

# ST_PLOTS -- Interactively examine the spatial density and luminosity
# functions.

procedure st_plots (sf, lf, gd, st, x, y, mag)

int	sf			# spatial density file descriptor
int	lf			# luminsosity function file descriptor
pointer	gd			# graphics stream pointer
pointer	st			# pointer to starlist structure
pointer	x			# pointer to x array
pointer	y			# pointer to y array
pointer mag			# pointer to mag array

int	wcs, key, plottype, newplot, newspace, newlum
pointer	sp, cmd, gt1, gt2, gt3
real	wx, wy
int	gt_gcur()
pointer	gt_init()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Intialize the plots.
	gt1 = gt_init()
	gt2 = gt_init()
	gt3 = gt_init()

	newspace = NO
	newlum = NO
	newplot = NO
	plottype = 1

	# Draw the first plot.
	call st_pfield (gd, gt1, st, Memr[x], Memr[y], Memr[mag], ST_NSTARS(st))

	while (gt_gcur ("cursor", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {
	    switch (key) {

	    case '?':
		call gpagefile (gd, HELPFILE1, "")

	    case ':':
		call st_colon (gd, st, sf, lf, Memc[cmd], newspace, newlum,
		    newplot)
		switch (plottype) {
		case 1:
		    call gt_colon (Memc[cmd], gd, gt1, newplot)
		case 2:
		    call gt_colon (Memc[cmd], gd, gt2, newplot)
		case 3:
		    call gt_colon (Memc[cmd], gd, gt3, newplot)
		}

	    case 'q':
		break

	    case 'f':

		if (newspace == YES) {
		    call st_mkspatial (sf, st, x, y, mag)
		    newspace = NO
		    newplot = YES
		}

		if (newlum == YES) {
		    call st_mklum (lf, st, x, y, mag)
		    newlum = NO
		    newplot = YES
		}

		if (newplot == YES) {
		    switch (plottype) {
		    case 1:
			call st_pfield (gd, gt1, st, Memr[x], Memr[y],
			    Memr[mag], ST_NSTARS(st))
		    case 2:
		        call st_prhist (gd, gt2, st, Memr[x], Memr[y],
		            ST_NSTARS(st))
		    case 3:
			call st_pmhist (gd, gt3, st, Memr[mag], ST_NSTARS(st))
		    default:
			call st_pfield (gd, gt1, st, Memr[x], Memr[y],
			    Memr[mag], ST_NSTARS(st))
		    }
		    newplot = NO
		}

	    case 'x':
		if (newspace == YES || newlum == YES)
		    call printf ("Type the f key to remake the star list\n")
		else if (plottype != 1 || newplot == YES) {
		    call st_pfield (gd, gt1, st, Memr[x], Memr[y], Memr[mag],
		        ST_NSTARS(st))
		    plottype = 1
		    newplot = NO
		}

	    case 'r':
		if (newspace == YES)
		    call printf ("Type the f key to remake the star list\n")
		else if (plottype != 2 || newplot == YES) {
		    call st_prhist (gd, gt2, st, Memr[x], Memr[y],
		        ST_NSTARS(st))
		    plottype = 2
		    newplot = NO
		}

	    case 'm':
		if (newlum == YES)
		    call printf ("Type the f key to remake the star list\n")
		else if (plottype != 3 || newplot == YES) {
		    call st_pmhist (gd, gt3, st, Memr[mag], ST_NSTARS(st))
		    plottype = 3
		    newplot = NO
		}

	    default:
	    }
	}

	# Recompute the results if necessary.
	if (newspace == YES)
	    call st_mkspatial (sf, st, x, y, mag)
	if (newlum == YES)
	    call st_mklum (lf, st, x, y, mag)

	# Free space for the plots.
	call gt_free (gt1)
	call gt_free (gt2)
	call gt_free (gt3)

	call sfree (sp)
end


define	HELPFILE2  "artdata$lists/gallist.key"

# ST_GPLOTS -- Fit the luminosity function and make plots.

procedure st_gplots (sf, lf, gd, st, x, y, mag, egal, axis, round, phi)

int	sf			# spatial distribution file descriptor
int	lf			# luminsosity distribution file descriptor
pointer	gd			# graphics stream pointer
pointer	st			# pointer to starlist structure
pointer	x			# pointer to x array
pointer	y			# pointer to y array
pointer mag			# pointer to mag array
pointer	egal			# pointer to type array
pointer axis			# pointer to the diameters array
pointer	round			# pointer to the roundness array
pointer	phi			# pointer to the position angle array

int	wcs, key, plottype
int	newplot, newspace, newlum, newmix, newaxis, newround, newphi
pointer	sp, cmd, gt1, gt2, gt3, gt4, gt5, gt6
real	wx, wy
int	gt_gcur()
pointer	gt_init()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Intialize the plots.
	gt1 = gt_init()
	gt2 = gt_init()
	gt3 = gt_init()
	gt4 = gt_init()
	gt5 = gt_init()
	gt6 = gt_init()

	newspace = NO
	newlum = NO
	newmix = NO
	newaxis = NO
	newround = NO
	newplot = NO
	newphi = NO
	plottype = 1

	# Draw the first plot.
	call st_pgfield (gd, gt1, st, Memr[x], Memr[y], Memr[mag], Memi[egal],
	    Memr[axis], Memr[round], ST_NSTARS(st))

	while (gt_gcur ("cursor", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {
	    switch (key) {

	    case '?':
		call gpagefile (gd, HELPFILE2, "")

	    case ':':
		call st_gcolon (gd, st, sf, lf, Memc[cmd], newspace, newlum,
		    newmix, newaxis, newround, newphi, newplot)
		switch (plottype) {
		case 1:
		    call gt_colon (Memc[cmd], gd, gt1, newplot)
		case 2:
		    call gt_colon (Memc[cmd], gd, gt2, newplot)
		case 3:
		    call gt_colon (Memc[cmd], gd, gt3, newplot)
		case 4:
		    call gt_colon (Memc[cmd], gd, gt4, newplot)
		case 5:
		    call gt_colon (Memc[cmd], gd, gt5, newplot)
		case 6:
		    call gt_colon (Memc[cmd], gd, gt6, newplot)
		}

	    case 'q':
		break

	    case 'f':


		if (newphi == YES) {
		    call st_gmkphi (st, x, y, mag, egal, axis, round, phi)
		    newphi = NO
		    newplot = YES
		}

		if (newspace == YES) {
		    call st_gmkspatial (sf, st, x, y, mag, egal, axis, round,
			phi)
		    newspace = NO
		    newplot = YES
		}

		if (newmix == YES) {
		    call st_gmkmix (st, x, y, mag, egal, axis, round, phi)
		    newmix = NO
		    newplot = YES
		}

		if (newlum == YES) {
		    call st_gmklum (lf, st, x, y, mag, egal, axis, round, phi)
		    call st_gmkround (st, x, y, mag, egal, axis, round, phi)
		    call st_gmkaxis (st, x, y, mag, egal, axis, round, phi)
		    newlum = NO
		    newaxis = NO
		    newround = NO
		    newplot = YES
		}

		if (newround == YES) {
		    call st_gmkround (st, x, y, mag, egal, axis, round, phi)
		    newround = NO
		    newplot = YES
		}

		if (newaxis == YES) {
		    call st_gmkaxis (st, x, y, mag, egal, axis, round, phi)
		    newaxis = NO
		    newplot = YES
		}

		if (newplot == YES) {
		    switch (plottype) {
		    case 1:
			call st_pgfield (gd, gt1, st, Memr[x], Memr[y],
			    Memr[mag], Memi[egal], Memr[axis], Memr[round],
			    ST_NSTARS(st))
		    case 2:
		        call st_prhist (gd, gt2, st, Memr[x], Memr[y],
		            ST_NSTARS(st))
		    case 3:
			call st_pmhist (gd, gt3, st, Memr[mag], ST_NSTARS(st))
		    case 4:
			call st_pdhist (gd, gt4, st, Memr[axis], ST_NSTARS(st))
		    case 5:
			call st_pehist (gd, gt5, st, Memr[round], ST_NSTARS(st))
		    case 6:
			call st_pphist (gd, gt6, st, Memr[phi], ST_NSTARS(st))
		    default:
			call st_pgfield (gd, gt1, st, Memr[x], Memr[y],
			    Memr[mag], Memi[egal], Memr[axis], Memr[round],
			    ST_NSTARS(st))
		    }
		    newplot = NO
		}

	    case 'x':
		if (newspace == YES || newlum == YES || newmix == YES ||
		    newround == YES)
		    call printf ("Type the f key to refit the galaxies list\n")
		else if (plottype != 1 || newplot == YES) {
		    call st_pgfield (gd, gt1, st, Memr[x], Memr[y], Memr[mag],
		        Memi[egal], Memr[axis], Memr[round], ST_NSTARS(st))
		    plottype = 1
		    newplot = NO
		}

	    case 'r':
		if (newspace == YES)
		    call printf ("Type the f key to refit the galaxies list\n")
		else if (plottype != 2 || newplot == YES) {
		    call st_prhist (gd, gt2, st, Memr[x], Memr[y],
		        ST_NSTARS(st))
		    plottype = 2
		    newplot = NO
		}

	    case 'm':
		if (newlum == YES)
		    call printf ("Type the f key to refit the galaxies list\n")
		else if (plottype != 3 || newplot == YES) {
		    call st_pmhist (gd, gt3, st, Memr[mag], ST_NSTARS(st))
		    plottype = 3
		    newplot = NO
		}

	    case 'd':
		if (newlum == YES)
		    call printf ("Type the f key to refit the galaxies list\n")
		else if (plottype != 4 || newplot == YES) {
		    call st_pdhist (gd, gt4, st, Memr[axis], ST_NSTARS(st))
		    plottype = 4
		    newplot = NO
		}

	    case 'e':
		if (newround == YES)
		    call printf ("Type the f key to refit the galaxies list\n")
		else if (plottype != 5 || newplot == YES) {
		    call st_pehist (gd, gt5, st, Memr[round], ST_NSTARS(st))
		    plottype = 5
		    newplot = NO
		}

	    case 'p':
		if (newphi == YES)
		    call printf ("Type the f key to refit the galaxies list\n")
		else if (plottype != 6 || newplot == YES) {
		    call st_pphist (gd, gt6, st, Memr[phi], ST_NSTARS(st))
		    plottype = 6
		    newplot = NO
		}

	    default:
	    }
	}

	# Recompute the functions if necessary.
	if (newphi == YES)
	    call st_gmkphi (st, x, y, mag, egal, axis, round, phi)
	if (newspace == YES)
	    call st_gmkspatial (sf, st, x, y, mag, egal, axis, round, phi)
	if (newmix == YES)
	    call st_gmkmix (st, x, y, mag, egal, axis, round, phi)
	if (newlum == YES) {
	    call st_gmklum (lf, st, x, y, mag, egal, axis, round, phi)
	    call st_gmkaxis (st, x, y, mag, egal, axis, round, phi)
	    call st_gmkround (st, x, y, mag, egal, axis, round, phi)
	} else {
	    if (newaxis == YES)
	        call st_gmkaxis (st, x, y, mag, egal, axis, round, phi)
	    if (newround == YES)
	        call st_gmkround (st, x, y, mag, egal, axis, round, phi)
	}

	# Free space for the plots.
	call gt_free (gt1)
	call gt_free (gt2)
	call gt_free (gt3)
	call gt_free (gt4)
	call gt_free (gt5)
	call gt_free (gt6)

	call sfree (sp)
end


# ST_PFIELD -- Plot distribution of stars in the x-y plane.

procedure st_pfield (gd, gt, st, x, y, mag, npts)

pointer	gd		# pointer to graphics stream
pointer	gt		# pointer to graphics descriptor
pointer	st		# pointer to starlist structure
real	x[ARB]		# x coords
real	y[ARB]		# y coords
real	mag[ARB]	# magnitudes
int	npts		# number of points

int	i
pointer	sp, sizes, par1, par2, title
bool	fp_equalr()

begin
	call smark (sp)
	call salloc (sizes, npts, TY_REAL)
	call salloc (par1, SZ_LINE, TY_CHAR)
	call salloc (par2, SZ_LINE, TY_CHAR)
	call salloc (title, 3 * SZ_LINE, TY_CHAR)

	# Clear screen.
	call gclear (gd)

	# Set the labels.
	call gt_sets (gt, GTXLABEL, "X coordinate")
	call gt_sets (gt, GTYLABEL, "Y coordinate")

	# Set the title.
	call sprintf (Memc[par1], SZ_LINE,
	    "Nstars: %d  Spatial model: %s  Luminosity model: %s\n")
	    call pargi (ST_NSTARS(st))
	    if (ST_SPATIAL(st) == ST_SPFILE)
		call pargstr (ST_SFILE(st))
	    else
		call pargstr (ST_SPSTRING(st))
	    if (ST_LUMINOSITY(st) == ST_LFFILE)
		call pargstr (ST_LFILE(st))
	    else
		call pargstr (ST_LFSTRING(st))
	call sprintf (Memc[par2], SZ_LINE,
	    "X range: %g to %g  Yrange: %g to %g  Mag range: %g to %g\n") 
	    call pargr (ST_XMIN(st))
	    call pargr (ST_XMAX(st))
	    call pargr (ST_YMIN(st))
	    call pargr (ST_YMAX(st))
	    call pargr (ST_MINMAG(st))
	    call pargr (ST_MAXMAG(st))
	call sprintf (Memc[title], 3 * SZ_LINE, "%s%s%s")
	    call pargstr ("MAP OF STARLIST\n")
	    call pargstr (Memc[par1])
	    call pargstr (Memc[par2])
	call gt_sets (gt, GTTITLE, Memc[title])

	# Set the plot axes min and max values.
	call gt_setr (gt, GTXMIN, ST_XMIN(st))
	call gt_setr (gt, GTXMAX, ST_XMAX(st))
	call gt_setr (gt, GTYMIN, ST_YMIN(st))
	call gt_setr (gt, GTYMAX, ST_YMAX(st))

	# Set the window and labels.
	call gt_swind (gd, gt)
	call gt_labax (gd, gt)

	# Plot.
	if (fp_equalr (ST_MAXMAG(st), ST_MINMAG(st)))
	    call amovkr (2.0, Memr[sizes], npts)
	else
	    call amapr (mag, Memr[sizes], npts, ST_MAXMAG(st), ST_MINMAG(st),
		1.0, 4.0)
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTMARK, "box")
	do i = 1, npts
	    call gmark (gd, x[i], y[i], GM_BOX, Memr[sizes+i-1],
		Memr[sizes+i-1])

	call sfree (sp)
end


# ST_PGFIELD -- Plot distribution of stars in the x-y plane.

procedure st_pgfield (gd, gt, st, x, y, mag, egal, axis, round, npts)

pointer	gd		# pointer to graphics stream
pointer	gt		# pointer to plot descriptor
pointer	st		# pointer to starlist structure
real	x[ARB]		# array of x coordinates
real	y[ARB]		# array of y coordinates
real	mag[ARB]	# array of magnitudes
int	egal[ARB]	# array of galaxy types
real	axis[ARB]	# array of diameters
real	round[ARB]	# array of roundness values
int	npts		# number of points

int	i
pointer	sp, par1, par2, title, xsizes
real	amin, amax
bool	fp_equalr()

begin
	call smark (sp)
	call salloc (par1, SZ_LINE, TY_CHAR)
	call salloc (par2, SZ_LINE, TY_CHAR)
	call salloc (title, 3 * SZ_LINE, TY_CHAR)
	call salloc (xsizes, npts, TY_REAL)

	# Clear screen.
	call gclear (gd)

	# Set the labels.
	call gt_sets (gt, GTXLABEL, "X coordinate")
	call gt_sets (gt, GTYLABEL, "Y coordinate")

	# Set the title.
	call sprintf (Memc[par1], SZ_LINE,
	    "Ngals: %d  Spatial model: %s  Luminosity model: %s\n")
	    call pargi (ST_NSTARS(st))
	    if (ST_SPATIAL(st) == ST_SPFILE)
		call pargstr (ST_SFILE(st))
	    else
		call pargstr (ST_SPSTRING(st))
	    if (ST_LUMINOSITY(st) == ST_LFFILE)
		call pargstr (ST_LFILE(st))
	    else
		call pargstr (ST_LFSTRING(st))
	call sprintf (Memc[par2], SZ_LINE,
	    "X range: %g to %g  Yrange: %g to %g  Mag range: %g to %g\n") 
	    call pargr (ST_XMIN(st))
	    call pargr (ST_XMAX(st))
	    call pargr (ST_YMIN(st))
	    call pargr (ST_YMAX(st))
	    call pargr (ST_MINMAG(st))
	    call pargr (ST_MAXMAG(st))
	call sprintf (Memc[title], 3 * SZ_LINE, "%s%s%s")
	    call pargstr ("MAP OF GALLIST\n")
	    call pargstr (Memc[par1])
	    call pargstr (Memc[par2])
	call gt_sets (gt, GTTITLE, Memc[title])

	# Set the x and y axis minimums and maximums.
	call gt_setr (gt, GTXMIN, ST_XMIN(st))
	call gt_setr (gt, GTXMAX, ST_XMAX(st))
	call gt_setr (gt, GTYMIN, ST_YMIN(st))
	call gt_setr (gt, GTYMAX, ST_YMAX(st))

	# Set the window and labels.
	call gt_swind (gd, gt)
	call gt_labax (gd, gt)

	# Compute the marksizes.
	call alimr (axis, npts, amin, amax)
	if (fp_equalr (amin, amax))
	    call amovkr (2.0, Memr[xsizes], npts)
	else
	    call amapr (axis, Memr[xsizes], npts, amin, amax, 1.0, 4.0)

	#call amulr (axis, round, Memr[ysizes], npts)
	#call alimr (Memr[ysizes], npts, amin, amax)
	#call amapr (Memr[ysizes], Memr[ysizes], npts, amin, amax, 1.0, 4.0)

	# Plot.
	call gt_sets (gt, GTTYPE, "mark")
	do i = 1, npts {
	    if (egal[i] == ST_DEVAUC)
	        call gmark (gd, x[i], y[i], GM_CIRCLE, Memr[xsizes+i-1],
		    Memr[xsizes+i-1])
	    else
	        call gmark (gd, x[i], y[i], GM_DIAMOND, Memr[xsizes+i-1],
		    Memr[xsizes+i-1])
	}

	call sfree (sp)
end


# ST_PMHIST -- Plot luminosity function of the stars or galaxies.

procedure st_pmhist (gd, gt, st, mag, npts)

pointer	gd		# pointer to graphics stream
pointer	gt		# pointer to the plot descriptor
pointer	st		# pointer to starlist structure
real	mag[ARB]	# array of magnitudes
int	npts		# number of points

int	i, nbins
pointer	sp, par, title, hx, hgm
real	mval, mmin, mmax, dm, hmin, hmax

begin
	# Compute the parameters of the histogram to be plotted.
	mmin = ST_MINMAG(st)
	if (ST_MBINSIZE(st) <= 0.0) {
	    nbins = 1
	    mmax = ST_MAXMAG(st)
	    dm = mmax - mmin
	} else {
	    dm = ST_MBINSIZE(st)
	    mval = (ST_MAXMAG(st) - mmin) / dm
	    i =  int (mval)
	    if (abs (mval -i ) <= 100.0 * EPSILONR) {
		nbins = i + 1
		mmax = ST_MAXMAG(st)
	    } else {
		mmax = mmin + (i + 1) * dm
		nbins = i + 2
	    }
	}

	# Allocate temporary space.
	call smark (sp)
	call salloc (par, SZ_LINE, TY_CHAR)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (hx, nbins, TY_REAL)
	call salloc (hgm, nbins, TY_REAL)

	# Make the histogram.
	call aclrr (Memr[hgm], nbins)
	call st_hgmr (mag, npts, Memr[hgm], nbins, mmin, mmax)
	call alimr (Memr[hgm], nbins, hmin, hmax)

	# Make the histogram x scale.
	mval = mmin + dm / 2.0
	do i = 1, nbins {
	    Memr[hx+i-1] = mval
	    mval = mval + dm
	}
	mval = mmin + nbins * dm

	# Clear the screen.
	call gclear (gd)

	# Set the labels.
	call gt_sets (gt, GTXLABEL, "Magnitude")
	call gt_sets (gt, GTYLABEL, "N(M)")

	# Set the title.
	call sprintf (Memc[par], SZ_LINE,
	    "%s: %d  Model: %s\nMag: %g to %g in steps of %g\n")
	    if (ST_TYPE(st) == ST_STARS)
		call pargstr ("Nstars")
	    else
		call pargstr ("Ngals")
	    call pargi (ST_NSTARS(st))
	    if (ST_LUMINOSITY(st) == ST_LFFILE)
		call pargstr (ST_LFILE(st))
	    else
		call pargstr (ST_LFSTRING(st))
	    call pargr (mmin)
	    call pargr (mmax)
	    call pargr (dm)
	call sprintf (Memc[title], 2 * SZ_LINE, "%s%s")
	    call pargstr ("LUMINOSITY FUNCTION\n")
	    call pargstr (Memc[par])
	call gt_sets (gt, GTTITLE, Memc[title])

	# Set the mins and maxs.
	call gt_setr (gt, GTXMIN, mmin)
	call gt_setr (gt, GTXMAX, mval)
	call gt_setr (gt, GTYMIN, 0.0)
	call gt_setr (gt, GTYMAX, hmax)

	# Set the window.
	call gt_swind (gd, gt)
	call gt_labax (gd, gt)

	# Plot.
	call gt_sets (gt, GTTYPE, "histogram")
	call gt_plot (gd, gt, Memr[hx], Memr[hgm], nbins)
	call gline (gd, mmin, Memr[hgm], Memr[hx], Memr[hgm])
	call gline (gd, Memr[hx+nbins-1], Memr[hgm+nbins-1], mval,
	    Memr[hgm+nbins-1])

	call sfree (sp)
end


# ST_PRHIST -- Plot radial density distribution of the stars.

procedure st_prhist (gd, gt, st, x, y, npts)

pointer	gd		# pointer to graphics stream
pointer	gt		# pointer to plot descriptor
pointer	st		# pointer to starlist structure
real	x[ARB]		# array of x values
real	y[ARB]		# array of y values
int	npts		# number of points

int	i, nbins
pointer	sp, par, title, r, hx, hgm
real	rval, rmin, rmax, dr, hmin, hmax

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (r, npts, TY_REAL)
	call salloc (par, SZ_LINE, TY_CHAR)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)

	# Compute the radial coordinate values.
	do i = 1, npts {
	    Memr[r+i-1]  = sqrt ((x[i] - ST_XC(st)) ** 2 +
	        (y[i] - ST_YC(st)) ** 2)
	}
	call alimr (Memr[r], npts, rmin, rmax)

	# Compute the size of the histogram.
	rmin = 0.0
	if (ST_RBINSIZE(st) <= 0) {
	    nbins = 1
	    dr = rmax - rmin
	} else {
	    dr = ST_RBINSIZE(st)
	    rval = (rmax - rmin) / dr
	    i = int (rval)
	    if (abs (rval - i) <= 100.0 * EPSILONR)
		nbins = i + 1
	    else {
		rmax = rmin + (i + 1) * dr
		nbins = i + 2
	    }
	}

	# Make the histogram and normalize by area.
	call salloc (hx, nbins, TY_REAL)
	call salloc (hgm, nbins, TY_REAL)
	call aclrr (Memr[hgm], nbins)
	call st_hgmr (Memr[r], npts, Memr[hgm], nbins, rmin, rmax)
	rval = rmin
	do i = 1, nbins {
	    Memr[hgm+i-1] = Memr[hgm+i-1] / ( PI * dr * (2.0 * rval + dr))
	    rval = rval + dr
	}
	call alimr (Memr[hgm], nbins, hmin, hmax)

	# Make the histogram x scale.
	rval = rmin + dr / 2.0
	do i = 1, nbins {
	    Memr[hx+i-1] = rval
	    rval = rval + dr
	}
	rval = rmin + nbins * dr

	# Clear the screen.
	call gclear (gd)

	# Set the labels.
	call gt_sets (gt, GTXLABEL, "Radial Distance")
	call gt_sets (gt, GTYLABEL, "N(R) / Area")

	# Set the title.
	call sprintf (Memc[par], SZ_LINE,
	    "Model: %s  %s: %d\nRadius: %g to %g in steps of %g\n")
	    if (ST_SPATIAL(st) == ST_SPFILE)
		call pargstr (ST_SFILE(st))
	    else
		call pargstr (ST_SPSTRING(st))
	    if (ST_TYPE(st) == ST_STARS)
		call pargstr ("Nstars")
	    else
		call pargstr ("Ngals")
	    call pargi (ST_NSTARS(st))
	    call pargr (rmin)
	    call pargr (rmax)
	    call pargr (dr)
	call sprintf (Memc[title], 2 * SZ_LINE, "%s%s")
	    call pargstr ("RADIAL DENSITY FUNCTION\n")
	    call pargstr (Memc[par])
	call gt_sets (gt, GTTITLE, Memc[title])

	# Set the x and y axes minimum and maximum values.
	call gt_setr (gt, GTXMIN, rmin) 
	call gt_setr (gt, GTXMAX, rval)
	call gt_setr (gt, GTYMIN, 0.0)
	call gt_setr (gt, GTYMAX, hmax)

	# Set the window.
	call gt_swind (gd, gt)
	call gt_labax (gd, gt)

	# Plot.
	call gt_sets (gt, GTTYPE, "histogram")
	call gt_plot (gd, gt, Memr[hx], Memr[hgm], nbins)
	call gline (gd, rmin, Memr[hgm], Memr[hx], Memr[hgm])
	call gline (gd, Memr[hx+nbins-1], Memr[hgm+nbins-1], rval,
	    Memr[hgm+nbins-1])

	call sfree (sp)
end


# ST_PDHIST -- Plot the distribution of galaxy diameters.

procedure st_pdhist (gd, gt, st, axis, npts)

pointer	gd		# pointer to graphics stream
pointer	gt		# pointer to plot descriptor
pointer	st		# pointer to starlist structure
real	axis[ARB]	# array of diameters
int	npts		# number of points

int	i, nbins
pointer	sp, par, title, hx, hgm
real	aval, amin, amax, da, hmin, hmax

begin

	# Allocate space for the histogram.
	call alimr (axis, npts, amin, amax)
	amax = max (ST_ERADIUS(st), ST_SRADIUS(st))
	if (ST_DBINSIZE(st) <= 0) {
	    nbins = 1
	    da = amax - amin
	} else {
	    da = ST_DBINSIZE(st)
	    aval = (amax - amin) / da
	    i = int (aval)
	    if (abs (aval - i) <= 100.0 * EPSILONR)
		nbins = i + 1
	    else {
		amin = amax - (i + 1) * da
		nbins = i + 2
	    }
	}

	# Allocate temporary space.
	call smark (sp)
	call salloc (par, SZ_LINE, TY_CHAR)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (hx, nbins, TY_REAL)
	call salloc (hgm, nbins, TY_REAL)

	# Make the histogram.
	call aclrr (Memr[hgm], nbins)
	call st_hgmr (axis, npts, Memr[hgm], nbins, amin, amax)
	call alimr (Memr[hgm], nbins, hmin, hmax)

	# Make the histogram x scale.
	aval = amin + da / 2.0
	do i = 1, nbins {
	    Memr[hx+i-1] = aval
	    aval = aval + da
	}
	aval = amin + nbins * da

	# Clear the screen.
	call gclear (gd)

	# Set the labels.
	call gt_sets (gt, GTXLABEL, "Half-Flux Radius")
	call gt_sets (gt, GTYLABEL, "N(radius)")

	# Set the title.
	call sprintf (Memc[par], SZ_LINE,
     "Luminosity function: %s  Ngals: %d\nDiameter: %g to %g in steps of %g\n")
	    if (ST_LUMINOSITY(st) == ST_LFFILE)
		call pargstr (ST_LFILE(st))
	    else
		call pargstr (ST_LFSTRING(st))
	    call pargi (ST_NSTARS(st))
	    call pargr (amin)
	    call pargr (amax)
	    call pargr (da)
	call sprintf (Memc[title], 2 * SZ_LINE, "%s%s")
	    call pargstr ("HALF-FLUX RADIUS DISTRIBUTION\n")
	    call pargstr (Memc[par])
	call gt_sets (gt, GTTITLE, Memc[title])

	# Set the mins and maxs.
	call gt_setr (gt, GTXMIN, amin)
	call gt_setr (gt, GTXMAX, aval)
	call gt_setr (gt, GTYMIN, 0.0)
	call gt_setr (gt, GTYMAX, hmax)

	# Set the window.
	call gt_swind (gd, gt)
	call gt_labax (gd, gt)

	# Plot.
	call gt_sets (gt, GTTYPE, "histogram")
	call gt_plot (gd, gt, Memr[hx], Memr[hgm], nbins)
	call gline (gd, amin, Memr[hgm], Memr[hx], Memr[hgm])
	call gline (gd, Memr[hx+nbins-1], Memr[hgm+nbins-1], aval,
	    Memr[hgm+nbins-1])

	call sfree (sp)
end


# ST_PEHIST -- Plot the distribution of galaxy diameters.

procedure st_pehist (gd, gt, st, round, npts)

pointer	gd		# pointer to graphics stream
pointer	gt		# pointer to plot descriptor
pointer	st		# pointer to starlist structure
real	round[ARB]	# array of roundness values
int	npts		# number of points

int	i, nbins
pointer	sp, par, title, hx, hgm
real	eval, emin, emax, de, hmin, hmax

begin
	# Compute the size of the histogram.
	emin = .1
	if (ST_EBINSIZE(st) <= 0) {
	    nbins = 1
	    emax = 1.
	    de = emax - emin
	} else {
	    de = ST_EBINSIZE(st)
	    eval = (1. - emin) / de
	    i = int (eval)
	    if (abs (eval - i) <= 100.0 * EPSILONR) {
		nbins = i + 1
		emax = 1.
	    } else {
		nbins = i + 2
		emax = emin + (i + 1) * de
	    }
	}

	# Allocate temporary space.
	call smark (sp)
	call salloc (par, SZ_LINE, TY_CHAR)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (hx, nbins, TY_REAL)
	call salloc (hgm, nbins, TY_REAL)

	# Make the histogram and normalize by area.
	call aclrr (Memr[hgm], nbins)
	call st_hgmr (round, npts, Memr[hgm], nbins, emin, emax)
	call alimr (Memr[hgm], nbins, hmin, hmax)

	# Make the histogram x scale.
	eval = emin + de / 2.0
	do i = 1, nbins {
	    Memr[hx+i-1] = eval
	    eval = eval + de
	}
	eval = emin + nbins * de

	# Clear the screen.
	call gclear (gd)

	# Set the labels.
	call gt_sets (gt, GTXLABEL, "Axial Ratio")
	call gt_sets (gt, GTYLABEL, "N(Axial Ratio)")

	# Set the title.
	call sprintf (Memc[par], SZ_LINE,
	    "Ngals: %d\nRoundness: %g to %g in steps of %g\n")
	    call pargi (ST_NSTARS(st))
	    call pargr (emin)
	    call pargr (emax)
	    call pargr (de)
	call sprintf (Memc[title], 2 * SZ_LINE, "%s%s")
	    call pargstr ("AXIAL RATIO DISTRIBUTION\n")
	    call pargstr (Memc[par])
	call gt_sets (gt, GTTITLE, Memc[title])

	# Set the mins and maxs.
	call gt_setr (gt, GTXMIN, emin)
	call gt_setr (gt, GTXMAX, eval)
	call gt_setr (gt, GTYMIN, 0.0)
	call gt_setr (gt, GTYMAX, hmax)

	# Set the window.
	call gt_swind (gd, gt)
	call gt_labax (gd, gt)

	# Plot.
	call gt_sets (gt, GTTYPE, "histogram")
	call gt_plot (gd, gt, Memr[hx], Memr[hgm], nbins)
	call gline (gd, emin, Memr[hgm], Memr[hx], Memr[hgm])
	call gline (gd, Memr[hx+nbins-1], Memr[hgm+nbins-1], eval,
	    Memr[hgm+nbins-1])

	call sfree (sp)
end


define	MIN_ANGLE	0.0
define	MAX_ANGLE	360.0

# ST_PPHIST -- Plot the distribution of galaxy diameters.

procedure st_pphist (gd, gt, st, phi, npts)

pointer	gd		# pointer to graphics stream
pointer	gt		# pointer to plot descriptor
pointer	st		# pointer to starlist structure
real	phi[ARB]	# array of position angle values
int	npts		# number of points

int	i, nbins
pointer	sp, par, title, hx, hgm
real	pval, pmin, pmax, dp, hmin, hmax

begin
	# Compute the size of the histogram.
	pmin = MIN_ANGLE
	if (ST_PBINSIZE(st) <= 0) {
	    nbins = 1
	    pmax = MAX_ANGLE
	    dp = pmax - pmin
	} else {
	    dp = ST_PBINSIZE(st)
	    pval = (MAX_ANGLE - pmin) / dp
	    i = int (pval)
	    if (abs (pval - i) <= 100.0 * EPSILONR) {
		nbins = i + 1
		pmax = MAX_ANGLE
	    } else {
	        pmax = MIN_ANGLE + (i + 1) * dp
		nbins = i + 2
	    }
	}

	# Allocate temporary space.
	call smark (sp)
	call salloc (par, SZ_LINE, TY_CHAR)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (hx, nbins, TY_REAL)
	call salloc (hgm, nbins, TY_REAL)

	# Make the histogram.
	call aclrr (Memr[hgm], nbins)
	call st_hgmr (phi, npts, Memr[hgm], nbins, pmin, pmax)
	call alimr (Memr[hgm], nbins, hmin, hmax)


	# Make the histogram x scale.
	pval = dp / 2.0
	do i = 1, nbins {
	    Memr[hx+i-1] = pval
	    pval = pval + dp
	}
	pval = pmin + nbins * dp

	# Clear the screen.
	call gclear (gd)

	# Set the axis labels.
	call gt_sets (gt, GTXLABEL, "Position Angle")
	call gt_sets (gt, GTYLABEL, "N(Position Angle)")

	# Set the title.
	call sprintf (Memc[par], SZ_LINE,
	    "Ngals: %d\nPosition angle: %g to %g in steps of %g\n")
	    call pargi (ST_NSTARS(st))
	    call pargr (pmin)
	    call pargr (pmax)
	    call pargr (dp)
	call sprintf (Memc[title], 2 * SZ_LINE, "%s%s")
	    call pargstr ("POSITION ANGLE DISTRIBUTION\n")
	    call pargstr (Memc[par])
	call gt_sets (gt, GTTITLE, Memc[title])

	# Set the axis mins and maxs.
	call gt_setr (gt, GTXMIN, pmin)
	call gt_setr (gt, GTXMAX, pval)
	call gt_setr (gt, GTYMIN, 0.0)
	call gt_setr (gt, GTYMAX, hmax)

	# Set the window.
	call gt_swind (gd, gt)
	call gt_labax (gd, gt)

	# Plot.
	call gt_sets (gt, GTTYPE, "histogram")
	call gt_plot (gd, gt, Memr[hx], Memr[hgm], nbins)
	call gline (gd, pmin, Memr[hgm], Memr[hx], Memr[hgm])
	call gline (gd, Memr[hx+nbins-1], Memr[hgm+nbins-1], pval,
	    Memr[hgm+nbins-1])

	call sfree (sp)
end


# ST_HGMR -- Accumulate the histogram of the input vector.  The output vector
# hmg (the histogram) should be cleared prior to the first call.

procedure st_hgmr (data, npix, hgm, nbins, z1, z2)

real 	data[ARB]		# data vector
int	npix			# number of pixels
real	hgm[ARB]		# output histogram
int	nbins			# number of bins in histogram
real	z1, z2			# greyscale values of first and last bins

int	bin, i
real	z, dz

begin
	dz = real (nbins - 1) / real (z2 - z1)
	if (abs (dz - 1.0) < (EPSILONR * 2.0)) {
	    do i = 1, npix {
		z = data[i]
	        if (z < z1 || z > z2)
		    next
	        bin = int (z - z1) + 1
	        hgm[bin] = hgm[bin] + 1.0
	    }
	} else {
	    do i = 1, npix {
		z = data[i]
	        if (z < z1 || z > z2)
		    next
	        bin = int ((z - z1) * dz) + 1
	        hgm[bin] = hgm[bin] + 1.0
	    }
	}
end
