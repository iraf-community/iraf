include <mach.h>
include	<ctype.h>
include "../lib/daophotdef.h"
include "../lib/psfdef.h"

define	HELPFILE   "daophot$psf/showpsf.key"

# DP_SHOWPSF -- Interactively make surface and/or contour plots of the
# data subraster around the current PSF star.

procedure dp_showpsf (dao, im, subrast, ncols, nlines, x1, y1, gd, star_ok)

pointer	dao			# pointer to DAOPHOT structure
pointer	im			# the input image descriptor
real	subrast[ncols,nlines]	# image subraster
int	ncols, nlines		# dimensions of the subraster
int	x1, y1			# coordinates of left hand corner
pointer	gd			# pointer to the graphics stream
bool	star_ok			# true if PSF star ok

real	hibad, wx, wy, rval
pointer	sp, cmd, title, psf
int	wcs, key, ip
bool	do_replot

int	clgcur(), ctor()

begin
	# Return if the graphics stream is undefined.
	if (gd == NULL)
	    return

	# Allocate working space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)

	# Initialize various daophot pointers.
	psf = DP_PSF (dao)

	# Initialize the contour plot parameters.
	DP_CCEILING (psf) = 0.0
	DP_CFLOOR (psf) = 0.0

	# Define the hibad parameter.
	hibad = DP_MAXGDATA(dao)
	if (IS_INDEFR(hibad))
	    hibad = MAX_REAL

	# Create the plot title.
	call dp_ltov (im, DP_CUR_PSFX(psf), DP_CUR_PSFY(psf), wx, wy, 1)
	call sprintf (Memc[title], SZ_LINE, "Star: %d  X: %g  Y: %g  Mag: %g\n")
	    call pargi (DP_CUR_PSFID(psf))
	    call pargr (wx)
	    call pargr (wy)
	    call pargr (DP_CUR_PSFMAG(psf))

	# Initialize plot.
	if (DP_PLOTTYPE(psf) == PSF_MESHPLOT)
	    call dp_surfpsf (dao, subrast, ncols, nlines, Memc[title], gd)
	else if (DP_PLOTTYPE(psf) == PSF_CONTOURPLOT)
	    call dp_contpsf (dao, subrast, ncols, nlines, Memc[title], gd)
	else if (DP_PLOTTYPE(psf) == PSF_RADIALPLOT)
	    call dp_radpsf (dao, subrast, ncols, nlines, x1, y1, Memc[title],
	        gd)

	if (DP_CUR_PSFMAX(psf) > hibad)
	    call printf ("Warning: Star is probably saturated\n")
	do_replot = false

	while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {

	    switch (key) {

	    # Print the help page
	    case '?':
		if (gd == NULL)
		    call pagefile (HELPFILE, "")
		else
		    call gpagefile (gd, HELPFILE, "")

	    # Print out the photometry for the star.
	    case 'p':
		call printf ("Star: %d  Mag: %7.2f  Coords: %7.2f %7.2f\n")
		    call pargi (DP_CUR_PSFID(psf))
		    call pargr (DP_CUR_PSFMAG(psf))
		    call pargr (DP_CUR_PSFX(psf))
		    call pargr (DP_CUR_PSFY(psf))

	    # Print out the plot parameters.
	    case 't':
	        if (DP_PLOTTYPE(psf) == PSF_MESHPLOT) {
		    call printf ("Surface plot: angv = %6.1f angh = %6.1f ")
		        call pargr (DP_MANGV(psf))
		        call pargr (DP_MANGH(psf))
		    call printf ("  Minimum: %7.1f   Maximum: %7.1f\n")
		        call pargr (DP_CUR_PSFMIN(psf))
		        call pargr (DP_CUR_PSFMAX(psf))
		} else if (DP_PLOTTYPE(psf) == PSF_CONTOURPLOT) {
		    call printf ("Contour plot: floor = %6.1f ceil = %6.1f ")
		        call pargr (DP_CFLOOR(psf))
		        call pargr (DP_CCEILING(psf))
		    call printf ("   Minimum: %7.1f   Maximum: %7.1f\n")
		        call pargr (DP_CUR_PSFMIN(psf))
		        call pargr (DP_CUR_PSFMAX(psf))
		} else if (DP_PLOTTYPE(psf) == PSF_RADIALPLOT) {
		    call printf (
		        "Profile plot: Minimum: %7.1f   Maximum: %7.1f\n")
		        call pargr (DP_CUR_PSFMIN(psf))
		        call pargr (DP_CUR_PSFMAX(psf))
		} else
		    call printf ("Unknown plot type.\n")

	    # Accept star and quit cursor loop.
	    case 'a':
		star_ok = true
		break

	    # Star rejected
	    case 'd':
		star_ok = false
		break

	    # Increase vertical viewing angle of mesh plot by 15 degrees.
	    case 'n':
	        if (DP_PLOTTYPE(psf) == PSF_MESHPLOT) {
		    DP_MANGV (psf) = DP_MANGV (psf) + 15.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Decrease vertical viewing angle of mesh plot by 15 degrees.
	    case 's':
	        if (DP_PLOTTYPE(psf) == PSF_MESHPLOT) {
		    DP_MANGV (psf) = DP_MANGV (psf) - 15.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Decrease horizontal viewing angle of mesh plot by 15 degrees.
	    case 'w':
	        if (DP_PLOTTYPE(psf) == PSF_MESHPLOT) {
		    DP_MANGH (psf) = DP_MANGH (psf) - 15.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Increase horizontal viewing angle of mesh plot by 15 degrees.
	   case 'e':
	        if (DP_PLOTTYPE(psf) == PSF_MESHPLOT) {
		    DP_MANGH (psf) = DP_MANGH (psf) + 15.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Plot the default mesh plot.
	    case 'm':
	        DP_PLOTTYPE(psf) = PSF_MESHPLOT
		DP_MANGV (psf) = 30.
		DP_MANGH (psf) = -30.
		DP_MFLOOR(psf) = 0.0
		DP_MCEILING(psf) = 0.0
		do_replot = true

	    # Plot the default contour plots.
	    case 'c':
	        DP_PLOTTYPE(psf) = PSF_CONTOURPLOT
		DP_CFLOOR(psf) = 0.0
		DP_CCEILING(psf) = 0.0
		do_replot = true

	    # Plot the radial profile plots.
	    case 'r':
	        DP_PLOTTYPE(psf) = PSF_RADIALPLOT
		do_replot = true

	    # Command mode.
	    case ':':
		for (ip=1;  IS_WHITE (Memc[cmd+ip-1]);  ip=ip+1)
		    ;

		switch (Memc[cmd+ip-1]) {

		# Set surface plot and angles
		case 'm':
		    DP_PLOTTYPE(psf) = PSF_MESHPLOT
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, DP_MANGV(psf)) <= 0)
			DP_MANGV(psf) = 30.
		    if (ctor (Memc[cmd], ip, DP_MANGH(psf)) <= 0)
			DP_MANGH(psf) = -30.
		    do_replot = true

		case 'c':
		    # Set surface contour and levels
		    DP_PLOTTYPE(psf) = PSF_CONTOURPLOT
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, DP_CFLOOR(psf)) <= 0)
			DP_CFLOOR(psf) = 0.0
		    if (ctor (Memc[cmd], ip, DP_CCEILING(psf)) <= 0)
			DP_CCEILING(psf) = 0.0
		    do_replot = true

		case 'r':
		    # PLot radial profile.
		    DP_PLOTTYPE(psf) = PSF_RADIALPLOT
		    do_replot = true

		# Set vertical angle
		case 'v':
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, rval) <= 0) {
			call printf (
			    "Surface plot vertical viewing angle: %g\n")
			    call pargr (DP_MANGV(psf))
		    } else {
			DP_MANGV(psf) = rval
			if (DP_PLOTTYPE(psf) == PSF_MESHPLOT)
		            do_replot = true
		    }

		# Set horizontal angle
		case 'h':
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, rval) <= 0) {
			call printf (
			    "Surface plot horizontal viewing angle: %g\n")
			    call pargr (DP_MANGH(psf))
		    } else {
			DP_MANGH(psf) = rval
			if (DP_PLOTTYPE(psf) == PSF_MESHPLOT)
		            do_replot = true
		    }

		# Set the floor value.
		case 'l':
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, rval) <= 0) {
			call printf (
			    "Contour plot floor value: %g\n")
			    call pargr (DP_CFLOOR(psf))
		    } else {
			DP_CFLOOR(psf) = rval
			if (DP_PLOTTYPE(psf) == PSF_CONTOURPLOT)
		    	    do_replot = true
		    }

		# Set the ceiling value.
		case 'u':
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, rval) <= 0) {
			call printf (
			    "Contour plot ceiling value: %g\n")
			    call pargr (DP_CCEILING(psf))
		    } else {
			DP_CCEILING(psf) = rval
			if (DP_PLOTTYPE(psf) == PSF_CONTOURPLOT)
		    	    do_replot = true
		    }

		default:
		    call printf ("Unknown keystroke or cursor command.\007\n")
	        }

	    default:
		call printf ("Unknown keystroke or cursor command.\007\n")
	    }

	    # Replot the data.
	    if (do_replot) {
		if (DP_PLOTTYPE(psf) == PSF_MESHPLOT)
	    	    call dp_surfpsf (dao, subrast, ncols, nlines, Memc[title],
		        gd)
		else if (DP_PLOTTYPE(psf) == PSF_CONTOURPLOT)
	    	    call dp_contpsf (dao, subrast, ncols, nlines, Memc[title],
		        gd)
		else if (DP_PLOTTYPE(psf) == PSF_RADIALPLOT)
	    	    call dp_radpsf (dao, subrast, ncols, nlines, x1, y1,
		        Memc[title], gd)
		do_replot = false
	    }
	}

	call gdeactivate (gd, 0)
	call sfree (sp)
end
