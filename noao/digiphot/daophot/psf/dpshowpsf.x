include	<ctype.h>
include	"../lib/daophot.h"
include "../lib/daophotdef.h"
include "../lib/psfdef.h"
include "../lib/psf.h"

define	HELPFILE   "daophot$psf/showpsf.key"

# DP_SHOWPSF -- Do a plot of either the PSF star or the PSF star minus
# the fitted Gaussian

procedure dp_showpsf (dao, subrast, ncols, nlines, gd, star_ok)

pointer	dao			# pointer to DAOPHOT structure
real	subrast[ncols,nlines]	# image subraster
int	ncols, nlines		# dimensions of the subraster
pointer	gd			# pointer to the graphics stream
bool	star_ok			# true if PSF star ok

bool	do_replot
int	wcs, key, ip
pointer	sp, cmd, title, psf, psfpl
real	wx, wy, rval

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
	psf   = DP_PSF (dao)
	psfpl = DP_PSFPLOT (psf)

	# Initialize the contour plot parameters.
	DP_CCEIL (psfpl) = 0.0
	DP_CFLOOR (psfpl) = 0.0
	DP_CZERO (psfpl) = DP_PSFMIN (psf)

	# Create the plot title.
	call sprintf (Memc[title], SZ_LINE, "Star: %d  X: %g  Y: %g  Mag: %g\n")
	    call pargi (DP_CUR_PSFID(psf))
	    call pargr (DP_CUR_PSFX(psf))
	    call pargr (DP_CUR_PSFY(psf))
	    call pargr (DP_CUR_PSFMAG(psf))

	# Initialize plot.
	if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT)
	    call dp_surfpsf (dao, subrast, ncols, nlines, Memc[title], gd)
	else if (DP_PLOT_TYPE(psfpl) == DP_CONTOURPLOT)
	    call dp_contpsf (dao, subrast, ncols, nlines, Memc[title], gd)
	do_replot = false

	while (clgcur ("cursor", wx, wy, wcs, key, Memc[cmd], SZ_LINE) != EOF) {

	    switch (key) {

	    # Quit the cursor loop
	    case 'o':
		star_ok = true
		break

	    # Print the help page
	    case '?':
		if (gd == NULL)
		    call pagefile (HELPFILE, "")
		else
		    call gpagefile (gd, HELPFILE, "")

	    # Increase vertical viewing angle of mesh plot by 15 degrees.
	    case 'u':
	        if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT) {
		    DP_MANGV (psfpl) = DP_MANGV (psfpl) + 15.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Decrease vertical viewing angle of mesh plot by 15 degrees.
	    case 'd':
	        if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT) {
		    DP_MANGV (psfpl) = DP_MANGV (psfpl) - 15.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Decrease horizontal viewing angle of mesh plot by 15 degrees.
	    case 'l':
	        if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT) {
		    DP_MANGH (psfpl) = DP_MANGH (psfpl) - 15.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Increase horizontal viewing angle of mesh plot by 15 degrees.
	   case 'r':
	        if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT) {
		    DP_MANGH (psfpl) = DP_MANGH (psfpl) + 15.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Plot the mesh plots.
	    case 'm':
	        if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT) {
		    DP_MANGV (psfpl) = 30.
		    DP_MANGH (psfpl) = -30.
		    do_replot = true
		} else
		    call printf ("This key only active for mesh plots.\n")

	    # Reset the contour plots.
	    case 'c':
	        if (DP_PLOT_TYPE(psfpl) == DP_CONTOURPLOT) {
		    DP_CFLOOR(psfpl) = 0.0
		    DP_CCEIL(psfpl) = 0.0
		    do_replot = true
		} else
		    call printf ("This key only active for contour plots.\n")

	    # Print out the photometry for the star.
	    case 'p':
		call printf ("Star: %d  Mag: %7.2f  Coords: %7.2f %7.2f\n")
		    call pargi (DP_CUR_PSFID(psf))
		    call pargr (DP_CUR_PSFMAG(psf))
		    call pargr (DP_CUR_PSFX(psf))
		    call pargr (DP_CUR_PSFY(psf))

	    # Print out the plot parameters.
	    case 's':
	        if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT) {
		    call printf ("Surface plot: angv = %6.1f angh = %6.1f ")
		        call pargr (DP_MANGV(psfpl))
		        call pargr (DP_MANGH(psfpl))
		    call printf ("  Minimum: %7.1f   Maximum: %7.1f\n")
		        call pargr (DP_PSFMIN(psf))
		        call pargr (DP_PSFMAX(psf))
		} else if (DP_PLOT_TYPE(psfpl) == DP_CONTOURPLOT) {
		    call printf ("Contour plot: floor = %6.1f ceil = %6.1f ")
		        call pargr (DP_CFLOOR(psfpl))
		        call pargr (DP_CCEIL(psfpl))
		    call printf ("   Minimum: %7.1f   Maximum: %7.1f\n")
		        call pargr (DP_PSFMIN(psf))
		        call pargr (DP_PSFMAX(psf))
		} else
		    call printf ("Unknown plot type.\n")

	    # Star rejected
	    case 'x':
		star_ok = false
		break

	    # Command mode.
	    case ':':
		for (ip=1;  IS_WHITE (Memc[cmd+ip-1]);  ip=ip+1)
		    ;

		switch (Memc[cmd+ip-1]) {

		# Set surface plot and angles
		case 'm':
		    DP_PLOT_TYPE(psfpl) = DP_MESHPLOT
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, DP_MANGV(psfpl)) <= 0)
			DP_MANGV(psfpl) = 30.
		    if (ctor (Memc[cmd], ip, DP_MANGH(psfpl)) <= 0)
			DP_MANGH(psfpl) = -30.
		    do_replot = true

		case 'c':
		    # Set surface contour and levels
		    DP_PLOT_TYPE(psfpl) = DP_CONTOURPLOT
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, DP_CFLOOR(psfpl)) <= 0)
			DP_CFLOOR(psfpl) = 0.0
		    if (ctor (Memc[cmd], ip, DP_CCEIL(psfpl)) <= 0)
			DP_CCEIL(psfpl) = 0.0
		    do_replot = true

		# Set vertical angle
		case 'v':
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, rval) <= 0) {
			call printf (
			    "Surface plot vertical viewing angle: %g\n")
			    call pargr (DP_MANGV(psfpl))
		    } else {
			DP_MANGV(psfpl) = rval
			if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT)
		            do_replot = true
		    }

		# Set horizontal angle
		case 'h':
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, rval) <= 0) {
			call printf (
			    "Surface plot horizontal viewing angle: %g\n")
			    call pargr (DP_MANGH(psfpl))
		    } else {
			DP_MANGH(psfpl) = rval
			if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT)
		            do_replot = true
		    }

		# Set the floor value.
		case 'l':
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, rval) <= 0) {
			call printf (
			    "Contour plot floor value: %g\n")
			    call pargr (DP_CFLOOR(psfpl))
		    } else {
			DP_CFLOOR(psfpl) = rval
			if (DP_PLOT_TYPE(psfpl) == DP_CONTOURPLOT)
		    	    do_replot = true
		    }

		# Set the ceiling value.
		case 'u':
		    ip = ip + 1
		    if (ctor (Memc[cmd], ip, rval) <= 0) {
			call printf (
			    "Contour plot ceiling value: %g\n")
			    call pargr (DP_CCEIL(psfpl))
		    } else {
			DP_CCEIL(psfpl) = rval
			if (DP_PLOT_TYPE(psfpl) == DP_CONTOURPLOT)
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
		if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT)
	    	    call dp_surfpsf (dao, subrast, ncols, nlines, Memc[title],
		        gd)
		else if (DP_PLOT_TYPE(psfpl) == DP_CONTOURPLOT)
	    	    call dp_contpsf (dao, subrast, ncols, nlines, Memc[title],
		        gd)
		do_replot = false
	    }
	}

	call gdeactivate (gd, 0)
	call sfree (sp)
end
