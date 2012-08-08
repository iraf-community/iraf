include	<imhdr.h>
include	<math/curfit.h>

# Interpolation mode
define	SI_LINEAR		1
define	SI_CURVES		2
define	SI_LEGENDRE		3
define	SI_CHEBYSHEV		4
define	SI_SPLINE3		5
define	SI_SPLINE1		6

# T_SINTERP -- Interpolate for values in a table and optionally generate
#		a spectral image
#
# A table of x,y pairs contained in a file is used to
# find interpolated values, y, for any other given independent
# variable, x. Extrapolation is performed if necessary.
#
# A series of values may be generated to generate a fine grid
# through a coarse sampling for purposes of plotting. This is
# done by setting the hidden parameter curve_gen to yes.
# The starting point, ending point, and sampling interval
# are also needed in this case (x1, x2, dx).
#
# If only a small number of values are needed to be interpolated
# from the table, the user may enter a number of x's from either
# a file or STDIN.

procedure t_sinterp()

real	x, y, x1, x2, dx
int	npts, i
int	filelist, tbl, in
int	user_mode, imlen, order, maxpts
char	fname[SZ_FNAME], tbl_file[SZ_FNAME]
char	image[SZ_FNAME]
char	interp[SZ_LINE]
bool	gen, make_image
pointer	im, pix, sp, xtab, ytab, cv

int	clpopni(), clgfil(), open(), fscan(), nscan()
int	clgeti(), clgwrd()
real	clgetr()
bool	clgetb()
pointer	immap(), impl1r()

begin
	# Initialize interpolator
	call intrp0 (1)
	cv = NULL

	# File containing x,y pairs in a table
	call clgstr ("tbl_file", tbl_file, SZ_FNAME)

	# Open table file and read as many points as possible
	tbl = open (tbl_file, READ_ONLY, TEXT_FILE)

	npts = 0
	maxpts = clgeti ("tbl_size")

	call smark (sp)
	call salloc (xtab, maxpts, TY_REAL)
	call salloc (ytab, maxpts, TY_REAL)

	while (fscan(tbl) != EOF) {
	    npts = npts + 1
	    if (npts > maxpts)
		call error (1, "Maximum table size exceeded.")
	    call gargr (Memr[xtab+npts-1])
	    call gargr (Memr[ytab+npts-1])
	    if (nscan() < 2) {
#		call eprintf ("Error reading x,y pairs\n")
		npts = npts - 1
	    }
	}

	call close (tbl)

	if (npts < 1)
	    call error (1, "Table has no entries.")

	# Linear, spline, or CURFIT option interpolator?
	user_mode = clgwrd ("interp_mode", interp, SZ_LINE,
	    ",linear,curves,legendre,chebyshev,spline3,spline1")

	if (user_mode > 2 && user_mode <= 6)
	    order = clgeti ("order")

	# Generate a curve?
	gen = clgetb ("curve_gen")

	# Or an image?
	make_image = clgetb ("make_image")

	if (gen || make_image) {
	    x1 = clgetr ("x1")
	    x2 = clgetr ("x2")
	    dx = clgetr ("dx")
	    imlen = clgeti ("npts")

	    # The above four variables overdefine the function
	    # One (other than x1) must be 0.0 --> solve for it
	    if (x2 == 0.0)
		x2 = x1 + (imlen-1) * dx
	    else if (dx == 0.0)
		dx = (x2 - x1) / (imlen - 1)

	    imlen = nint ((x2 - x1) / dx + 1)

	    # Verify that dx will not cause an infinite loop
	    if (dx == 0.0 || dx * (x2-x1) < 0.0)
		call error (1, "Interval paramater dx implies infinite loop.")

	    if (make_image) {
		call clgstr ("image", image, SZ_FNAME)
		im = immap (image, NEW_IMAGE, 0)

		IM_NDIM (im) = 1
		IM_LEN (im, 1) = imlen
		IM_PIXTYPE (im) = TY_REAL

		pix = impl1r (im)

		do i = 1, imlen {
		    x = x1 + (i - 1) * dx
		    call gen_pixel (Memr[xtab], Memr[ytab], npts, 
			user_mode, order, x, y, cv)
		    Memr[pix+i-1] = y
		}

		call imaddr (im, "CRVAL1", x1)
		call imaddr (im, "CDELT1", dx)
		call imaddr (im, "CD1_1", dx)
		call imaddr (im, "CRPIX1", 1.)
		call imaddi (im, "DC-FLAG", 0)

		call imunmap (im)
	    } else {
		do i = 1, imlen {
		    x = x1 + (i - 1) * dx
		    call gen_pixel (Memr[xtab], Memr[ytab], npts, 
			user_mode, order, x, y, cv)
		    call printf ("%12.5g  %12.5g\n")
		        call pargr (x)
		        call pargr (y)
		}
		call flush (STDOUT)
	    }

	# No, just one point at a time
	} else {

	    # Open input list
	    filelist = clpopni ("input")

	    while (clgfil (filelist, fname, SZ_FNAME) != EOF) {
		in = open (fname, READ_ONLY, TEXT_FILE)

		# Process input requests
		while (fscan(in) != EOF) {
		    call gargr (x)

		    call gen_pixel (Memr[xtab], Memr[ytab], npts, 
			user_mode, order, x, y, cv)
		    call printf ("%12.5g  %12.5g\n")
			call pargr (x)
			call pargr (y)
		    call flush (STDOUT)
		}

		call close (in)
	    }

	    call clpcls (filelist)
	}

	call cvfree (cv)
	call sfree (sp)
end

# GEN_PIXEL -- Generate a pixel value using specified interpolator

procedure gen_pixel (xtab, ytab, npts, mode, order, x, y, cv)

real	xtab[ARB], ytab[ARB]
int	npts
real	x
int	mode, order
real	y
pointer	cv

int	fit, ier
pointer	wt, sp

real	cveval()

begin
	# Interpolate after selecting option
	switch (mode) {
	    case SI_LINEAR:
		call lintrp (1, xtab, ytab, npts, x, y, ier)

	    case SI_CURVES:
		call intrp  (1, xtab, ytab, npts, x, y, ier)

	    default:
		if (cv == NULL) {
		    call smark (sp)
		    call salloc (wt, npts, TY_REAL)
		    call amovkr (1.0, Memr[wt], npts)

		    switch (mode) {
	    		case SI_LEGENDRE:
			    fit = LEGENDRE
	    		case SI_CHEBYSHEV:
			    fit = CHEBYSHEV
	    		case SI_SPLINE3:
			    fit = SPLINE3
	    		case SI_SPLINE1:
			    fit = SPLINE1
			default:
			    fit = SPLINE1
		    }

		    call cvinit (cv, fit, order, xtab[1], xtab[npts])
		    call cvfit (cv, xtab, ytab, Memr[wt], npts, WTS_UNIFORM,
			ier)
		    call sfree (sp)
		}
		y = cveval (cv, x)
	}
end
