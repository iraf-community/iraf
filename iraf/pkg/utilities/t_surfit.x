include	<imhdr.h>
include	<error.h>
include	<math/gsurfit.h>


# T_SURFIT -- Fit a surface to a set of x, y, and z points from an input
# text file.  Output the surface parameters, coefficients, errors, data
# points with fit and residuals, and chi square to the standard output.
# Optionally evaluate the surface for a set of x and y from a text file and
# write x, y, and z to an output text file.  Optionally evaluate the surface
# over the fit limits and create an image with appropriate WCS.

procedure t_surfit ()

pointer	input			# Input file
pointer	func			# Function type
pointer	wttype			# Weight type
pointer	image			# Surface image
pointer	coords			# Coordinates to evaluate
pointer	fit			# Fit output file
int	xorder			# X order
int	yorder			# Y order
int	xterms			# Cross-terms?
double	xmin, xmax		# Surface range
double	ymin, ymax		# Surface range
double	zmin, zmax		# Data limits
int	ncols			# Number of image columns
int	nlines			# Number of image lines

int	i, j, k, fd, n, ncoeff, maxorder, xincr
double	r[8], dx, dy, chisqr
pointer	sf, im, mw
pointer sp, x, y, z, w, c, e, f, xvec, yvec, ptr, xtype

int	clgeti(), clgwrd()
int	open(), fscan(), nscan(), nowhite(), dgsgeti()
double	clgetd(), dgseval()
pointer	immap(), impl2d(), mw_open()
errchk	open, malloc, realloc, immap, impl2d, mw_open
errchk	dgsinit, dgsfit, dgscoeff, dgserrors

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (func, SZ_FNAME, TY_CHAR)
	call salloc (wttype, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (fit, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (xtype, SZ_FNAME, TY_CHAR)

	x = NULL
	y = NULL
	z = NULL
	w = NULL
	fd = NULL
	sf = NULL
	im = NULL
	mw = NULL

	iferr {
	    # Read points to be fit.
	    call clgstr ("input", Memc[input], SZ_FNAME)
	    ptr = open (Memc[input], READ_ONLY, TEXT_FILE); fd = ptr

	    n = 0
	    while (fscan (fd) != EOF) {
		call gargd (r[1])
		call gargd (r[2])
		call gargd (r[3])
		call gargd (r[4])
		if (nscan() < 3)
		    next
		if (nscan() < 4)
		    r[4] = 1.
		if (n == 0) {
		    call malloc (x, 100, TY_DOUBLE)
		    call malloc (y, 100, TY_DOUBLE)
		    call malloc (z, 100, TY_DOUBLE)
		    call malloc (w, 100, TY_DOUBLE)
		} else if (mod (n, 100) == 0) {
		    call realloc (x, n+100, TY_DOUBLE)
		    call realloc (y, n+100, TY_DOUBLE)
		    call realloc (z, n+100, TY_DOUBLE)
		    call realloc (w, n+100, TY_DOUBLE)
		}
		Memd[x+n] = r[1]
		Memd[y+n] = r[2]
		Memd[z+n] = r[3]
		Memd[w+n] = r[4]
		n = n + 1
	    }
	    call close (fd)
	    if (n == 0)
		call error (1, "No points")

	    # Set x, y, z limits and reject data outside range.
	    xmin = clgetd ("xmin")
	    xmax = clgetd ("xmax")
	    ymin = clgetd ("ymin")
	    ymax = clgetd ("ymax")
	    zmin = clgetd ("zmin")
	    zmax = clgetd ("zmax")
	    call alimd (Memd[x], n, dx, dy)
	    if (IS_INDEFD(xmin))
		xmin = dx
	    if (IS_INDEFD(xmax))
		xmax = dy
	    call alimd (Memd[y], n, dx, dy)
	    if (IS_INDEFD(ymin))
		ymin = dx
	    if (IS_INDEFD(ymax))
		ymax = dy
	    call alimd (Memd[z], n, dx, dy)
	    if (IS_INDEFD(zmin))
		zmin = dx
	    if (IS_INDEFD(zmax))
		zmax = dy
	    j = 0
	    do i = 0, n-1 {
		if (Memd[x+i] < xmin || Memd[x+i] > xmax)
		    next
		if (Memd[y+i] < ymin || Memd[y+i] > ymax)
		    next
		if (Memd[z+i] < zmin || Memd[z+i] > zmax)
		    next
		Memd[x+j] = Memd[x+i]
		Memd[y+j] = Memd[y+i]
		Memd[z+j] = Memd[z+i]
		Memd[w+j] = Memd[w+i]
		j = j + 1
	    }
	    n = j
	    if (n == 0)
		call error (2, "No data values")

	    # Fit surface.
	    i = clgwrd ("function", Memc[func], SZ_FNAME, GS_FUNCTIONS)
	    xorder = clgeti ("xorder")
	    yorder = clgeti ("yorder")
	    xterms = clgwrd ("xterms", Memc[xtype], SZ_FNAME, GS_XTYPES) - 1

	    # Set the weights.
	    j = clgwrd ("weighting", Memc[wttype], SZ_FNAME,
		"|uniform|user|statistical|instrumental|")
	    switch (j) {
	    case 1:
		do k = 0, n-1
		    Memd[w+k] = 1
	    case 2:
		;
	    case 3:
		do k = 0, n-1
		    Memd[w+k] = 1 / max (1.0d-20, abs (Memd[z+k]))
	    case 4:
		do k = 0, n-1
		    Memd[w+k] = 1 / max (1.0d-20, Memd[z+k]**2)
	    }

	    call dgsinit (sf, i, xorder, yorder, xterms, xmin, xmax, ymin, ymax)
	    call dgsfit (sf, Memd[x], Memd[y], Memd[z], Memd[w], n, WTS_USER, i)
	    if (i != OK)
		call error (2, "Fitting error")

	    # Output parameters, coefficients, errors, and fit results.
	    ncoeff = dgsgeti (sf, GSNCOEFF)
	    call salloc (c, ncoeff, TY_DOUBLE)
	    call salloc (e, ncoeff, TY_DOUBLE)
	    call salloc (f, n, TY_DOUBLE)
	    call dgscoeff (sf, Memd[c], ncoeff)
	    call dgsvector (sf, Memd[x], Memd[y], Memd[f], n)
	    call dgserrors (sf, Memd[z], Memd[w], Memd[f], chisqr, Memd[e])

	    call printf ("Surface parameters:\n")
	    call printf ("  function = %s\n")
		call pargstr (Memc[func])
	    call printf ("  xorder = %d\n  yorder = %d\n  xterms = %s\n")
		call pargi (xorder)
		call pargi (yorder)
		call pargstr (Memc[xtype])
	    call printf ("  weighting = %s\n")
		call pargstr (Memc[wttype])
	    call printf ("  xmin = %8.6g\n  xmax = %8.6g\n")
		call pargd (xmin)
		call pargd (xmax)
	    call printf ("  ymin = %8.6g\n  ymax = %8.6g\n")
		call pargd (ymin)
		call pargd (ymax)
	    call printf ("  zmin = %8.6g\n  zmax = %8.6g\n")
		call pargd (zmin)
		call pargd (zmax)

	    call printf ("\nSurface coefficients:\n")
	    call printf ("   x  y    coeff    error\n")
	    i = 0
	    if (xterms == GS_XFULL) {
		do k = 1, yorder {
		    do j = 1, xorder {
			call printf ("  %2d %2d %8.6g %8.6g\n")
			    call pargi (j-1)
			    call pargi (k-1)
			    call pargd (Memd[c+i])
			    call pargd (Memd[e+i])
			i = i + 1
		    }
		}
	    } else if (xterms == GS_XHALF) {
		maxorder = max (xorder+1, yorder+1)
		xincr = xorder
		do k = 1, yorder {
		    do j = 1, xincr {
			call printf ("  %2d %2d %8.6g %8.6g\n")
			    call pargi (j-1)
			    call pargi (k-1)
			    call pargd (Memd[c+i])
			    call pargd (Memd[e+i])
			i = i + 1
		    }
		    if ((k + xorder + 1) > maxorder)
			xincr = xincr - 1
		}
	    } else {
		do j = 1, xorder {
		    call printf ("  %2d %2d %8.6g %8.6g\n")
			call pargi (j-1)
			call pargi (0)
			call pargd (Memd[c+i])
			call pargd (Memd[e+i])
		    i = i + 1
		}
		do k = 2, yorder {
		    call printf ("  %2d %2d %8.6g %8.6g\n")
			call pargi (0)
			call pargi (k-1)
			call pargd (Memd[c+i])
			call pargd (Memd[e+i])
		    i = i + 1
		}
	    }

	    call printf ("\nFitted points:\n")
	    call printf ("  %8s %8s %8s %8s %8s %8s\n")
		call pargstr ("x")
		call pargstr ("y")
		call pargstr ("z")
		call pargstr ("fit")
		call pargstr ("residual")
		call pargstr ("weight")
	    do i = 0, n-1 {
		call printf ("  %8.6g %8.6g %8.6g %8.6g %8.6g %8.6g\n")
		    call pargd (Memd[x+i])
		    call pargd (Memd[y+i])
		    call pargd (Memd[z+i])
		    call pargd (Memd[f+i])
		    call pargd (Memd[z+i] - Memd[f+i])
		    call pargd (Memd[w+i])
	    }
	    call printf ("\n  chisqr = %8.6g\n")
		call pargd (chisqr)

	    # Evaluate surface if desired.
	    call clgstr ("coordinates", Memc[coords], SZ_FNAME)
	    if (nowhite (Memc[coords], Memc[coords], SZ_FNAME) != 0) {
		ptr = open (Memc[coords], READ_ONLY, TEXT_FILE); fd = ptr

		call clgstr ("fit", Memc[fit], SZ_FNAME)
		if (nowhite (Memc[fit], Memc[fit], SZ_FNAME) != 0) {
		    i = open (Memc[fit], APPEND, TEXT_FILE)
		    while (fscan (fd) != EOF) {
			call gargd (r[1])
			call gargd (r[2])
			if (nscan() < 2)
			    next
			if (r[1]<xmin || r[1]>xmax || r[2]<ymin || r[2]>ymax)
			    next
			r[3] = dgseval (sf, r[1], r[2])
			call fprintf (i, "%8.6g %8.6g %8.6g\n")
			    call pargd (r[1])
			    call pargd (r[2])
			    call pargd (r[3])
		    }
		    call close (i)
		}
		call close (fd)
	    }

	    # Create an image if desired.
	    call clgstr ("image", Memc[image], SZ_FNAME)
	    if (nowhite (Memc[image], Memc[image], SZ_FNAME) != 0) {
		ncols = clgeti ("ncols")
		nlines = clgeti ("nlines")

		ptr = immap (Memc[image], NEW_IMAGE, 0); im = ptr
		IM_PIXTYPE(im) = TY_REAL
		IM_LEN(im,1) = ncols
		IM_LEN(im,2) = nlines

		call salloc (xvec, ncols, TY_DOUBLE)
		call salloc (yvec, ncols, TY_DOUBLE)
		dx = (xmax - xmin) / (ncols - 1)
		dy = (ymax - ymin) / (nlines - 1)
		do i = 1, ncols
		    Memd[xvec+i-1] = xmin + dx * (i - 1)
		do i = 1, nlines {
		    Memd[yvec] = ymin + dy * (i - 1)
		    call amovkd (Memd[yvec], Memd[yvec], ncols)
		    call dgsvector (sf, Memd[xvec], Memd[yvec],
			Memd[impl2d(im,i)], ncols)
		}

		r[1] = 1.
		r[2] = 1.
		r[3] = xmin
		r[4] = ymin
		r[5] = dx
		r[6] = 0.
		r[7] = 0.
		r[8] = dy
		mw = mw_open (NULL, 2)
		call mw_newsystem (mw, "world", 2)
		call mw_swtermd (mw, r[1], r[3], r[5], 2)
		call mw_saveim (mw, im)

		call mw_close (mw)
		call imunmap (im)
	    }

	    call dgsfree (sf)
	} then {
	    if (fd != NULL)
		call close (fd)
	    if (mw != NULL)
		call mw_close (mw)
	    if (im != NULL)
		call imunmap (im)
	    if (sf != NULL)
		call dgsfree (sf)
	    call erract (EA_WARN)
	}

	call sfree (sp)
end
