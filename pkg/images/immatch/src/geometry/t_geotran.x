# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <mwset.h>
include <math.h>
include <math/gsurfit.h>
include "geotran.h"

# T_GEOTRAN -- Geometrically transform a list of images either linearly or
# using a transformation computed by the GEOMAP task.

procedure t_geotran ()

int	ncols, nlines			# output picture size
real	xmin, xmax, ymin, ymax		# minimum and maximum ref values
real	xscale, yscale			# output picture scale
real	xin, yin			# input picture origin
real	xshift, yshift			# x and y shifts
real	xout, yout			# output picture origin
real	xmag, ymag			# input picture scale
real	xrotation, yrotation		# rotation angle
int	nxblock, nyblock		# block size of image to be used

bool	verbose
int	list1, list2, tflist, ndim, nc, nl, mode
pointer	sp, imtlist1, imtlist2, database, transform, record
pointer	image1, image2, imtemp, imroot, section, str
pointer	geo, sx1, sy1, sx2, sy2, in, out, mw
real	xs, ys, txshift, tyshift, txmag, tymag, txrot, tyrot
double	oltv[2], nltv[2], oltm[2,2], nltm[2,2]

bool	clgetb(), envgetb(), streq()
int	imtopen(), imtlen(), clgeti(), imtgetim(), clgwrd(), btoi()
pointer	immap(), mw_openim()
real	clgetr()
errchk	immap()

begin
	# Set up  the geotran structure.
	call smark (sp)
	call salloc (imtlist1, SZ_LINE, TY_CHAR)
	call salloc (imtlist2, SZ_LINE, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (transform, SZ_FNAME, TY_CHAR)
	call salloc (record, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (imroot, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (geo, LEN_GEOSTRUCT, TY_STRUCT)

	# Get the input and output lists and database file.
	call clgstr ("input", Memc[imtlist1], SZ_FNAME)
	call clgstr ("output", Memc[imtlist2], SZ_FNAME)
	call clgstr ("database", Memc[database], SZ_FNAME)
	if (Memc[database] != EOS) {
	    call clgstr ("transforms", Memc[transform], SZ_FNAME)
	    tflist =  imtopen (Memc[transform])
	    GT_GEOMODE(geo) = clgwrd ("geometry", Memc[str], SZ_LINE, 
	        ",junk,linear,distortion,geometric,")
	} else {
	    tflist = NULL
	    GT_GEOMODE(geo) = GT_NONE
	}

	# Get the output picture format parameters.
	xmin = clgetr ("xmin")
	xmax = clgetr ("xmax")
	ymin = clgetr ("ymin")
	ymax = clgetr ("ymax")
	xscale = clgetr ("xscale")
	yscale = clgetr ("yscale")
	ncols= clgeti ("ncols")
	nlines = clgeti ("nlines")

	# Get the geometric transformation parameters.
	xin = clgetr ("xin")
	yin = clgetr ("yin")
	xshift = clgetr ("xshift")
	yshift = clgetr ("yshift")
	xout = clgetr ("xout")
	yout = clgetr ("yout")
	xmag = clgetr ("xmag")
	ymag = clgetr ("ymag")
	xrotation = clgetr ("xrotation")
	yrotation = clgetr ("yrotation")

	# Get the interpolation parameters.
	call clgstr ("interpolant", GT_INTERPSTR(geo), SZ_FNAME)
	#GT_INTERPOLANT(geo) = clgwrd ("interpolant", Memc[str], SZ_LINE, 
	    #",nearest,linear,poly3,poly5,spline3,")
	GT_BOUNDARY(geo) = clgwrd ("boundary", Memc[str], SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	GT_CONSTANT(geo) = clgetr ("constant")
	GT_XSAMPLE(geo) = clgetr ("xsample")
	GT_YSAMPLE(geo) = clgetr ("ysample")
	GT_FLUXCONSERVE(geo) = btoi (clgetb("fluxconserve"))

	nxblock = clgeti ("nxblock")
	nyblock = clgeti ("nyblock")
	verbose = clgetb ("verbose")

	# Open the lists of images and check the scale lengths.
	list1 = imtopen (Memc[imtlist1])
	list2 = imtopen (Memc[imtlist2])
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    if (tflist != NULL)
	        call imtclose (tflist)
	    call error (0, "Input and output lists not the same length.")
	}

	# Check the transform list.
	if (tflist != NULL) {
	    if (imtlen (tflist) > 1 && imtlen (tflist) != imtlen (list1)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call imtclose (tflist)
	        call error (0, "Transform and input lists not the same length.")
	    }
	}

	# Loop over the images.
	if (verbose) {
	    call printf ("\n")
	}
	while (imtgetim (list1, Memc[image1], SZ_FNAME) != EOF &&
	    imtgetim (list2, Memc[image2], SZ_FNAME) != EOF) {

	    # Print messages.
	    if (verbose) {
		call printf ("Transforming image %s to image %s\n")
		    call pargstr (Memc[image1])
		    call pargstr (Memc[image2])
		call flush (STDOUT)
	    }

	    # Open the images.
	    in = immap (Memc[image1], READ_ONLY, 0)
	    call imgimage (Memc[image1], Memc[str], SZ_FNAME)
	    call imgimage (Memc[image2], Memc[imroot], SZ_FNAME)
	    call imgsection (Memc[image2], Memc[section], SZ_FNAME)
	    if (streq (Memc[str], Memc[imroot])) {
		call strcpy (Memc[imroot], Memc[imtemp], SZ_FNAME)
		call mktemp ("tmp", Memc[image2], SZ_FNAME)
	    } else
		call strcpy (Memc[image2], Memc[imtemp], SZ_FNAME)
	    ifnoerr (out = immap (Memc[image2], READ_WRITE, 0)) {
		mode = READ_WRITE
		nc = IM_LEN(out,1)
		nl = IM_LEN(out,2)
		xs = INDEF
		ys = INDEF
	    } else if (Memc[section] != EOS) {
		mode = NEW_IMAGE
	        out = immap (Memc[imroot], NEW_IMAGE, 0)
		IM_NDIM(out) = IM_NDIM(in)
		if (IS_INDEFI(ncols))
		    IM_LEN(out,1) = IM_LEN(in,1)
		else
		    IM_LEN(out,1) = ncols
		if (IS_INDEFI(nlines))
		    IM_LEN(out,2) = IM_LEN(in,2)
		else
		    IM_LEN(out,2) = nlines
		IM_PIXTYPE(out) = IM_PIXTYPE(in)
		call geo_imzero (out, GT_CONSTANT(geo))
		call imunmap (out)
		out = immap (Memc[image2], READ_WRITE, 0)
		nc = IM_LEN(out,1)
		nl = IM_LEN(out,2)
		xs = INDEF
		ys = INDEF
	    } else {
		mode = NEW_COPY
	        out = immap (Memc[image2], NEW_COPY, in)
		nc = ncols
		nl = nlines
		xs = xscale
		ys = yscale
	    }

	    # Set the geometry parameters.
	    call geo_set (geo, xmin, xmax, ymin, ymax, xs, ys, nc, nl, xin,
	        yin, xshift, yshift, xout, yout, xmag, ymag, xrotation,
		yrotation)

	    # Get the coordinate surfaces.
	    if (GT_GEOMODE(geo) == GT_NONE) {
		call geo_format (in, out, geo, sx1, sy1, sx2, sy2)
		if (verbose) {
		    call geo_lcoeffr (sx1, sy1, txshift, tyshift, txmag,
			tymag, txrot, tyrot)
		    call printf ("    xshift: %.2f yshift: %.2f ")
                        call pargr (txshift)
                        call pargr (tyshift)
                    call printf ("xmag: %.2f ymag: %.2f ")
                        call pargr (txmag)
                        call pargr (tymag)
                    call printf ("xrot: %.2f yrot: %.2f\n")
                        call pargr (txrot)
                        call pargr (tyrot)
		    call flush (STDOUT)
		}
	    } else { 
		if (imtgetim (tflist, Memc[str], SZ_FNAME) != EOF)
		    call strcpy (Memc[str], Memc[record], SZ_FNAME)
	        call geo_dformat (in, out, geo, Memc[database], Memc[record],
		    sx1, sy1, sx2, sy2)
		if (verbose) {
		    call printf ("    Using transform %s in database %s\n")
			call pargstr (Memc[record])
			call pargstr (Memc[database])
		    call flush (STDOUT)
		}
	    }

	    # Transform the image.
	    if (IM_LEN(out,1) <= nxblock && IM_LEN(out,2) <= nyblock) {
	        if (GT_XSAMPLE(geo) > 1.0 || GT_YSAMPLE(geo) > 1.0)
		    call geo_simtran (in, out, geo, sx1, sy1, sx2, sy2)
	        else
	            call geo_imtran (in, out, geo, sx1, sy1, sx2, sy2)
	    } else {
	        if (GT_XSAMPLE(geo) > 1.0 || GT_YSAMPLE(geo) > 1.0) {
		    if (IM_NDIM(out) == 1)
		        call geo_stran (in, out, geo, sx1, sy1, sx2, sy2,
			    nxblock, 1)
		    else
		        call geo_stran (in, out, geo, sx1, sy1, sx2, sy2,
			    nxblock, nyblock)
	        } else {
		    if (IM_NDIM(out) == 1)
	                call geo_tran (in, out, geo, sx1, sy1, sx2, sy2,
			    nxblock, 1)
		    else
	                call geo_tran (in, out, geo, sx1, sy1, sx2, sy2,
			    nxblock, nyblock)
		}
	    }

	    # Update the linear part of the wcs.
	    if (!envgetb ("nomwcs") && mode == NEW_COPY) {
		ndim = IM_NDIM(in)
		mw = mw_openim (in)
		call geo_gwcs (geo, sx1, sy1, oltm, oltv)
		call mw_invertd (oltm, nltm, ndim)
		call mw_vmuld (nltm, oltv, nltv, ndim)
		call anegd (nltv, nltv, ndim)
		call geo_swcs (mw, nltm, nltv, ndim) 
		call mw_saveim (mw, out)
		call mw_close (mw)
	    }

	    # Free the surfaces.
	    call gsfree (sx1)
	    call gsfree (sy1)
	    call gsfree (sx2)
	    call gsfree (sy2)

	    # Close the images.
	    call imunmap (in)
	    call imunmap (out)

	    call xt_delimtemp (Memc[image2], Memc[imtemp])
	}

	# Clean up.
	call sfree (sp)
	if (tflist != NULL)
	    call imtclose (tflist)
	call imtclose (list1)
	call imtclose (list2)
end


# GEO_IMZERO -- Create a dummy output image filled with the constant boundary
# extension value.

procedure geo_imzero (im, constant)

pointer	im			#I pointer to the input image
real	constant		#I the constant value to insert in the imagw

int	npix
pointer	sp, v, buf
int	impnls(), impnll(), impnlr(), impnld(), impnlx()

begin
        # Setup start vector for sequential reads and writes.
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)
        call amovkl (long(1), Meml[v], IM_MAXDIM)

        # Initialize the image.
        npix = IM_LEN(im, 1)
        switch (IM_PIXTYPE(im)) {
        case TY_SHORT:
            while (impnls (im, buf, Meml[v]) != EOF)
                call amovks (short (constant), Mems[buf], npix)
        case TY_USHORT, TY_INT, TY_LONG:
            while (impnll (im, buf, Meml[v]) != EOF)
                call amovkl (long (constant), Meml[buf], npix)
        case TY_REAL:
            while (impnlr (im, buf, Meml[v]) != EOF)
                call amovkr (constant, Memr[buf], npix)
        case TY_DOUBLE:
            while (impnld (im, buf, Meml[v]) != EOF)
                call amovkd (double (constant), Memd[buf], npix)
        case TY_COMPLEX:
            while (impnlx (im, buf, Meml[v]) != EOF)
                call amovkx (complex (constant, 0.0), Memx[buf], npix)
        default:
            call error (1, "Unknown pixel datatype")
        }

	call sfree (sp)
end


# GEO_SET -- Set the image dependent task parameters individually for each
# image.

procedure geo_set (geo, xmin, xmax, ymin, ymax, xscale, yscale, ncols, nlines,
        xin, yin, xshift, yshift, xout, yout, xmag, ymag, xrotation, yrotation)

pointer	geo			#I pointer to geotran structure
real	xmin, xmax		#I minimum and maximum reference values
real	ymin, ymax		#I minimum and maximum reference values
real	xscale, yscale		#I output picture scale
int	ncols, nlines		#I output picture size
real	xin, yin		#I input picture pixel coordinates
real	xshift, yshift		#I shift of origin
real	xout, yout		#I corresponding output picture coords
real	xmag, ymag		#I input picture scale
real	xrotation, yrotation	#I scale angle

begin
	# Set the output picture format parameters.
	GT_XMIN(geo) = xmin
	GT_XMAX(geo) = xmax
	GT_YMIN(geo) = ymin
	GT_YMAX(geo) = ymax
	GT_XSCALE(geo) = xscale
	GT_YSCALE(geo) = yscale
	GT_NCOLS(geo) = ncols
	GT_NLINES(geo) = nlines

	# Set the transformation parameters.
	GT_XIN(geo) = xin
	GT_YIN(geo) = yin
	GT_XSHIFT(geo) = xshift
	GT_YSHIFT(geo) = yshift
	GT_XOUT(geo) = xout
	GT_YOUT(geo) = yout
	GT_XMAG(geo) = xmag
	GT_YMAG(geo) = ymag
	GT_XROTATION(geo) = xrotation
	GT_YROTATION(geo) = yrotation
end


# GEO_FORMAT -- Format the output picture when there is no database file.

procedure geo_format (in, out, geo, sx1, sy1, sx2, sy2)

pointer	in		#I pointer to the input image
pointer	out		#I pointer to the ouput image
pointer	geo		#I pointer to the geotran structure
pointer sx1, sy1	#O pointer to linear surfaces
pointer	sx2, sy2	#O pointer to distortion surfaces

real	xmax, ymax

begin
	# Get the scale transformation parameters.
	if (IS_INDEFR(GT_XMAG(geo)))
	    GT_XMAG(geo) = 1.
	if (IM_NDIM(in) == 1)
	    GT_YMAG(geo) = 1.
	else if (IS_INDEFR(GT_YMAG(geo)))
	    GT_YMAG(geo) = 1.

	# Get the rotate transformation parameters.
	if (IM_NDIM(in) == 1)
	    GT_XROTATION(geo) =  DEGTORAD(0.)
	else if (IS_INDEFR(GT_XROTATION(geo)))
	    GT_XROTATION(geo) =  DEGTORAD(0.)
	else
	    GT_XROTATION(geo) =  DEGTORAD(GT_XROTATION(geo))
	if (IM_NDIM(in) == 1)
	    GT_YROTATION(geo) =  DEGTORAD(0.)
	else if (IS_INDEFR(GT_YROTATION(geo)))
	    GT_YROTATION(geo) =  DEGTORAD(0.)
	else
	    GT_YROTATION(geo) =  DEGTORAD(GT_YROTATION(geo))

	# Automatically compute the maximum extent of the image.
	if (GT_XMAX(geo) <= 0.0 || GT_YMAX(geo) <= 0.0) {

	    # Compute the size of the output image.
	    xmax = abs (cos(GT_XROTATION(geo)) * IM_LEN(in,1) /
	        GT_XMAG(geo)) + abs(sin(GT_YROTATION(geo)) * IM_LEN(in,2) /
		GT_YMAG(geo))
	    ymax = abs (sin(GT_XROTATION(geo)) * IM_LEN(in, 1) /
	        GT_XMAG(geo)) + abs (cos(GT_YROTATION(geo)) * IM_LEN(in,2) /
		GT_YMAG(geo))
	}

	# Set up the x reference coordinate limits.
	if (IS_INDEF(GT_XMIN(geo)))
	    GT_XMIN(geo) = 1.
	else
	    GT_XMIN(geo) = max (1.0, GT_XMIN(geo))
	if (IS_INDEF(GT_XMAX(geo)))
	    GT_XMAX(geo) = IM_LEN(in,1)
	else if (GT_XMAX(geo) <= 0.0)
	    #GT_XMAX(geo) = int (xmax + 1.0)
	    GT_XMAX(geo) = xmax

	# Set up the y reference coordinate limits.
	if (IS_INDEF(GT_YMIN(geo)))
	    GT_YMIN(geo) = 1.
	else
	    GT_YMIN(geo) = max (1.0, GT_YMIN(geo))
	if (IS_INDEF(GT_YMAX(geo)))
	    GT_YMAX(geo) = IM_LEN(in, 2)
	else if (GT_YMAX(geo) <= 0.0)
	    #GT_YMAX(geo) = int (ymax + 1.0) 
	    GT_YMAX(geo) = ymax

	# Set the number of columns and rows.
	if (IS_INDEFI(GT_NCOLS(geo)))
	    GT_NCOLS(geo) = IM_LEN(in, 1)
	if (IM_NDIM(in) == 1)
	    GT_NLINES(geo) = 1
	else if (IS_INDEFI(GT_NLINES(geo)))
	    GT_NLINES(geo) = IM_LEN(in, 2)

	# Set scale, overiding number of columns and rows if necessary. 
	if (IS_INDEFR(GT_XSCALE(geo)))
	    GT_XSCALE(geo) = (GT_XMAX(geo) - GT_XMIN(geo)) / (GT_NCOLS(geo) - 1)
	else
	    GT_NCOLS(geo) = (GT_XMAX(geo) - GT_XMIN(geo)) / GT_XSCALE(geo) + 1
	if (IM_NDIM(in) == 1)
	    GT_YSCALE(geo) = 1.0
	else if (IS_INDEFR(GT_YSCALE(geo)))
	    GT_YSCALE(geo) = (GT_YMAX(geo) - GT_YMIN(geo)) /
	        (GT_NLINES(geo) - 1)
	else
	    GT_NLINES(geo) = (GT_YMAX(geo) - GT_YMIN(geo)) / GT_YSCALE(geo) + 1
	IM_LEN(out, 1) = GT_NCOLS(geo)
	IM_LEN(out, 2) = GT_NLINES(geo)

	# Set up the surfaces, distortion surfaces are NULL.
	if (IM_NDIM(in) == 1) {
	    call gsinit (sx1, GS_POLYNOMIAL, 2, 2, GS_XNONE, GT_XMIN(geo),
	        GT_XMAX(geo), 0.5, 1.5)
	    call gsinit (sy1, GS_POLYNOMIAL, 2, 2, GS_XNONE, GT_XMIN(geo),
	        GT_XMAX(geo), 0.5, 1.5)
	} else {
	    call gsinit (sx1, GS_POLYNOMIAL, 2, 2, GS_XNONE, GT_XMIN(geo),
	        GT_XMAX(geo), GT_YMIN(geo), GT_YMAX(geo))
	    call gsinit (sy1, GS_POLYNOMIAL, 2, 2, GS_XNONE, GT_XMIN(geo),
	        GT_XMAX(geo), GT_YMIN(geo), GT_YMAX(geo))
	}
	sx2 = NULL
	sy2 = NULL

	# Adjust rotation, x and y scale, scale angle, and flip.
	call geo_rotmagr (sx1, sy1, GT_XMAG(geo), GT_YMAG(geo),
	    GT_XROTATION(geo), GT_YROTATION(geo))

	# Adjust the shift.
	call geo_shift (in, out, geo, sx1, sy1)
end


# GEO_DFORMAT -- Get the coordinate transformation from a database file.

procedure geo_dformat (in, out, geo, database, transform, sx1, sy1, sx2, sy2)

pointer	in, out			#I pointers to input and output images
pointer	geo			#I pointer to geotran structure
char	database[ARB]		#I name of database file
char	transform[ARB]		#I name of transform
pointer	sx1, sy1		#O pointer to linear part of surface fit
pointer	sx2, sy2		#O pointer to higher order surface

int	i, dt, rec, ncoeff, junk
pointer	xcoeff, ycoeff, newsx1, newsy1
int	dtmap(), dtlocate(), dtgeti(), dtscan()
errchk	gsrestore

begin
	# Map the database and locate the transformation record.
	dt = dtmap (database, READ_ONLY)
	rec = dtlocate (dt, transform)

	# Get the linear part of the fit.
	ncoeff = dtgeti (dt, rec, "surface1")
	call malloc (xcoeff, ncoeff, TY_REAL)
	call malloc (ycoeff, ncoeff, TY_REAL)
	do i = 1, ncoeff {
	    junk = dtscan (dt)
	    call gargr (Memr[xcoeff+i-1])
	    call gargr (Memr[ycoeff+i-1])
	}
	call gsrestore (sx1, Memr[xcoeff])
	call gsrestore (sy1, Memr[ycoeff])

	# Set the output image format parameters.
	call geo_dout (in, out, geo, sx1, sy1)

	# Adjust the linear part of the fit.
	call gscopy (sx1, newsx1)
	call gscopy (sy1, newsy1)
	if (GT_GEOMODE(geo) == GT_DISTORT)
	    call geo_rotmagr (newsx1, newsy1, 1.0, 1.0, 0.0, 0.0)
	else if (! IS_INDEFR(GT_XMAG(geo)) || ! IS_INDEFR(GT_YMAG(geo)) ||
	    ! IS_INDEFR(GT_XROTATION(geo)) || ! IS_INDEFR(GT_YROTATION(geo)))
	    call geo_dcoeff (geo, dt, rec, newsx1, newsy1)
	call geo_dshift (in, out, dt, rec, geo, newsx1, newsy1)

	# Get the higher order part of the fit.
	ncoeff = dtgeti (dt, rec, "surface2")
	if (ncoeff > 0 && (GT_GEOMODE(geo) == GT_GEOMETRIC || GT_GEOMODE(geo) ==
	    GT_DISTORT)) {

	    # Get the distortion coefficients.
	    call realloc (xcoeff, ncoeff, TY_REAL)
	    call realloc (ycoeff, ncoeff, TY_REAL)
	    do i = 1, ncoeff {
	        junk = dtscan(dt)
	        call gargr (Memr[xcoeff+i-1])
	        call gargr (Memr[ycoeff+i-1])
	    }
	    iferr {
	        call gsrestore (sx2, Memr[xcoeff])
	    } then {
		call mfree (sx2, TY_STRUCT)
		sx2 = NULL
	    }
	    iferr {
	        call gsrestore (sy2, Memr[ycoeff])
	    } then {
		call mfree (sy2, TY_STRUCT)
		sy2 = NULL
	    }

	} else {

	    sx2 = NULL
	    sy2 = NULL
	}

	# Redefine the surfaces.
	call gsfree (sx1)
	call gscopy (newsx1, sx1)
	call gsfree (newsx1)
	call gsfree (sy1)
	call gscopy (newsy1, sy1)
	call gsfree (newsy1)

	# Cleanup.
	call mfree (xcoeff, TY_REAL)
	call mfree (ycoeff, TY_REAL)
	call dtunmap (dt)
end


# GEO_DOUT --  Set the output image format using information in the database
# file.

procedure geo_dout (in, out, geo, sx1, sy1)

pointer	in, out		#I pointers to input and output image
pointer	geo		#I pointer to geotran sturcture
pointer	sx1, sy1	#I pointers to linear surface descriptors

real	gsgetr ()

begin
	# Set the reference coordinate limits.
	if (IS_INDEFR(GT_XMIN(geo)))
	    GT_XMIN(geo) = gsgetr (sx1, GSXMIN)
	if (IS_INDEFR(GT_XMAX(geo)))
	    GT_XMAX(geo) = gsgetr (sx1, GSXMAX)
	if (IS_INDEFR(GT_YMIN(geo)))
	    GT_YMIN(geo) = gsgetr (sy1, GSYMIN)
	if (IS_INDEFR(GT_YMAX(geo)))
	    GT_YMAX(geo) = gsgetr (sy1, GSYMAX)

	# Set the number of lines and columns.
	if (IS_INDEFI(GT_NCOLS(geo)))
	    GT_NCOLS(geo) = IM_LEN(in, 1)
	if (IM_NDIM(in) == 1)
	    GT_NLINES(geo) = 1
	else if (IS_INDEFI(GT_NLINES(geo)))
	    GT_NLINES(geo) = IM_LEN(in, 2)

	# Set scale, overiding the number of columns and rows if necessary. 
	if (IS_INDEFR(GT_XSCALE(geo)))
	    GT_XSCALE(geo) = (GT_XMAX(geo) - GT_XMIN(geo)) / (GT_NCOLS(geo) - 1)
	else
	    GT_NCOLS(geo) = abs ((GT_XMAX(geo) - GT_XMIN(geo)) /
	        GT_XSCALE(geo)) + 1
	if (IM_NDIM(in) == 1)
	    GT_YSCALE(geo) = 1.0
	else if (IS_INDEFR(GT_YSCALE(geo)))
	    GT_YSCALE(geo) = (GT_YMAX(geo) - GT_YMIN(geo)) /
	        (GT_NLINES(geo) - 1)
	else
	    GT_NLINES(geo) = abs ((GT_YMAX(geo) - GT_YMIN(geo)) /
	        GT_YSCALE(geo)) + 1

	# Set the output image size.
	IM_LEN(out,1) = GT_NCOLS(geo)
	IM_LEN(out,2) = GT_NLINES(geo)
end


# GEO_DSHIFT -- Adjust the shifts using information in the database file.

procedure geo_dshift (in, out, dt, rec, geo, sx1, sy1)

pointer	in, out		#I pointer to input and output images
pointer	dt		#I pointer to database
int	rec		#I pointer to database record
pointer	geo		#I pointer to geotran structure
pointer	sx1, sy1	#U pointers to linear surfaces

real	gseval()

begin
	# Define the output origin.
	if (IS_INDEFR(GT_XOUT(geo)))
	    GT_XOUT(geo) = (GT_XMAX(geo) + GT_XMIN(geo)) / 2.0
	if (IS_INDEFR(GT_YOUT(geo)))
	    GT_YOUT(geo) = (GT_YMAX(geo) + GT_YMIN(geo)) / 2.0

	# Define the input image origin.
	if (IS_INDEFR(GT_XIN(geo)))
	    GT_XIN(geo) = gseval (sx1, GT_XOUT(geo), GT_YOUT(geo))
	if (IS_INDEFR(GT_YIN(geo)))
	    GT_YIN(geo) = gseval (sy1, GT_XOUT(geo), GT_YOUT(geo))

	# Define the shifts.
	if (IS_INDEFR(GT_XSHIFT(geo)))
	    GT_XSHIFT(geo) = GT_XIN(geo) - gseval (sx1, GT_XOUT(geo),
	        GT_YOUT(geo))
	if (IS_INDEFR(GT_YSHIFT(geo)))
	    GT_YSHIFT(geo) = GT_YIN(geo) - gseval (sy1, GT_XOUT(geo),
	        GT_YOUT(geo))
	
	# Correct the coefficients.
	call geo_xyshiftr (sx1, sy1, GT_XSHIFT(geo), GT_YSHIFT(geo))
end


# GEO_SHIFT -- Compute the shift.

procedure geo_shift (in, out, geo, sx1, sy1)

pointer	in, out		#I pointer to input and output images
pointer	geo		#I pointer to geotran structure
pointer	sx1, sy1	#I pointers to linear surfaces

real	gseval()

begin
	# Determine the output origin.
	if (IS_INDEFR(GT_XOUT(geo)))
	    GT_XOUT(geo) =  (GT_XMAX(geo) + GT_XMIN(geo)) / 2.0
	if (IS_INDEFR(GT_YOUT(geo)))
	    GT_YOUT(geo) =  (GT_YMAX(geo) + GT_YMIN(geo)) / 2.0

	# Determine the input origin.
	if (IS_INDEFR(GT_XIN(geo)))
	    GT_XIN(geo) = (real (IM_LEN (in, 1)) + 1.) / 2.
	if (IS_INDEFR(GT_YIN(geo)))
	    GT_YIN(geo) = (real (IM_LEN (in, 2)) + 1.) / 2.

	# Determine the final x and y shifts.
	if (! IS_INDEFR(GT_XSHIFT(geo)))
	    GT_XOUT(geo) = GT_XIN(geo) + GT_XSHIFT(geo)
	if (! IS_INDEFR(GT_YSHIFT(geo)))
	    GT_YOUT(geo) = GT_YIN(geo) + GT_YSHIFT(geo)
	GT_XSHIFT(geo) = GT_XIN(geo) - gseval (sx1, GT_XOUT(geo),
	        GT_YOUT(geo))
	GT_YSHIFT(geo) = GT_YIN(geo) - gseval (sy1, GT_XOUT(geo),
	    GT_YOUT(geo))

	# Alter coefficients.
	call geo_xyshiftr (sx1, sy1, GT_XSHIFT(geo), GT_YSHIFT(geo))
end


# GEO_DCOEFF -- Alter the linear componets of the surface fit after the fact.

procedure geo_dcoeff (geo, dt, rec, sx1, sy1)

pointer	geo		#I pointer to geotran structure
pointer	dt		#I pointer to database record
int	rec		#I database record
pointer	sx1, sy1	#U pointers to the linear surface

real	dtgetr()
errchk	dtgetr()

begin
	# Get the transformation parameters.
	if (IS_INDEFR(GT_XMAG(geo))) {
	    iferr (GT_XMAG(geo) = dtgetr (dt, rec, "xmag"))
	        GT_XMAG(geo) = dtgetr (dt, rec, "xscale")
	}
	if (IS_INDEFR(GT_YMAG(geo))) {
	    iferr (GT_YMAG(geo) = dtgetr (dt, rec, "ymag"))
	        GT_YMAG(geo) = dtgetr (dt, rec, "yscale")
	}
	if (IS_INDEFR(GT_XROTATION(geo)))
	    GT_XROTATION(geo) =  DEGTORAD(dtgetr (dt, rec, "xrotation"))
	else
	    GT_XROTATION(geo) = DEGTORAD(GT_XROTATION(geo))
	if (IS_INDEFR(GT_YROTATION(geo)))
	    GT_YROTATION(geo) =  DEGTORAD(dtgetr (dt, rec, "yrotation"))
	else
	    GT_YROTATION(geo) = DEGTORAD(GT_YROTATION(geo))

	call geo_rotmagr (sx1, sy1, GT_XMAG(geo), GT_YMAG(geo),
	    GT_XROTATION(geo), GT_YROTATION(geo))
end


# GEO_GWCS -- Compute the ltm and ltv vectors using the GEOTRAN coordinate
# surfaces.

procedure geo_gwcs (geo, sx1, sy1, ltm, ltv)

pointer	geo		# pointer to the geotran structure
pointer	sx1		# pointer to the linear x coordinate surface
pointer	sy1		# pointer to the linear y coordinate surface
double	ltm[2,2]	# rotation matrix
double	ltv[2]		# shift vector

double	xscale, yscale, xmin, ymin
int	ncoeff
pointer	sp, xcoeff, ycoeff
real	xrange, yrange
int	gsgeti()
real	gsgetr()

begin
	# Allocate space for the coefficients.
	call smark (sp)
	ncoeff = max (gsgeti (sx1, GSNSAVE), gsgeti (sy1, GSNSAVE))
	call salloc (xcoeff, ncoeff, TY_REAL)
	call salloc (ycoeff, ncoeff, TY_REAL)

	# Fetch the coefficients.
	call gssave (sx1, Memr[xcoeff])
	call gssave (sy1, Memr[ycoeff])

	# Denormalize the coefficients for non-polynomial functions.
	xrange = gsgetr (sx1, GSXMAX) - gsgetr (sx1, GSXMIN)
	yrange = gsgetr (sy1, GSYMAX) - gsgetr (sy1, GSYMIN)
	if (gsgeti (sx1, GSTYPE) != GS_POLYNOMIAL) {
	    Memr[xcoeff+GS_SAVECOEFF+1] = Memr[xcoeff+GS_SAVECOEFF+1] * 2. /
	        xrange
	    Memr[xcoeff+GS_SAVECOEFF+2] = Memr[xcoeff+GS_SAVECOEFF+2] * 2. /
	        yrange
	}
	if (gsgeti (sy1, GSTYPE) != GS_POLYNOMIAL) {
	    Memr[ycoeff+GS_SAVECOEFF+1] = Memr[ycoeff+GS_SAVECOEFF+1] * 2. /
	        xrange
	    Memr[ycoeff+GS_SAVECOEFF+2] = Memr[ycoeff+GS_SAVECOEFF+2] * 2. /
	        yrange
	}

	# Set the shift vector.
	ltv[1] = Memr[xcoeff+GS_SAVECOEFF]
	ltv[2] = Memr[ycoeff+GS_SAVECOEFF]

	# Set the rotation vector.
	ltm[1,1] = Memr[xcoeff+GS_SAVECOEFF+1]
	ltm[2,1] = Memr[xcoeff+GS_SAVECOEFF+2]
	ltm[1,2] = Memr[ycoeff+GS_SAVECOEFF+1]
	ltm[2,2] = Memr[ycoeff+GS_SAVECOEFF+2]

	# Get the sign of the scale vector which is always +ve.
	xmin = GT_XMIN(geo)
	ymin = GT_YMIN(geo)
	if (GT_XMIN(geo) > GT_XMAX(geo))
	    xscale = -GT_XSCALE(geo)
	else
	    xscale = GT_XSCALE(geo)
	if (GT_YMIN(geo) > GT_YMAX(geo))
	    yscale = -GT_YSCALE(geo)
	else
	    yscale = GT_YSCALE(geo)

	# Correct for reference units that are not in pixels.
	ltv[1] = ltv[1] + ltm[1,1] * xmin + ltm[2,1] * ymin - ltm[1,1] *
	    xscale - ltm[2,1] * yscale
	ltv[2] = ltv[2] + ltm[1,2] * xmin + ltm[2,2] * ymin - ltm[1,2] *
	    xscale - ltm[2,2] * yscale
	ltm[1,1] = ltm[1,1] * xscale
	ltm[2,1] = ltm[2,1] * yscale
	ltm[1,2] = ltm[1,2] * xscale
	ltm[2,2] = ltm[2,2] * yscale

	call sfree (sp)
end


define	LTM	Memd[ltm+(($2)-1)*pdim+($1)-1]

# GEO_SWCS -- Update the wcs and write it to the image header.

procedure geo_swcs (mw, gltm, gltv, ldim)

pointer	mw			# the mwcs descriptor
double	gltm[ldim,ldim]		# the input cd matrix from geotran
double	gltv[ldim]		# the input shift vector from geotran
int	ldim			# number of logical dimensions

int	axes[IM_MAXDIM], naxes, pdim, nelem, axmap, ax1, ax2
pointer	sp, ltm, ltv_1, ltv_2
int	mw_stati()

begin
	# Convert axis bitflags to the axis lists.
	if (ldim == 1) {
	    call mw_gaxlist (mw, 01B, axes, naxes)
	    if (naxes < 1)
	        return
	} else {
	    call mw_gaxlist (mw, 03B, axes, naxes)
	    if (naxes < 2)
	        return
	}

	# Initialize the parameters.
	pdim = mw_stati (mw, MW_NDIM) 
	nelem = pdim * pdim
	axmap = mw_stati (mw, MW_USEAXMAP)
	call mw_seti (mw, MW_USEAXMAP, NO)

	# Allocate working space.
	call smark (sp)
	call salloc (ltm, nelem, TY_DOUBLE)
	call salloc (ltv_1, pdim, TY_DOUBLE)
	call salloc (ltv_2, pdim, TY_DOUBLE)

	# Initialize the vectors and matrices.
	call mw_mkidmd (Memd[ltm], pdim)
	call aclrd (Memd[ltv_1], pdim)
	call aclrd (Memd[ltv_2], pdim)

	# Enter the linear operation.
	ax1 = axes[1]
	Memd[ltv_2+ax1-1] = gltv[1]
	LTM(ax1,ax1) = gltm[1,1]
	if (ldim == 2) {
	    ax2 = axes[2]
	    Memd[ltv_2+ax2-1] = gltv[2]
	    LTM(ax2,ax1) = gltm[2,1]
	    LTM(ax1,ax2) = gltm[1,2]
	    LTM(ax2,ax2) = gltm[2,2]
	}

	# Perform the translation.
	call mw_translated (mw, Memd[ltv_1], Memd[ltm], Memd[ltv_2], pdim)

	call sfree (sp)
	call mw_seti (mw, MW_USEAXMAP, axmap)
end
