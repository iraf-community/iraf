# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imset.h>
include <mach.h>
include <math/gsurfit.h>
include <math/iminterp.h>
include "geotran.h"

define	NMARGIN	3		# number of boundary pixels

# GEOIMTRAN -- Procedure to correct an image for geometric distortion
# Output image blocks nxblock by nyblock pixels are calculated by interpolating
# in the input image.

procedure geoimtran (input, output, geo, sx1, sy1, sx2, sy2, nxblock, nyblock)

pointer	input		# pointer to input image
pointer	output		# pointer to output image
pointer	geo		# pointer to geotran structure
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to higher order surface
int	nxblock, nyblock# working block size

pointer	sp, xref, yref, msi

real	gsgetr()

begin
	# allocate working space
	call smark (sp)
	call salloc (xref, GT_NCOLS(geo), TY_REAL)
	call salloc (yref, GT_NLINES(geo), TY_REAL)

	# initialize the interpolant
	call msiinit (msi, GT_INTERPOLANT(geo))

	# calculate the reference coordinates of the output image pixels
	call georef (geo, Memr[xref], GT_NCOLS(geo), Memr[yref],
	    GT_NLINES(geo), gsgetr (sx1, GSXMIN), gsgetr (sx1, GSXMAX),
	    gsgetr (sx1, GSYMIN), gsgetr (sx1, GSYMAX))

	# interpolate
	call geoigsvector (input, output, geo, msi, Memr[xref],
	    Memr[yref], nxblock, nyblock, sx1, sy1, sx2, sy2)

	# clean up
	call msifree (msi)
	call sfree (sp)
end

# GEOSIMTRAN -- Procedure to correct an image for geometric distortion
# Output image blocks nxblock by nyblock pixels are calculated by interpolating
# in the input image.

procedure geosimtran (input, output, geo, sx1, sy1, sx2, sy2, nxblock, nyblock)

pointer	input		# pointer to input image
pointer	output		# pointer to output image
pointer	geo		# pointer to geotran structure
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to higher order surface
int	nxblock, nyblock# working block size

int	nxsample, nysample
pointer	sp, xsample, ysample, xinterp, yinterp
pointer	xmsi, ymsi, jmsi, msi, xbuf, ybuf, jbuf
real	junk

real	gsgetr()

begin
	# set up sampling size
	nxsample = GT_NCOLS(geo) / GT_XSAMPLE(geo)
	nysample = GT_NLINES(geo) / GT_YSAMPLE(geo)

	# allocate working space and intialize the interpolant
	call smark (sp)
	call salloc (xsample, nxsample, TY_REAL)
	call salloc (ysample, nysample, TY_REAL)
	call salloc (xinterp, GT_NCOLS(geo), TY_REAL)
	call salloc (yinterp, GT_NLINES(geo), TY_REAL)

	# intialize interpolants
	call msiinit (xmsi, II_BILINEAR)
	call msiinit (ymsi, II_BILINEAR)
	call msiinit (msi, GT_INTERPOLANT(geo))
	if (GT_FLUXCONSERVE(geo) == YES)
	    call msiinit (jmsi, II_BILINEAR)

	# calculate the sampled reference coordinates and the interpolated
	# reference coordinates
	call georef (geo, Memr[xsample], nxsample, Memr[ysample],
	    nysample, gsgetr (sx1, GSXMIN), gsgetr (sx1, GSXMAX),
	    gsgetr (sx1, GSYMIN), gsgetr (sx1, GSYMAX))
	call geosample (geo, Memr[xinterp], Memr[yinterp], nxsample, nysample)

	# initialize the buffers
	xbuf = NULL
	ybuf = NULL
	jbuf = NULL

	# set up interpolants
	call geobuffer (sx1, sx2, xmsi, Memr[xsample], Memr[ysample], 1,
	    nxsample, 1, nysample, junk, junk, xbuf)
	call geobuffer (sy1, sy2, ymsi, Memr[xsample], Memr[ysample], 1,
	    nxsample, 1, nysample, junk, junk, ybuf)
	if (GT_FLUXCONSERVE(geo) == YES)
	    call geojbuffer (sx1, sy1, sx2, sy2, jmsi, Memr[xsample],
	        Memr[ysample], 1, nxsample, 1, nysample, junk, junk, jbuf)

	# calculate the coordinates of the output pixels in the input
	# image
	call geoimsivector (input, output, geo, xmsi, ymsi, jmsi, msi, 
	    sx1, sy1, sx2, sy2, Memr[xinterp], Memr[yinterp], nxblock, nyblock)

	# free space
	call msifree (xmsi)
	call msifree (ymsi)
	call msifree (msi)
	if (GT_FLUXCONSERVE(geo) == YES)
	    call msifree (jmsi)
	call mfree (xbuf, TY_REAL)
	call mfree (ybuf, TY_REAL)
	if (GT_FLUXCONSERVE(geo) == YES)
	    call mfree (jbuf, TY_REAL)
	call sfree (sp)
end


# GEOIMSIVECTOR -- Procedure to interpolate the surface coordinates

procedure geoimsivector (in, out, geo, xmsi, ymsi, jmsi, msi, sx1, sy1, sx2,
    sy2, xref, yref, ncols, nlines) 

pointer	in		# pointer to input image
pointer	out		# pointer to output image
pointer	geo		# pointer to geotran structure
pointer	xmsi, ymsi	# pointer to the interpolation cord surfaces
pointer	jmsi		# pointer to Jacobian surface
pointer	msi		# pointer to interpolation surface
pointer	sx1, sy1	# linear surface pointers
pointer	sx2, sy2	# distortion surface pointers
real	xref[ARB]	# x reference coordinates
real	yref[ARB]	# y reference coordinates
int	ncols, nlines	# number of columns and rows

int	j
pointer	sp, x, y, xin, yin, xout, yout, inbuf, outbuf 
real	factor

pointer	imgs2r(), imps2r()
real	jfactor()

begin
	call smark (sp)
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)
	call salloc (xin, ncols, TY_REAL)
	call salloc (yin, ncols, TY_REAL)
	call salloc (xout, ncols, TY_REAL)
	call salloc (yout, ncols, TY_REAL)

	# fit the interpolant
	inbuf = imgs2r (in, 1, int (IM_LEN(in,1)), 1, int (IM_LEN(in,2)))
	if (inbuf == EOF)
	    call error (0, "Error reading image")
	call msifit (msi, Memr[inbuf], int (IM_LEN(in,1)), int (IM_LEN(in,2)),
	    int (IM_LEN(in,1)))

	# compute the output buffer
	do j = 1, nlines {

	    # compute coordinates
	    call amovkr (yref[j], Memr[y], ncols)
	    call msivector (xmsi, xref, Memr[y], Memr[xin], ncols)
	    call msivector (ymsi, xref, Memr[y], Memr[yin], ncols)

	    # correct for outof bounds pixels
	    call geobtran (in, geo, Memr[xin], Memr[yin], Memr[xout],
	        Memr[yout], ncols)

	    # write to output image
	    outbuf = imps2r (out, 1, ncols, j, j)
	    if (outbuf == EOF)
		call error (0, "Error writing output image")
	    call msivector (msi, Memr[xout], Memr[yout], Memr[outbuf], ncols)

	    # perform constant boundary extension
	    if (GT_BOUNDARY(geo) == BT_CONSTANT)
		call geobconstant (in, geo, Memr[xin], Memr[yin],
		    Memr[outbuf], Memr[outbuf], ncols)

	    # preserve flux in image
	    if (GT_FLUXCONSERVE(geo) == YES) {
		factor = GT_XSCALE(geo) * GT_YSCALE(geo)
		if (GT_GEOMODE(geo) == GT_LINEAR || (sx2 == NULL && sy2 ==
		    NULL))
		    call amulkr (Memr[outbuf], factor * jfactor (sx1, sy1),
		        Memr[outbuf], ncols)
		else {
		    call geomsiflux (jmsi, xref, yref, Memr[outbuf], 1, ncols,
		        j, 1, 1)
		    call amulkr (Memr[outbuf], factor, Memr[outbuf], ncols)
		}
	    }
	}

	call sfree (sp)
end

# GEOIGSVECTOR -- Procedure to evaluate the surface without sampling

procedure geoigsvector (input, output, geo, msi, xref, yref, ncols, nlines,
    sx1, sy1, sx2, sy2)

pointer	input		# pointer to input image
pointer	output		# pointer to output image
pointer	geo		# pointer to geotran structure
pointer	msi		# pointer to interpolant
real	xref[ARB]	# x reference array
real	yref[ARB]	# y reference array
int	ncols, nlines	# number of columns and lines
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to distortion surface

int	j
pointer	sp, y, xin, yin, xout, yout, temp, inbuf, outbuf
real	factor

pointer	imgs2r(), imps2r()
real	jfactor()

begin
	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (xin, ncols, TY_REAL)
	call salloc (yin, ncols, TY_REAL)
	call salloc (xout, ncols, TY_REAL)
	call salloc (yout, ncols, TY_REAL)
	call salloc (temp, ncols, TY_REAL)

	# fill image buffer
	inbuf = imgs2r (input, 1, int (IM_LEN(input,1)), 1,
	    int (IM_LEN(input,2)))
	if (inbuf == EOF)
	    call error (0, "Error reading image")

	# fit the interpolant
	call msifit (msi, Memr[inbuf], int (IM_LEN(input,1)),
	    int (IM_LEN(input,2)), int (IM_LEN(input,1)))

	# calculate the  x and y input image coordinates
	do j = 1, nlines {

	    # get output image buffer
	    outbuf = imps2r (output, 1, ncols, j, j)
	    if (output == EOF)
		call error (0, "Error writing output image")

	    # fit x coords
	    call amovkr (yref[j], Memr[y], ncols)
	    call gsvector (sx1, xref, Memr[y], Memr[xin], ncols)
	    if (sx2 != NULL) {
		call gsvector (sx2, xref, Memr[y], Memr[temp], ncols)
		call aaddr (Memr[xin], Memr[temp], Memr[xin], ncols)
	    }

	    # fit ycoords
	    call gsvector (sy1, xref, Memr[y], Memr[yin], ncols)
	    if (sy2 != NULL) {
		call gsvector (sy2, xref, Memr[y], Memr[temp], ncols)
		call aaddr (Memr[yin], Memr[temp], Memr[yin], ncols)
	    }

	    # convert interpolation coords for out of bounds
	    call geobtran (input, geo, Memr[xin], Memr[yin], Memr[xout], 
	        Memr[yout], ncols)

	    # interpolate in input image
	    call msivector (msi, Memr[xout], Memr[yout], Memr[outbuf], ncols)

	    # correct for constant boundary extension
	    if (GT_BOUNDARY(geo) == BT_CONSTANT)
		call geobconstant (input, geo, Memr[xin], Memr[yin],
		    Memr[outbuf], Memr[outbuf], ncols)

	    # preserve flux in image
	    if (GT_FLUXCONSERVE(geo) == YES) {
		factor = GT_XSCALE(geo) * GT_YSCALE(geo)
		if (GT_GEOMODE(geo) == GT_LINEAR || (sx2 == NULL && sy2 ==
		        NULL))
		    call amulkr (Memr[outbuf], factor * jfactor (sx1, sy1),
		        Memr[outbuf], ncols)
		else
		    call geogsflux (xref, yref, Memr[outbuf], 1, ncols, j,
		        sx1, sy1, sx2, sy2)
		    call amulkr (Memr[outbuf], factor, Memr[outbuf], ncols)
	    }
	}

	call sfree (sp)
end

# GEOBTRAN -- Procedure to map out of bound pixel into the input image

procedure geobtran (input, geo, xin, yin, xout, yout, ncols)

pointer	input		# pointer to the input image
pointer	geo		# pointer to geotran strcuture
real	xin[ARB]	# x input coords
real	yin[ARB]	# y input coords
real	xout[ARB]	# x output coords
real	yout[ARB]	# y output coords
int	ncols		# number of columns

int	i
real	xmax, ymax, xtemp, ytemp

begin
	xmax = IM_LEN(input,1)
	ymax = IM_LEN(input,2)

	switch (GT_BOUNDARY(geo)) {
	case BT_CONSTANT, BT_NEAREST:
	    do i = 1, ncols {
		if (xin[i] < 1.0)
		    xout[i] = 1.0
		else if (xin[i] > xmax)
		    xout[i] = xmax
		else
		    xout[i] = xin[i]
		if (yin[i] < 1.0)
		    yout[i] = 1.0
		else if (yin[i] > ymax)
		    yout[i] = ymax
		else
		    yout[i] = yin[i]
	    }
	case BT_REFLECT:
	    do i = 1, ncols {
		if (xin[i] < 1.0)
		    xout[i] = 1.0 + (1.0 - xin[i])
		else if (xin[i] > xmax)
		    xout[i] = xmax - (xin[i] - xmax)
		else
		    xout[i] = xin[i]
		if (yin[i] < 1.0)
		    yout[i] = 1.0 + (1.0 - yin[i])
		else if (yin[i] > ymax)
		    yout[i] = ymax - (yin[i] - ymax)
		else
		    yout[i] = yin[i]
	    }
	case BT_WRAP:
	    do i = 1, ncols {
		xtemp = xin[i]
		ytemp = yin[i]

		if (xtemp < 1.0) {
		    while (xtemp < 1.0)
		        xtemp = xtemp + xmax
		    if (xtemp < 1.0)
			xtemp = xmax - xtemp
		    else if (xtemp > xmax)
			xtemp = 2.0 + xmax - xtemp
		} else if (xtemp > xmax) {
		    while (xtemp > xmax)
		        xtemp = xtemp - xmax
		    if (xtemp < 1.0)
			xtemp = xmax - xtemp
		    else if (xtemp > xmax)
			xtemp = 2.0 + xmax - xtemp
		}
		xout[i] = xtemp

		if (ytemp < 1.0) {
		    while (ytemp < 1.0)
		        ytemp = ytemp + ymax
		    if (ytemp < 1.0)
			ytemp = ymax - ytemp
		    else if (ytemp > ymax)
			ytemp = 2.0 + ymax - ytemp
		} else if (ytemp > ymax) {
		    while (ytemp > ymax)
		        ytemp = ytemp - ymax
		    if (ytemp < 1.0)
			ytemp = ymax - ytemp
		    else if (ytemp > ymax)
			ytemp = 2.0 + ymax - ytemp
		}
		yout[i] = ytemp
	    }
	}
end

# GEOBCONSTANT -- Procedure to map out of bound pixel into the input image

procedure geobconstant (input, geo, xin, yin, inbuf, outbuf, ncols)

pointer	input		# pointer to the input image
pointer	geo		# pointer to geotran structure
real	xin[ARB]	# x input coords
real	yin[ARB]	# y input coords
real	inbuf[ARB]	# input buffer
real	outbuf[ARB]	# output buffer
int	ncols		# number of columns

int	i
real	xmax, ymax, constant

begin
	xmax = IM_LEN(input,1)
	ymax = IM_LEN(input,2)
	constant = GT_CONSTANT(geo)

	do i = 1, ncols {
	    if (xin[i] < 1.0 || xin[i] > xmax || yin[i] < 1.0 || yin[i] > ymax)
		outbuf[i] = constant
	    else
		outbuf[i] = inbuf[i]
	}
end
