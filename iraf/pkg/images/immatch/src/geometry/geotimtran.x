# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imset.h>
include <mach.h>
include <math/gsurfit.h>
include <math/iminterp.h>
include "geotran.h"

# GEO_IMTRAN -- Correct an entire image for geometric distortion using the
# transformed coordinates and image interpolation.

procedure geo_imtran (input, output, geo, sx1, sy1, sx2, sy2)

pointer	input			#I pointer to input image
pointer	output			#I pointer to output image
pointer	geo			#I pointer to geotran structure
pointer	sx1, sy1		#I pointers to linear surface descriptors
pointer	sx2, sy2		#I pointer to higher order surface descriptors

int	nincr
pointer	sp, xref, yref, msi
real	shift
real	gsgetr()

begin
	# Initialize the interpolant and compute the out-of-bounds pixel
	# margin required.
	if (IM_NDIM(input) == 1) {
	    call asitype (GT_INTERPSTR(geo), GT_INTERPOLANT(geo),
	        GT_NSINC(geo), nincr, shift)
	    call asisinit (msi, GT_INTERPOLANT(geo), GT_NSINC(geo),
		nincr, shift, 0.0)
	} else {
	    call msitype (GT_INTERPSTR(geo), GT_INTERPOLANT(geo),
	        GT_NSINC(geo), nincr, shift)
	    call msisinit (msi, GT_INTERPOLANT(geo), GT_NSINC(geo),
		nincr, nincr, shift, shift, 0.0)
	}
        call geo_margset (sx1, sy1, sx2, sy2, GT_XMIN(geo), GT_XMAX(geo),
            GT_NCOLS(geo), GT_YMIN(geo), GT_YMAX(geo), GT_NLINES(geo),
            GT_INTERPOLANT(geo), GT_NSINC(geo),  GT_NXYMARGIN(geo))

	# Allocate working space.
	call smark (sp)
	call salloc (xref, GT_NCOLS(geo), TY_REAL)
	call salloc (yref, GT_NLINES(geo), TY_REAL)

	# Calculate the reference coordinates of the input image pixels.
	call geo_ref (geo, Memr[xref], 1, GT_NCOLS(geo), GT_NCOLS(geo),
	    Memr[yref], 1, GT_NLINES(geo), GT_NLINES(geo), gsgetr (sx1,
	    GSXMIN), gsgetr (sx1, GSXMAX), gsgetr (sx1, GSYMIN), gsgetr (sx1,
	    GSYMAX), GT_ONE)

        # Configure the out-of-bounds pixel references for the input image.
        call geo_imset (input, geo, sx1, sy1, sx2, sy2, Memr[xref],
	    GT_NCOLS(geo), Memr[yref], GT_NLINES(geo))

	# Interpolate.
	call geo_gsvector (input, output, geo, msi, Memr[xref], 1,
	    GT_NCOLS(geo), Memr[yref], 1, GT_NLINES(geo), sx1, sy1, sx2, sy2)

	# Clean up.
	if (IM_NDIM(input) == 1)
	    call asifree (msi)
	else
	    call msifree (msi)
	call sfree (sp)
end


# GEO_SIMTRAN -- Correct an entire image for geometric distortion using 
# nterpolated coordinate surfaces to speed up computation of the transformed
# coordinates and image interpolation.

procedure geo_simtran (input, output, geo, sx1, sy1, sx2, sy2)

pointer	input			#I pointer to input image
pointer	output			#I pointer to output image
pointer	geo			#I pointer to geotran structure
pointer	sx1, sy1		#I pointer to linear surface descriptors
pointer	sx2, sy2		#I pointer to higher order surface descriptors

int	nxsample, nysample, nincr
pointer	sp, xsample, ysample, xinterp, yinterp
pointer	xmsi, ymsi, jmsi, msi, xbuf, ybuf, jbuf
real	shift
real	gsgetr()

begin
	# Allocate working space and intialize the interpolant.
	call smark (sp)
	call salloc (xsample, GT_NCOLS(geo), TY_REAL)
	call salloc (ysample, GT_NLINES(geo), TY_REAL)
	call salloc (xinterp, GT_NCOLS(geo), TY_REAL)
	call salloc (yinterp, GT_NLINES(geo), TY_REAL)

	# Set up sampling size.
	if (GT_NCOLS(geo) == 1)
	    nxsample = 1
	else
	    nxsample = GT_NCOLS(geo) / GT_XSAMPLE(geo)
	if (GT_NLINES(geo) == 1)
	    nysample = 1
	else
	    nysample = GT_NLINES(geo) / GT_YSAMPLE(geo)

	# Initialize interpolants.
	if (IM_NDIM(input) == 1) {
	    call asiinit (xmsi, II_LINEAR)
	    call asiinit (ymsi, II_LINEAR)
	    call asitype (GT_INTERPSTR(geo), GT_INTERPOLANT(geo),
		GT_NSINC(geo), nincr, shift)
	    call asisinit (msi, GT_INTERPOLANT(geo), GT_NSINC(geo),
		nincr, shift, 0.0)
	    if (GT_FLUXCONSERVE(geo) == YES)
	        call asiinit (jmsi, II_LINEAR)
	} else {
	    call msiinit (xmsi, II_BILINEAR)
	    call msiinit (ymsi, II_BILINEAR)
	    call msitype (GT_INTERPSTR(geo), GT_INTERPOLANT(geo),
		GT_NSINC(geo), nincr, shift)
	    call msisinit (msi, GT_INTERPOLANT(geo), GT_NSINC(geo),
		nincr, nincr, shift, shift, 0.0)
	    if (GT_FLUXCONSERVE(geo) == YES)
	        call msiinit (jmsi, II_BILINEAR)
	}
        call geo_margset (sx1, sy1, sx2, sy2, GT_XMIN(geo), GT_XMAX(geo),
            GT_NCOLS(geo), GT_YMIN(geo), GT_YMAX(geo), GT_NLINES(geo),
            GT_INTERPOLANT(geo), GT_NSINC(geo),  GT_NXYMARGIN(geo))

        # Setup input image boundary extension parameters.
        call geo_ref (geo, Memr[xsample], 1, GT_NCOLS(geo), GT_NCOLS(geo),
            Memr[ysample], 1, GT_NLINES(geo), GT_NLINES(geo), gsgetr (sx1,
            GSXMIN), gsgetr (sx1, GSXMAX), gsgetr (sx1, GSYMIN), gsgetr (sx1,
            GSYMAX), GT_ONE)
        call geo_imset (input, geo, sx1, sy1, sx2, sy2, Memr[xsample],
            GT_NCOLS(geo), Memr[ysample], GT_NLINES(geo))

	# Calculate the sampled reference coordinates and the interpolated
	# reference coordinates.
	call geo_ref (geo, Memr[xsample], 1, nxsample, nxsample, Memr[ysample],
	    1, nysample, nysample, gsgetr (sx1, GSXMIN), gsgetr (sx1, GSXMAX),
	    gsgetr (sx1, GSYMIN), gsgetr (sx1, GSYMAX), GT_ONE)
	call geo_sample (geo, Memr[xinterp], 1, GT_NCOLS(geo), nxsample,
	    Memr[yinterp], 1, GT_NLINES(geo), nysample, GT_ONE)

	# Initialize the buffers
	xbuf = NULL
	ybuf = NULL
	jbuf = NULL

	# Set up interpolants
	call geo_xbuffer (sx1, sx2, xmsi, Memr[xsample], Memr[ysample], 1,
	    nxsample, 1, nysample, xbuf)
	call geo_ybuffer (sy1, sy2, ymsi, Memr[xsample], Memr[ysample], 1,
	    nxsample, 1, nysample, ybuf)
	if (GT_FLUXCONSERVE(geo) == YES && (sx2 != NULL || sy2 != NULL)) {
	    if (IM_NDIM(input) == 1)
	        call geo_jbuffer (sx1, NULL, sx2, NULL, jmsi, Memr[xsample],
	            Memr[ysample], 1, nxsample, 1, nysample, jbuf)
	    else
	        call geo_jbuffer (sx1, sy1, sx2, sy2, jmsi, Memr[xsample],
	            Memr[ysample], 1, nxsample, 1, nysample, jbuf)
	}

	# Transform the image.
	call geo_msivector (input, output, geo, xmsi, ymsi, jmsi, msi, 
	    sx1, sy1, sx2, sy2, Memr[xinterp], 1, GT_NCOLS(geo), nxsample,
	    Memr[yinterp], 1, GT_NLINES(geo), nysample, 1, 1)

	# Free space.
	if (IM_NDIM(input) == 1) {
	    call asifree (xmsi)
	    call asifree (ymsi)
	    call asifree (msi)
	    if (GT_FLUXCONSERVE(geo) == YES)
	        call asifree (jmsi)
	} else {
	    call msifree (xmsi)
	    call msifree (ymsi)
	    call msifree (msi)
	    if (GT_FLUXCONSERVE(geo) == YES)
	        call msifree (jmsi)
	}
	call mfree (xbuf, TY_REAL)
	call mfree (ybuf, TY_REAL)
	if (jbuf != NULL)
	    call mfree (jbuf, TY_REAL)
	call sfree (sp)
end


## GEO_IMSIVECTOR -- Evaluate the output image using interpolated surface
## coordinates.
#
#procedure geo_imsivector (in, out, geo, xmsi, ymsi, jmsi, msi, sx1, sy1, sx2,
#        sy2, xref, yref, ncols, nlines) 
#
#pointer	in			#I pointer to input image
#pointer	out			#I pointer to output image
#pointer	geo			#I pointer to geotran structure
#pointer	xmsi, ymsi		#I pointer to the interpolation xy surfaces
#pointer	jmsi			#I pointer to Jacobian surface
#pointer	msi			#I pointer to interpolation surface
#pointer	sx1, sy1		#I linear surface descriptors
#pointer	sx2, sy2		#I distortion surface pointers
#real	xref[ARB]		#I x reference coordinates
#real	yref[ARB]		#I y reference coordinates
#int	ncols, nlines		#I number of columns and rows
#
#int	j
#pointer	sp, x, y, xin, yin, xout, yout, inbuf, outbuf 
#real	factor
#pointer	imgs1r(), imgs2r(), imps1r(), imps2r()
#real	geo_jfactor()
#
#begin
#	# Allocate working space.
#	call smark (sp)
#	call salloc (x, ncols, TY_REAL)
#	call salloc (y, ncols, TY_REAL)
#	call salloc (xin, ncols, TY_REAL)
#	call salloc (yin, ncols, TY_REAL)
#	call salloc (xout, ncols, TY_REAL)
#	call salloc (yout, ncols, TY_REAL)
#
#	# Fit the interpolant
#	if (IM_NDIM(in) == 1)
#	    inbuf = imgs1r (in, 1, int (IM_LEN(in,1)))
#	else
#	    inbuf = imgs2r (in, 1, int (IM_LEN(in,1)), 1, int (IM_LEN(in,2)))
#	if (inbuf == EOF)
#	    call error (0, "Error reading image")
#	if (IM_NDIM(in) == 1)
#	    call asifit (msi, Memr[inbuf], int (IM_LEN(in,1)))
#	else
#	    call msifit (msi, Memr[inbuf], int (IM_LEN(in,1)),
#	        int (IM_LEN(in,2)), int (IM_LEN(in,1)))
#
#	# Compute the output bufferr.
#	do j = 1, nlines {
#
#	    # Compute coordinates.
#	    call amovkr (yref[j], Memr[y], ncols)
#	    if (IM_NDIM(in) == 1) {
#	        call asivector (xmsi, xref, Memr[xin], ncols)
#	        call asivector (ymsi, xref, Memr[yin], ncols)
#	    } else {
#	        call msivector (xmsi, xref, Memr[y], Memr[xin], ncols)
#	        call msivector (ymsi, xref, Memr[y], Memr[yin], ncols)
#	    }
#
#	    # Correct for out-of-bounds pixels.
#	    call geo_btran (in, geo, Memr[xin], Memr[yin], Memr[xout],
#	        Memr[yout], ncols)
#
#	    # Write to output image.
#	    if (IM_NDIM(in) == 1)
#	        outbuf = imps1r (out, 1, ncols)
#	    else
#	        outbuf = imps2r (out, 1, ncols, j, j)
#	    if (outbuf == EOF)
#		call error (0, "Error writing output image")
#	    if (IM_NDIM(in) == 1)
#	        call asivector (msi, Memr[xout], Memr[outbuf], ncols)
#	    else
#	        call msivector (msi, Memr[xout], Memr[yout], Memr[outbuf],
#		    ncols)
#
#	    # Perform constant boundary extension.
#	    if (GT_BOUNDARY(geo) == BT_CONSTANT)
#		call geo_bconstant (in, geo, Memr[xin], Memr[yin],
#		    Memr[outbuf], Memr[outbuf], ncols)
#
#	    # Preserve flux in image.
#	    if (GT_FLUXCONSERVE(geo) == YES) {
#		factor = GT_XSCALE(geo) * GT_YSCALE(geo)
#		if (GT_GEOMODE(geo) == GT_LINEAR || (sx2 == NULL && sy2 ==
#		    NULL)) {
#		    if (IM_NDIM(in) == 1)
#		        call amulkr (Memr[outbuf], factor * geo_jfactor (sx1,
#			    NULL), Memr[outbuf], ncols)
#		    else
#		        call amulkr (Memr[outbuf], factor * geo_jfactor (sx1,
#			    sy1), Memr[outbuf], ncols)
#		} else {
#		    if (IM_NDIM(in) == 1)
#		        call geo_msiflux (jmsi, xref, yref, Memr[outbuf],
#			    1, ncols, 0, 1, 1)
#		    else
#		        call geo_msiflux (jmsi, xref, yref, Memr[outbuf],
#			    1, ncols, j, 1, 1)
#		    call amulkr (Memr[outbuf], factor, Memr[outbuf], ncols)
#		}
#	    }
#	}
#
#	call sfree (sp)
#end


## GEO_IGSVECTOR -- Evaluate the output image using fitted coordinates.
#
#procedure geo_igsvector (input, output, geo, msi, xref, yref, ncols, nlines,
#        sx1, sy1, sx2, sy2)
#
#pointer	input			#I pointer to input image
#pointer	output			#I pointer to output image
#pointer	geo			#I pointer to geotran structure
#pointer	msi			#I pointer to interpolant
#real	xref[ARB]		#I x reference array
#real	yref[ARB]		#I y reference array
#int	ncols, nlines		#I number of columns and lines
#pointer	sx1, sy1		#I pointer to linear surface
#pointer	sx2, sy2		#I pointer to distortion surface
#
#int	j
#pointer	sp, y, xin, yin, xout, yout, temp, inbuf, outbuf
#real	factor
#pointer	imgs1r(), imgs2r(), imps1r(), imps2r()
#real	geo_jfactor()
#
#begin
#	# Allocate working space.
#	call smark (sp)
#	call salloc (y, ncols, TY_REAL)
#	call salloc (xin, ncols, TY_REAL)
#	call salloc (yin, ncols, TY_REAL)
#	call salloc (xout, ncols, TY_REAL)
#	call salloc (yout, ncols, TY_REAL)
#	call salloc (temp, ncols, TY_REAL)
#
#	# Fill image buffer.
#	if (IM_NDIM(input) == 1)
#	    inbuf = imgs1r (input, 1, int (IM_LEN(input,1)))
#	else
#	    inbuf = imgs2r (input, 1, int (IM_LEN(input,1)), 1,
#	        int (IM_LEN(input,2)))
#	if (inbuf == EOF)
#	    call error (0, "Error reading image")
#
#	# Fit the interpolant.
#	if (IM_NDIM(input) == 1)
#	    call asifit (msi, Memr[inbuf], int (IM_LEN(input,1)))
#	else
#	    call msifit (msi, Memr[inbuf], int (IM_LEN(input,1)),
#	        int (IM_LEN(input,2)), int (IM_LEN(input,1)))
#
#	# Calculate the  x and y input image coordinates.
#	do j = 1, nlines {
#
#	    # Get output image buffer.
#	    if (IM_NDIM(input) == 1)
#	        outbuf = imps1r (output, 1, ncols)
#	    else
#	        outbuf = imps2r (output, 1, ncols, j, j)
#	    if (output == EOF)
#		call error (0, "Error writing output image")
#
#	    # Fit x coords.
#	    call amovkr (yref[j], Memr[y], ncols)
#	    call gsvector (sx1, xref, Memr[y], Memr[xin], ncols)
#	    if (sx2 != NULL) {
#		call gsvector (sx2, xref, Memr[y], Memr[temp], ncols)
#		call aaddr (Memr[xin], Memr[temp], Memr[xin], ncols)
#	    }
#
#	    # Fit y coords.
#	    call gsvector (sy1, xref, Memr[y], Memr[yin], ncols)
#	    if (sy2 != NULL) {
#		call gsvector (sy2, xref, Memr[y], Memr[temp], ncols)
#		call aaddr (Memr[yin], Memr[temp], Memr[yin], ncols)
#	    }
#
#	    # Compute of of bounds pixels.
#	    call geo_btran (input, geo, Memr[xin], Memr[yin], Memr[xout], 
#	        Memr[yout], ncols)
#
#	    # Interpolate in input image.
#	    if (IM_NDIM(input) == 1)
#	        call asivector (msi, Memr[xout], Memr[outbuf], ncols)
#	    else
#	        call msivector (msi, Memr[xout], Memr[yout], Memr[outbuf],
#		    ncols)
#
#	    # Correct for constant boundary extension.
#	    if (GT_BOUNDARY(geo) == BT_CONSTANT)
#		call geo_bconstant (input, geo, Memr[xin], Memr[yin],
#		    Memr[outbuf], Memr[outbuf], ncols)
#
#	    # Preserve flux in image.
#	    if (GT_FLUXCONSERVE(geo) == YES) {
#		factor = GT_XSCALE(geo) * GT_YSCALE(geo)
#		if (GT_GEOMODE(geo) == GT_LINEAR || (sx2 == NULL && sy2 ==
#		        NULL)) {
#		    if (IM_NDIM(input) == 1)
#		        call amulkr (Memr[outbuf], factor * geo_jfactor (sx1,
#			    NULL), Memr[outbuf], ncols)
#		    else
#		        call amulkr (Memr[outbuf], factor * geo_jfactor (sx1,
#			    sy1), Memr[outbuf], ncols)
#		} else {
#		    if (IM_NDIM(input) == 1)
#		        call geo_gsflux (xref, yref, Memr[outbuf], 1, ncols, j,
#		            sx1, NULL, sx2, NULL)
#		    else
#		        call geo_gsflux (xref, yref, Memr[outbuf], 1, ncols, j,
#		            sx1, sy1, sx2, sy2)
#		    call amulkr (Memr[outbuf], factor, Memr[outbuf], ncols)
#		}
#	    }
#	}
#
#	call sfree (sp)
#end


## GEO_BTRAN -- Map out-of-bounds pixel into the input image.
#
#procedure geo_btran (input, geo, xin, yin, xout, yout, ncols)
#
#pointer	input			#I pointer to the input image
#pointer	geo			#I pointer to geotran strcuture
#real	xin[ARB]		#I x input coords
#real	yin[ARB]		#I y input coords
#real	xout[ARB]		#O x output coords
#real	yout[ARB]		#O y output coords
#int	ncols			#I number of columns
#
#int	i
#real	xmax, ymax, xtemp, ytemp
#
#begin
#	xmax = IM_LEN(input,1)
#	if (IM_NDIM(input) == 1)
#	    ymax = 1.0
#	else
#	    ymax = IM_LEN(input,2)
#
#	switch (GT_BOUNDARY(geo)) {
#	case BT_CONSTANT, BT_NEAREST:
#	    do i = 1, ncols {
#		if (xin[i] < 1.0)
#		    xout[i] = 1.0
#		else if (xin[i] > xmax)
#		    xout[i] = xmax
#		else
#		    xout[i] = xin[i]
#		if (yin[i] < 1.0)
#		    yout[i] = 1.0
#		else if (yin[i] > ymax)
#		    yout[i] = ymax
#		else
#		    yout[i] = yin[i]
#	    }
#	case BT_REFLECT:
#	    do i = 1, ncols {
#		if (xin[i] < 1.0)
#		    xout[i] = 1.0 + (1.0 - xin[i])
#		else if (xin[i] > xmax)
#		    xout[i] = xmax - (xin[i] - xmax)
#		else
#		    xout[i] = xin[i]
#		if (yin[i] < 1.0)
#		    yout[i] = 1.0 + (1.0 - yin[i])
#		else if (yin[i] > ymax)
#		    yout[i] = ymax - (yin[i] - ymax)
#		else
#		    yout[i] = yin[i]
#	    }
#	case BT_WRAP:
#	    do i = 1, ncols {
#		xtemp = xin[i]
#		ytemp = yin[i]
#
#		if (xtemp < 1.0) {
#		    while (xtemp < 1.0)
#		        xtemp = xtemp + xmax
#		    if (xtemp < 1.0)
#			xtemp = xmax - xtemp
#		    else if (xtemp > xmax)
#			xtemp = 2.0 + xmax - xtemp
#		} else if (xtemp > xmax) {
#		    while (xtemp > xmax)
#		        xtemp = xtemp - xmax
#		    if (xtemp < 1.0)
#			xtemp = xmax - xtemp
#		    else if (xtemp > xmax)
#			xtemp = 2.0 + xmax - xtemp
#		}
#		xout[i] = xtemp
#
#		if (ytemp < 1.0) {
#		    while (ytemp < 1.0)
#		        ytemp = ytemp + ymax
#		    if (ytemp < 1.0)
#			ytemp = ymax - ytemp
#		    else if (ytemp > ymax)
#			ytemp = 2.0 + ymax - ytemp
#		} else if (ytemp > ymax) {
#		    while (ytemp > ymax)
#		        ytemp = ytemp - ymax
#		    if (ytemp < 1.0)
#			ytemp = ymax - ytemp
#		    else if (ytemp > ymax)
#			ytemp = 2.0 + ymax - ytemp
#		}
#		yout[i] = ytemp
#	    }
#	}
#end


## GEO_BCONSTANT -- Map constant out-of-bounds pixels into the input image.
#
#procedure geo_bconstant (input, geo, xin, yin, inbuf, outbuf, ncols)
#
#pointer	input			#I pointer to the input image
#pointer	geo			#I pointer to geotran structure
#real	xin[ARB]		#I x input coords
#real	yin[ARB]		#I y input coords
#real	inbuf[ARB]		#I input buffer
#real	outbuf[ARB]		#O output buffer
#int	ncols			#I number of columns
#
#int	i
#real	xmax, ymax, constant
#
#begin
#	xmax = IM_LEN(input,1)
#	if (IM_NDIM(input) == 1)
#	    ymax = 1.0
#	else
#	    ymax = IM_LEN(input,2)
#	constant = GT_CONSTANT(geo)
#	do i = 1, ncols {
#	    if (xin[i] < 1.0 || xin[i] > xmax || yin[i] < 1.0 || yin[i] > ymax)
#		outbuf[i] = constant
#	    else
#		outbuf[i] = inbuf[i]
#	}
#end
