# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imset.h>
include <mach.h>
include <math/gsurfit.h>
include <math/iminterp.h>
include "geotran.h"

define	NMARGIN	3		# number of boundary pixels

# GEOTRAN -- Procedure to correct an image for geometric distortion
# Output image blocks nxblock by nyblock pixels are calculated by interpolating
# in the input image.

procedure geotran (input, output, geo, sx1, sy1, sx2, sy2, nxblock, nyblock)

pointer	input			# pointer to input image
pointer	output			# pointer to output image
pointer	geo			# pointer to geotran structure
pointer	sx1, sy1		# pointer to linear surface
pointer	sx2, sy2		# pointer to higher order surface
int	nxblock, nyblock	# working block size

int	ncols, nlines, l1, l2, c1, c2
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

	# setup input image boundary extension parameters
	call geoimset (input, geo, sx1, sy1, sx2, sy2, Memr[xref],
	    GT_NCOLS(geo), Memr[yref], GT_NLINES(geo))

	# loop over the line blocks
	for (l1 = 1; l1 <= GT_NLINES(geo); l1 = l1 + nyblock) {

	    # l1 and l2 are the line limits in the output image
	    l2 = min (l1 + nyblock - 1, GT_NLINES(geo)) 
	    nlines = l2 - l1 + 1

	    # loop over the column blocks
	    for (c1 = 1; c1 <= GT_NCOLS(geo); c1 = c1 + nxblock) {

		# c1 and c2 are the column limits in the output image
		c2 = min (c1 + nxblock - 1, GT_NCOLS(geo))
		ncols = c2 - c1 + 1

		# interpolate
		call geogsvector (input, output, geo, msi, Memr[xref],
		    Memr[yref], c1, c2, l1, l2, sx1, sy1, sx2, sy2)
	    }
	}

	# clean up
	call msifree (msi)
	call sfree (sp)
end

# GEOSTRAN -- Procedure to correct an image for geometric distortion
# Output image blocks nxblock by nyblock pixels are calculated by interpolating
# in the input image.

procedure geostran (input, output, geo, sx1, sy1, sx2, sy2, nxblock, nyblock)

pointer	input		# pointer to input image
pointer	output		# pointer to output image
pointer	geo		# pointer to geotran structure
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to higher order surface
int	nxblock, nyblock# working block size

int	nxsample, nysample, ncols, nlines, l1, l2, c1, c2
int	line1, line2, llast1, llast2
pointer	sp, xsample, ysample, xinterp, yinterp
pointer	xmsi, ymsi, jmsi, msi, xbuf, ybuf, jbuf

real	gsgetr()

begin
	# allocate working space and intialize the interpolant
	call smark (sp)
	call salloc (xsample, GT_NCOLS(geo), TY_REAL)
	call salloc (ysample, GT_NLINES(geo), TY_REAL)
	call salloc (xinterp, GT_NCOLS(geo), TY_REAL)
	call salloc (yinterp, GT_NLINES(geo), TY_REAL)

	# compute the sample size
	nxsample = GT_NCOLS(geo) / GT_XSAMPLE(geo)
	nysample = GT_NLINES(geo) / GT_YSAMPLE(geo)

	# intialize interpolants
	call msiinit (xmsi, II_BILINEAR)
	call msiinit (ymsi, II_BILINEAR)
	call msiinit (msi, GT_INTERPOLANT(geo))
	if (GT_FLUXCONSERVE(geo) == YES)
	    call msiinit (jmsi, II_BILINEAR)

	# setup input image boundary extension parameters
	call georef (geo, Memr[xsample], GT_NCOLS(geo), Memr[ysample],
	    GT_NLINES(geo), gsgetr (sx1, GSXMIN), gsgetr (sx1, GSXMAX),
	    gsgetr (sx1, GSYMIN), gsgetr (sx1, GSYMAX))
	call geoimset (input, geo, sx1, sy1, sx2, sy2, Memr[xsample],
	    GT_NCOLS(geo), Memr[ysample], GT_NLINES(geo))

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

	# loop over the line blocks
	for (l1 = 1; l1 <= GT_NLINES(geo); l1 = l1 + nyblock) {

	    # l1 and l2 are the line limits in the output image
	    l2 = min (l1 + nyblock - 1, GT_NLINES(geo)) 
	    nlines = l2 - l1 + 1

	    # line1 and line2 are the coordinates in the interpolation surface
	    line1 = max (1, min (nysample - 1, int (Memr[yinterp+l1-1])))
	    line2 = min (nysample, int (Memr[yinterp+l2-1] + 1.0))

	    if ((xbuf == NULL) || (ybuf == NULL) || (jbuf == NULL) ||
	        (line1 < llast1) || (line2 > llast2)) {
		call geobuffer (sx1, sx2, xmsi, Memr[xsample], Memr[ysample], 1,
		    nxsample, line1, line2, llast1, llast2, xbuf)
		call geobuffer (sy1, sy2, ymsi, Memr[xsample], Memr[ysample], 1,
		    nxsample, line1, line2, llast1, llast2, ybuf)
	        if (GT_FLUXCONSERVE(geo) == YES)
		    call geojbuffer (sx1, sy1, sx2, sy2, jmsi, Memr[xsample],
		        Memr[ysample], 1, nxsample, line1, line2, llast1,
			llast2, jbuf)
		llast1 = line1
		llast2 = line2
	    }


	    # loop over the column blocks
	    for (c1 = 1; c1 <= GT_NCOLS(geo); c1 = c1 + nxblock) {

		# c1 and c2 are the column limits in the output image
		c2 = min (c1 + nxblock - 1, GT_NCOLS(geo))
		ncols = c2 - c1 + 1

		# calculate the coordinates of the output pixels in the input
		# image
		call geomsivector (input, output, geo, xmsi, ymsi, jmsi, msi, 
		    sx1, sy1, sx2, sy2, Memr[xinterp], Memr[yinterp], c1, c2,
		    l1, l2, 1, line1)
	    }
	}

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

# GEOREF -- Procedure to determine the x and y coordinates at which
# the coordinate surface will be subsampled

procedure georef (geo, x, nx, y, ny, xmin, xmax, ymin, ymax)

pointer	geo		# pointer to the geotran structure
real	x[nx]		# output x sample coordinates
int	nx		# size of sampled x array
real	y[ny]		# output y sample coordinates
int	ny		# number of output y coordinates
real	xmin, xmax	# limits on x coordinates
real	ymin, ymax	# limits on y coordinates

int	i
real	dx, dy

begin

	dx = (GT_XMAX(geo) - GT_XMIN(geo)) / (nx - 1)
	do  i = 1, nx
	    x[i] = min (xmax, max (xmin, GT_XMIN(geo) + (i - 1) * dx))

	dy = (GT_YMAX(geo) - GT_YMIN(geo)) / (ny  - 1)
	do i = 1, ny
	    y[i] = min (ymax, max (ymin, GT_YMIN(geo) + (i - 1) * dy))
end

# GEOSAMPLE -- Procedure to calculate the sampled reference points

procedure geosample (geo, xref, yref, nxsample, nysample)

pointer	geo		# pointer to geotran structure
real	xref[ARB]	# x reference values
real	yref[ARB]	# y reference values
int	nxsample	# number of sample points in x
int	nysample	# number of sample points in y

int	i

begin
	do i = 1, GT_NCOLS(geo)
	    xref[i] = min (real (nxsample), max (1., real ((nxsample - 1) * i +
	        (GT_NCOLS(geo) - nxsample)) / (GT_NCOLS(geo) - 1)))

	do i = 1, GT_NLINES(geo)
	    yref[i] = min (real (nysample), max (1., real ((nysample - 1) * i +
	        (GT_NLINES(geo) - nysample)) / (GT_NLINES(geo) - 1)))
end

# GEOBUFFER -- Procedure to fit the surface interpolants

procedure geobuffer (s1, s2,  msi, xsample, ysample, c1, c2, l1, l2,
	last1, last2, buf)

pointer	s1, s2		# pointers to the linear surface
pointer	msi		# interpolant
real	xsample[ARB]	# sampled x reference coordinates
real	ysample[ARB]	# sampled y reference coordinates
int	c1, c2		# columns of interest in sampled image
int	l1, l2		# lines of interest in the sampled image
int	last1, last2	# pointer to previous lines
pointer	buf		# pointer to output buffer

int	i, ncols, nlines, llast1, llast2, nclast, nllast
pointer	sp, sf, y, z, buf1, buf2

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# combine surfaces
	if (s2 == NULL)
	    call gscopy (s1, sf)
	else
	    call gsadd (s1, s2, sf)

	# allocate workin space
	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (z, ncols, TY_REAL)

	# if buffer undefined then allocate memory for the buffer. Reallocate
	# the buffer if the number of lines or columns changes.
	if (buf ==  NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	} else {
	    llast1 = last1
	    llast2 = last2
	}

	if (l1 < llast1) {
	    do i = l2, l1, -1 {
		if (i > llast1)
		    buf1 = buf + (i - llast1) * ncols
		else {
		    buf1 = z
		    call amovkr (ysample[i], Memr[y], ncols)
		    call gsvector (sf, xsample[c1], Memr[y], Memr[buf1], ncols)
		}
		buf2 = buf + (i - l1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	} else if (l2 > llast2) {
	    do i = l1, l2 {
		if (i < llast2)
		    buf1 = buf + (i - llast1) * ncols
		else {
		    buf1 = z
		    call amovkr (ysample[i], Memr[y], ncols)
		    call gsvector (sf, xsample[c1], Memr[y], Memr[buf1], ncols)
		}
		buf2 = buf + (i - l1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	}

	llast1 = l1
	llast2 = l2
	nclast = ncols
	nllast = nlines

	# fit the interpolant
	call msifit (msi, Memr[buf], ncols, nlines, ncols)

	call gsfree (sf)
	call sfree (sp)
end

# GEOJBUFFER -- Procedure to fit the surface interpolants

procedure geojbuffer (sx1, sy1, sx2, sy2, jmsi, xsample, ysample, c1, c2, l1,
    l2, last1, last2, jbuf)

pointer	sx1, sy1	# pointer to the linear surface
pointer	sx2, sy2	# pointer to the distortion surface
pointer	jmsi		# interpolant
real	xsample[ARB]	# sampled x reference coordinates
real	ysample[ARB]	# sampled y reference coordinates
int	c1, c2		# columns of interest in sampled image
int	l1, l2		# lines of interest in the sampled image
int	last1, last2	# previous line numbers
pointer	jbuf		# pointer to output buffer

int	i, ncols, nlines, llast1, llast2, nclast, nllast
pointer	sp, sx, sy, y, z, buf1, buf2

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# combine surfaces
	if (sx2 == NULL)
	    call gscopy (sx1, sx)
	else
	    call gsadd (sx1, sx2, sx)
	if (sy2 == NULL)
	    call gscopy (sy1, sy)
	else
	    call gsadd (sy1, sy2, sy)

	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (z, ncols, TY_REAL)

	# if buffer undefined then allocate memory for the buffer. Reallocate
	# the buffer if the number of lines or columns changes.
	if (jbuf ==  NULL) {
	    call malloc (jbuf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (jbuf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	} else {
	    llast1 = last1
	    llast2 = last2
	}

	if (l1 < llast1) {
	    do i = l2, l1, -1 {
		if (i > llast1)
		    buf1 = jbuf + (i - llast1) * ncols
		else {
		    buf1 = z
		    call amovkr (ysample[i], Memr[y], ncols)
		    call jgsvector (sx, sy, xsample[c1], Memr[y], Memr[buf1],
		        ncols)
		}
		buf2 = jbuf + (i - l1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	} else if (l2 > llast2) {
	    do i = l1, l2 {
		if (i < llast2)
		    buf1 = jbuf + (i - llast1) * ncols
		else {
		    buf1 = z
		    call amovkr (ysample[i], Memr[y], ncols)
		    call jgsvector (sx, sy, xsample[c1], Memr[y], Memr[buf1],
		        ncols)
		}
		buf2 = jbuf + (i - l1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	}

	# update buffer pointers
	llast1 = l1
	llast2 = l2
	nclast = ncols
	nllast = nlines

	# fit the interpolant
	call msifit (jmsi, Memr[jbuf], ncols, nlines, ncols)

	call gsfree (sx)
	call gsfree (sy)
	call sfree (sp)
end

# JGSVECTOR -- Procedure to compute the Jacobian of the transformation

procedure jgsvector (sx, sy, x, y, out, ncols)

pointer	sx, sy		# surface descriptors
real	x[ARB]		# x values
real	y[ARB]		# y values
real	out[ARB]	# output values
int	ncols		# number of points

pointer	sp, der1, der2

begin
	call smark (sp)
	call salloc (der1, ncols, TY_REAL)
	call salloc (der2, ncols, TY_REAL)

	call gsder (sx, x, y, Memr[der1], ncols, 1, 0)
	call gsder (sy, x, y, Memr[der2], ncols, 0, 1)
	call amulr (Memr[der1], Memr[der2], out, ncols)
	call gsder (sx, x, y, Memr[der1], ncols, 0, 1)
	call gsder (sy, x, y, Memr[der2], ncols, 1, 0)
	call amulr (Memr[der1], Memr[der2], Memr[der1], ncols)
	call asubr (out, Memr[der1], out, ncols)

	call sfree (sp)
end

# GEOMSIVECTOR -- Procedure to interpolate the surface coordinates

procedure geomsivector (in, out, geo, xmsi, ymsi, jmsi, msi, sx1, sy1, sx2,
    sy2, xref, yref, c1, c2, l1, l2, x0, y0)

pointer	in		# pointer to input image
pointer	out		# pointer to output image
pointer	geo		# pointer to geotran structure
pointer	xmsi, ymsi	# pointer to the interpolation cord surfaces
pointer	jmsi		# pointer to Jacobian surface
pointer	msi		# pointer to interpolation surface
pointer	sx1, sy1	# pointers to linear surfaces
pointer	sx2, sy2	# pointer to higher order surfaces
real	xref[ARB]	# x reference coordinates
real	yref[ARB]	# y reference coordinates
int	c1, c2		# column limits in output image
int	l1, l2		# line limits in output image
int	x0, y0		# zero points of interpolation coordinates

int	j, ncols, nlines, imc1, imc2, iml1, iml2, nicols
pointer	sp, x, y, xin, yin, inbuf, outbuf 
real	xmin, xmax, ymin, ymax, factor

pointer	imgs2r(), imps2r()
real	jfactor()

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	call smark (sp)
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)
	call salloc (xin, ncols, TY_REAL)
	call salloc (yin, ncols, TY_REAL)

	# find min max of interpolation coords
	call geoiminmax (xref, yref, c1, c2, l1, l2, x0, y0, xmsi, ymsi, xmin,
	    xmax, ymin, ymax)

	# get the appropriate image section
	imc1 = xmin - NMARGIN
	if (imc1 < 0)
	    imc1 = imc1 - 1
	imc2 = int (xmax) + NMARGIN + 1
	nicols = imc2 - imc1 + 1
	iml1 = ymin - NMARGIN
	if (iml1 < 0)
	    iml1 = iml1 - 1
	iml2 = int (ymax) + NMARGIN + 1

	# fit the interpolant
	inbuf = imgs2r (in, imc1, imc2, iml1, iml2)
	if (inbuf == EOF)
	    call error (0, "Error reading image")
	call msifit (msi, Memr[inbuf], (imc2 - imc1 + 1), (iml2 - iml1 + 1),
	    (imc2 - imc1 + 1))

	# compute the output buffer
	call aaddkr (xref[c1], real (-x0 + 1), Memr[x], ncols)
	do j = l1, l2 {

	    # compute coordinates
	    call amovkr (yref[j] + real (-y0 + 1), Memr[y], ncols)
	    call msivector (xmsi, Memr[x], Memr[y], Memr[xin], ncols)
	    if (imc1 != 1)
		call aaddkr (Memr[xin], real (-imc1 + 1), Memr[xin], ncols)
	    call msivector (ymsi, Memr[x], Memr[y], Memr[yin], ncols)
	    if (iml1 != 1)
		call aaddkr (Memr[yin], real (-iml1 + 1), Memr[yin], ncols)

	    # write to output image
	    outbuf = imps2r (out, c1, c2, j, j)
	    if (outbuf == EOF)
		call error (0, "Error writing output image")
	    call msivector (msi, Memr[xin], Memr[yin], Memr[outbuf], ncols)

	    # preserve flux in image
	    if (GT_FLUXCONSERVE(geo) == YES) {
		factor = GT_XSCALE(geo) * GT_YSCALE(geo)
		if (GT_GEOMODE(geo) == GT_LINEAR || (sx2 == NULL && sy2 ==
		    NULL))
		    call amulkr (Memr[outbuf], factor * jfactor (sx1, sy1),
		        Memr[outbuf], ncols)
		else {
		    call geomsiflux (jmsi, xref, yref, Memr[outbuf], c1, c2,
		        j, x0, y0)
		    call amulkr (Memr[outbuf], factor, Memr[outbuf], ncols)
		}
	    }
	}

	call sfree (sp)
end

# GEOGSVECTOR -- Procedure to evaluate the surface without sampling

procedure geogsvector (input, output, geo, msi, xref, yref, c1, c2, l1, l2,
    sx1, sy1, sx2, sy2)

pointer	input		# pointer to input image
pointer	output		# pointer to output image
pointer	geo		# pointer to geotran structure
pointer	msi		# pointer to interpolant
real	xref[ARB]	# x reference array
real	yref[ARB]	# y reference array
int	c1, c2		# columns of interest in output image
int	l1, l2		# lines of interest in the output image
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to distortion surface

int	j, ncols, nicols, nlines, imc1, imc2, iml1, iml2
pointer	sp, y, xin, yin, temp, inbuf, outbuf
real	xmin, xmax, ymin, ymax, factor

pointer	imgs2r(), imps2r()
real	jfactor()

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (xin, ncols, TY_REAL)
	call salloc (yin, ncols, TY_REAL)
	call salloc (temp, ncols, TY_REAL)

	# compute the maximum and minimum
	call geominmax (xref, yref, c1, c2, l1, l2, sx1, sy1, sx2, sy2, xmin,
	    xmax, ymin, ymax)

	# get the appropriate image section
	imc1 = xmin - NMARGIN
	if (imc1 < 0)
	    imc1 = imc1 - 1
	imc2 = int (xmax) + NMARGIN + 1
	iml1 = ymin - NMARGIN
	if (iml1 < 0)
	    iml1 = iml1 - 1
	iml2 = int (ymax) + NMARGIN + 1
	nicols = imc2 - imc1 + 1

	# fill buffer
	inbuf = imgs2r (input, imc1, imc2, iml1, iml2)
	if (inbuf == EOF)
	    call error (0, "Error reading image")

	# fit the interpolant
	call msifit (msi, Memr[inbuf], (imc2 - imc1 + 1), (iml2 - iml1 + 1),
	    (imc2 - imc1 + 1))

	# calculate the  x and y input image coordinates
	do j = l1, l2 {

	    # get output image buffer
	    outbuf = imps2r (output, c1, c2, j, j)
	    if (output == EOF)
		call error (0, "Error writing output image")

	    call amovkr (yref[j], Memr[y], ncols)
	    # fit x coords
	    call gsvector (sx1, xref[c1], Memr[y], Memr[xin], ncols)
	    if (sx2 != NULL) {
		call gsvector (sx2, xref[c1], Memr[y], Memr[temp], ncols)
		call aaddr (Memr[xin], Memr[temp], Memr[xin], ncols)
	    }
	    if (imc1 != 1)
		call aaddkr (Memr[xin], real (-imc1 + 1), Memr[xin], ncols)

	    # fit ycoords
	    call gsvector (sy1, xref[c1], Memr[y], Memr[yin], ncols)
	    if (sy2 != NULL) {
		call gsvector (sy2, xref[c1], Memr[y], Memr[temp], ncols)
		call aaddr (Memr[yin], Memr[temp], Memr[yin], ncols)
	    }
	    if (iml1 != 1)
		call aaddkr (Memr[yin], real (-iml1 + 1), Memr[yin], ncols)

	    # interpolate in input image
	    call msivector (msi, Memr[xin], Memr[yin], Memr[outbuf], ncols)

	    # preserve flux in image
	    if (GT_FLUXCONSERVE(geo) == YES) {
		factor = GT_XSCALE(geo) * GT_YSCALE(geo)
		if (GT_GEOMODE(geo) == GT_LINEAR || (sx2 == NULL && sy2 ==
		    NULL))
		    call amulkr (Memr[outbuf], factor * jfactor (sx1, sy1),
		        Memr[outbuf], ncols)
		else {
		    call geogsflux (xref, yref, Memr[outbuf], c1, c2, j,
		        sx1, sy1, sx2, sy2)
		    call amulkr (Memr[outbuf], factor, Memr[outbuf], ncols)
		}
	    }
	}

	call sfree (sp)
end


# GEOIMINMAX -- Procedure to find minmax interpolation coords

procedure geoiminmax (xref, yref, c1, c2, l1, l2, x0, y0, xmsi, ymsi, xmin,
    xmax, ymin, ymax)

real	xref[ARB]		# x reference coords
real	yref[ARB]		# y reference coords
int	c1, c2			# columns limits
int	l1, l2			# line limits
int	x0, y0			# interpolation coord zero points
pointer	xmsi, ymsi		# coord surfaces
real	xmin, xmax		# output xmin and xmax
real	ymin, ymax		# output ymin and ymax

int	j, ncols
pointer	sp, x, y, xin, yin
real	mintemp, maxtemp, x1, x2, y1, y2

real	msieval()

begin
	call smark (sp)
	ncols = c2 - c1 + 1
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)
	call salloc (xin, ncols, TY_REAL)
	call salloc (yin, ncols, TY_REAL)

	xmin = MAX_REAL
	xmax = -MAX_REAL
	ymin = MAX_REAL
	ymax = -MAX_REAL

	# find the minimum and maximum
	do j = l1, l2 {

	    if (j == l1 || j == l2) {
	        call amovkr (yref[j] + real (-y0 + 1), Memr[y], ncols)
		call aaddkr (xref[c1], real (-x0 + 1), Memr[x], ncols)
		call msivector (xmsi, Memr[x], Memr[y], Memr[xin], ncols)
		call msivector (ymsi, Memr[x], Memr[y], Memr[yin], ncols)
		call alimr (Memr[xin], ncols, mintemp, maxtemp)
		xmin = min (xmin, mintemp)
		xmax = max (xmax, maxtemp)
		call alimr (Memr[yin], ncols, mintemp, maxtemp)
		ymin = min (ymin, mintemp)
		ymax = max (ymax, maxtemp)
	    } else {
		x1 = msieval (xmsi, xref[c1] + real (-x0 + 1),
		    yref[j] + real (-y0 + 1))
		x2 = msieval (xmsi, xref[c1+ncols-1] + real (-x0 + 1),
		    yref[j] + real (-y0 + 1))
		xmin = min (xmin, x1, x2)
		xmax = max (xmax, x1, x2)

		y1 = msieval (ymsi, xref[c1] + real (-x0 + 1),
		    yref[j]  + real (-y0 + 1))
		y2 = msieval (ymsi, xref[c1+ncols-1] + real (-x0 + 1),
		    yref[j] + real (-y0 + 1))
		ymin = min (ymin, y1, y2)
		ymax = max (ymax, y1, y2)
	    }
	}

	call sfree (sp)

end

# GEOMINMAX -- Procedure to compute the min and max

procedure geominmax (xref, yref, c1, c2, l1, l2, sx1, sy1, sx2, sy2, xmin, xmax,
    ymin, ymax)

real	xref[ARB]		# x reference coords
real	yref[ARB]		# y reference coords
int	c1, c2			# columns limits
int	l1, l2			# line limits
pointer	sx1, sy1		# linear surface
pointer	sx2, sy2		# distortion surface
real	xmin, xmax		# output xmin and xmax
real	ymin, ymax		# output ymin and ymax

int	j, ncols
pointer	sp, y, xin, yin, temp
real	x1, x2, y1, y2, mintemp, maxtemp

real	gseval()

begin
	call smark (sp)
	ncols = c2 - c1 + 1
	call salloc (y, ncols, TY_REAL)
	call salloc (xin, ncols, TY_REAL)
	call salloc (yin, ncols, TY_REAL)
	call salloc (temp, ncols, TY_REAL)

	xmin = MAX_REAL
	xmax = -MAX_REAL
	ymin = MAX_REAL
	ymax = -MAX_REAL

	# find the maximum and minimum
	do j = l1, l2 {

	    if (j == l1 || j == l2) {

	        call amovkr (yref[j], Memr[y], ncols)
	        call gsvector (sx1, xref[c1], Memr[y], Memr[xin], ncols)
	        if (sx2 != NULL) {
		    call gsvector (sx2, xref[c1], Memr[y], Memr[temp], ncols)
		    call aaddr (Memr[xin], Memr[temp], Memr[xin], ncols)
	        }
	        call gsvector (sy1, xref[c1], Memr[y], Memr[yin], ncols)
	        if (sy2 != NULL) {
		    call gsvector (sy2, xref[c1], Memr[y], Memr[temp], ncols)
		    call aaddr (Memr[yin], Memr[temp], Memr[yin], ncols)
		}

		call alimr (Memr[xin], ncols, mintemp, maxtemp)
		xmin = min (xmin, mintemp)
		xmax = max (xmax, maxtemp)
		call alimr (Memr[yin], ncols, mintemp, maxtemp)
		ymin = min (ymin, mintemp)
		ymax = max (ymax, maxtemp)

	    } else {

		x1 = gseval (sx1, xref[c1], yref[j])
		x2 = gseval (sx1, xref[c1+ncols-1], yref[j])
		if (sx2 != NULL) {
		    x1 = x1 + gseval (sx2, xref[c1], yref[j])
		    x2 = x2 + gseval (sx2, xref[c1+ncols-1], yref[j])
		}
		xmin = min (xmin, x1, x2)
		xmax = max (xmax, x1, x2)

		y1 = gseval (sy1, xref[c1], yref[j])
		y2 = gseval (sy1, xref[c1+ncols-1], yref[j])
		if (sy2 != NULL) {
		    y1 = y1 + gseval (sy2, xref[c1], yref[j])
		    y2 = y2 + gseval (sy2, xref[c1+ncols-1], yref[j])
		}
		ymin = min (ymin, y1, y2)
		ymax = max (ymax, y1, y2)

	    }
	}

	call sfree (sp)
end

# GEOIMSET -- Procedure to set up input image boundary conditions

procedure geoimset (im, geo, sx1, sy1, sx2, sy2, xref, nx, yref, ny)

pointer	im		# pointer to image
pointer	geo		# pointer to geotran structure
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to distortion surface
real	xref[ARB]	# x reference coordinates
int	nx		# number of x reference coordinates
real	yref[ARB]	# y reference coordinates
int	ny		# number of y reference coordinates

int	bndry, npts
pointer	sp, x1, x2, y1, y2, xtemp, ytemp
real	xn1, xn2, xn3, xn4, yn1, yn2, yn3, yn4, xmin, xmax, ymin, ymax
real	gseval()

begin
	npts = max (nx, ny)

	xn1 = gseval (sx1, GT_XMIN(geo), GT_YMIN(geo))
	xn2 = gseval (sx1, GT_XMAX(geo), GT_YMIN(geo))
	xn3 = gseval (sx1, GT_XMAX(geo), GT_YMAX(geo))
	xn4 = gseval (sx1, GT_XMIN(geo), GT_YMAX(geo))
	xmin = min (xn1, xn2, xn3, xn4)
	xmax = max (xn1, xn2, xn3, xn4)

	yn1 = gseval (sy1, GT_XMIN(geo), GT_YMIN(geo))
	yn2 = gseval (sy1, GT_XMAX(geo), GT_YMIN(geo))
	yn3 = gseval (sy1, GT_XMAX(geo), GT_YMAX(geo))
	yn4 = gseval (sy1, GT_XMIN(geo), GT_YMAX(geo))
	ymin = min (yn1, yn2, yn3, yn4)
	ymax = max (yn1, yn2, yn3, yn4)

	if (sx2 != NULL) {

	    call smark (sp)
	    call salloc (x1, npts, TY_REAL)
	    call salloc (x2, npts, TY_REAL)
	    call salloc (xtemp, npts, TY_REAL)
	    call salloc (ytemp, npts, TY_REAL)

	    call amovkr (GT_YMIN(geo), Memr[ytemp], nx) 
	    call gsvector (sx1, xref, Memr[ytemp], Memr[x1], nx)
	    call gsvector (sx2, xref, Memr[ytemp], Memr[x2], nx)
	    call aaddr (Memr[x1], Memr[x2], Memr[x1], nx)
	    call alimr (Memr[x1], nx, xn1, yn1)

	    call amovkr (GT_XMAX(geo), Memr[xtemp], ny) 
	    call gsvector (sx1, Memr[xtemp], yref, Memr[x1], ny)
	    call gsvector (sx2, Memr[xtemp], yref, Memr[x2], ny)
	    call aaddr (Memr[x1], Memr[x2], Memr[x1], ny)
	    call alimr (Memr[x1], ny, xn2, yn2)

	    call amovkr (GT_YMAX(geo), Memr[ytemp], nx) 
	    call gsvector (sx1, xref, Memr[ytemp], Memr[x1], nx)
	    call gsvector (sx2, xref, Memr[ytemp], Memr[x2], nx)
	    call aaddr (Memr[x1], Memr[x2], Memr[x1], nx)
	    call alimr (Memr[x1], nx, xn3, yn3)

	    call amovkr (GT_XMIN(geo), Memr[xtemp], ny) 
	    call gsvector (sx1, Memr[xtemp], yref, Memr[x1], ny)
	    call gsvector (sx2, Memr[xtemp], yref, Memr[x2], ny)
	    call aaddr (Memr[x1], Memr[x2], Memr[x1], ny)
	    call alimr (Memr[x1], ny, xn4, yn4)

	    xmin = min (xn1, xn2, xn3, xn4)
	    xmax = max (yn1, yn2, yn3, yn4)

	    call sfree (sp)
	}

	if (sy2 != NULL) {

	    call smark (sp)
	    call salloc (y1, npts, TY_REAL)
	    call salloc (y2, npts, TY_REAL)
	    call salloc (xtemp, npts, TY_REAL)
	    call salloc (ytemp, npts, TY_REAL)

	    call amovkr (GT_YMIN(geo), Memr[ytemp], nx) 
	    call gsvector (sy1, xref, Memr[ytemp], Memr[y1], nx)
	    call gsvector (sy2, xref, Memr[ytemp], Memr[y2], nx)
	    call aaddr (Memr[y1], Memr[y2], Memr[y1], nx)
	    call alimr (Memr[y1], nx, xn1, yn1)

	    call amovkr (GT_XMAX(geo), Memr[xtemp], ny) 
	    call gsvector (sy1, Memr[xtemp], yref, Memr[y1], ny)
	    call gsvector (sy2, Memr[xtemp], yref, Memr[y2], ny)
	    call aaddr (Memr[y1], Memr[y2], Memr[y1], ny)
	    call alimr (Memr[y1], ny, xn2, yn2)

	    call amovkr (GT_YMAX(geo), Memr[ytemp], nx) 
	    call gsvector (sy1, xref, Memr[ytemp], Memr[y1], nx)
	    call gsvector (sy2, xref, Memr[ytemp], Memr[y2], nx)
	    call aaddr (Memr[y1], Memr[y2], Memr[y1], nx)
	    call alimr (Memr[y1], nx, xn3, yn3)

	    call amovkr (GT_XMIN(geo), Memr[xtemp], ny) 
	    call gsvector (sy1, Memr[xtemp], yref, Memr[y1], ny)
	    call gsvector (sy2, Memr[xtemp], yref, Memr[y2], ny)
	    call aaddr (Memr[y1], Memr[y2], Memr[y1], ny)
	    call alimr (Memr[y1], ny, xn4, yn4)

	    ymin = min (xn1, xn2, xn3, xn4)
	    ymax = max (yn1, yn2, yn3, yn4)

	    call sfree (sp)
	}

	if (xmin < 1.0 || ymin < 1.0 || xmax > real (IM_LEN(im,1)) ||
	    ymax > real (IM_LEN(im,2)))
	    bndry = max (1.0 - xmin, 1.0 - ymin,
	        xmax - IM_LEN(im,1), ymax - IM_LEN(im,2)) + 1
	else
	    bndry = 1

	call imseti (im, IM_NBNDRYPIX, bndry + NMARGIN)
	call imseti (im, IM_TYBNDRY, GT_BOUNDARY(geo))
	call imsetr (im, IM_BNDRYPIXVAL, GT_CONSTANT(geo))
end

# GEOGSFLUX -- Procedure to preserve flux after a transformation

procedure geogsflux (xref, yref, buf, c1, c2, line, sx1, sy1, sx2, sy2)

real	xref[ARB]	# pointer to x reference coordinates
real	yref[ARB]	# pointer to y reference coordinates
real	buf[ARB]	# output image buffer
int	c1, c2		# column limits in the output image
int	line		# line limits in the output image
pointer	sx1, sy1	# pointer to linear surface
pointer	sx2, sy2	# pointer to distortion surface

int	ncols
pointer	sp, y, der1, der2, jacob, sx, sy

begin
	ncols = c2 - c1 + 1

	# get the reference coordinates
	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (der1, ncols, TY_REAL)
	call salloc (der2, ncols, TY_REAL)
	call salloc (jacob, ncols, TY_REAL)

	# add the two surfaces together for efficiency
	if (sx2 != NULL)
	    call gsadd (sx1, sx2, sx)
	else
	    call gscopy (sx1, sx)
	if (sy2 != NULL)
	    call gsadd (sy1, sy2, sy)
	else
	    call gscopy (sy1, sy)

	# multiply the output buffer by the Jacobian
	call amovkr (yref[line], Memr[y], ncols)
	call gsder (sx, xref[c1], Memr[y], Memr[der1], ncols, 1, 0)
	call gsder (sy, xref[c1], Memr[y], Memr[der2], ncols, 0, 1)
	call amulr (Memr[der1], Memr[der2], Memr[jacob], ncols)
	call gsder (sx, xref[c1], Memr[y], Memr[der1], ncols, 0, 1) 
	call gsder (sy, xref[c1], Memr[y], Memr[der2], ncols, 1, 0)
	call amulr (Memr[der1], Memr[der2], Memr[der1], ncols)
	call asubr (Memr[jacob], Memr[der1], Memr[jacob], ncols)
	call aabsr (Memr[jacob], Memr[jacob], ncols)
	call amulr (buf, Memr[jacob], buf, ncols)

	# clean up
	call gsfree (sx)
	call gsfree (sy)
	call sfree (sp)
end

# GEOMSIFLUX -- Procedure to interpolate the surface coordinates

procedure geomsiflux (jmsi, xinterp, yinterp, outdata, c1, c2, line, x0, y0)

pointer	jmsi	        # pointer to the jacobian interpolant
real	xinterp[ARB]	# x reference coordinates
real	yinterp[ARB]	# y reference coordinates
real	outdata[ARB]	# output data
int	c1, c2		# column limits in output image
int	line		# line to be flux corrected
int	x0, y0		# zero points of interpolation coordinates

int	ncols
pointer	sp, x, y, jacob

begin
	# allocate tempoaray sapce
	call smark (sp)
	ncols = c2 - c1 + 1
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)
	call salloc (jacob, ncols, TY_REAL)

	# calculate the x points
	if (x0 == 1)
	    call amovr (xinterp[c1], Memr[x], ncols)
	else
	    call aaddkr (xinterp[c1], real (-x0 + 1), Memr[x], ncols)

	# multiply the data by the Jacobian
	call amovkr ((yinterp[line] + real (-y0 + 1)), Memr[y], ncols)
	call msivector (jmsi, Memr[x], Memr[y], Memr[jacob], ncols)
	call aabsr (Memr[jacob], Memr[jacob], ncols)
	call amulr (outdata, Memr[jacob], outdata, ncols)

	call sfree (sp)
end

# JFACTOR -- Jacobian of a linear transformation

real procedure jfactor (sx1, sy1)

pointer	sx1		# pointer to x surface
pointer	sy1		# pointer to y surface

real	xval, yval, xx, xy, yx, yy

real	gsgetr()

begin
	xval = (gsgetr (sx1, GSXMIN) + gsgetr (sx1, GSXMAX)) / 2.0
	yval = (gsgetr (sy1, GSYMIN) + gsgetr (sy1, GSYMIN)) / 2.0 

	call gsder (sx1, xval, yval, xx, 1, 1, 0)
	call gsder (sx1, xval, yval, xy, 1, 0, 1)
	call gsder (sy1, xval, yval, yx, 1, 1, 0)
	call gsder (sy1, xval, yval, yy, 1, 0, 1)

	return (abs (xx * yy - xy * yx))
end
