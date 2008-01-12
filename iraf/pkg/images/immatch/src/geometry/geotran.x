# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imset.h>
include <mach.h>
include <math/gsurfit.h>
include <math/iminterp.h>
include "geotran.h"

define	NMARGIN		3	# number of boundary pixels
define	NMARGIN_SPLINE3	16	# number of spline boundary pixels

# GEO_TRAN -- Correct an image for geometric distortion block by block using
# fitted coordinates and image interpolation.

procedure geo_tran (input, output, geo, sx1, sy1, sx2, sy2, nxblock, nyblock)

pointer	input			#I pointer to input image
pointer	output			#I pointer to output image
pointer	geo			#I pointer to geotran structure
pointer	sx1, sy1		#I pointers to linear surfaces
pointer	sx2, sy2		#I pointers to higher order surfaces
int	nxblock, nyblock	#I working block size

int	l1, l2, c1, c2, nincr
pointer	sp, xref, yref, msi
real	shift
real	gsgetr()

begin
	# Initialize the interpolant.
	if (IM_NDIM(input) == 1) {
	    call asitype (GT_INTERPSTR(geo), GT_INTERPOLANT(geo), GT_NSINC(geo),
	        nincr, shift)
	    call asisinit (msi, GT_INTERPOLANT(geo), GT_NSINC(geo), nincr,
	        shift, 0.0)
	} else {
	    call msitype (GT_INTERPSTR(geo), GT_INTERPOLANT(geo),
	        GT_NSINC(geo), nincr, shift)
	    call msisinit (msi, GT_INTERPOLANT(geo), GT_NSINC(geo), nincr,
	        nincr, shift, shift, 0.0)
	}
	call geo_margset (sx1, sy1, sx2, sy2, GT_XMIN(geo), GT_XMAX(geo),
	    GT_NCOLS(geo), GT_YMIN(geo), GT_YMAX(geo), GT_NLINES(geo),
	    GT_INTERPOLANT(geo), GT_NSINC(geo),  GT_NXYMARGIN(geo))

	# Allocate working space.
	call smark (sp)
	call salloc (xref, GT_NCOLS(geo), TY_REAL)
	call salloc (yref, GT_NLINES(geo), TY_REAL)

	# Compute the reference coordinates corresponding to the center of
	# the output image pixels.
	call geo_ref (geo, Memr[xref], 1, GT_NCOLS(geo), GT_NCOLS(geo),
	    Memr[yref], 1, GT_NLINES(geo), GT_NLINES(geo), gsgetr (sx1,
	    GSXMIN), gsgetr (sx1, GSXMAX), gsgetr (sx1, GSYMIN), gsgetr (sx1,
	    GSYMAX), GT_ONE)

	# Configure the out-of-bounds pixel references for the input image.
	call geo_imset (input, geo, sx1, sy1, sx2, sy2, Memr[xref],
	        GT_NCOLS(geo), Memr[yref], GT_NLINES(geo))

	# Loop over the line blocks.
	for (l1 = 1; l1 <= GT_NLINES(geo); l1 = l1 + nyblock) {

	    # Set line limits in the output image.
	    l2 = min (l1 + nyblock - 1, GT_NLINES(geo)) 

	    # Loop over the column blocks
	    for (c1 = 1; c1 <= GT_NCOLS(geo); c1 = c1 + nxblock) {

		# Set column limits in the output image.
		c2 = min (c1 + nxblock - 1, GT_NCOLS(geo))

		# Interpolate
		call geo_gsvector (input, output, geo, msi, Memr[xref],
		    c1, c2, Memr[yref], l1, l2, sx1, sy1, sx2, sy2)
	    }
	}

	# Clean up.
	if (IM_NDIM(input) == 1)
	    call asifree (msi)
	else
	    call msifree (msi)
	call sfree (sp)
end


# GEO_STRAN -- Correct an image for geometric distortion block by block using
# interpolated coordinates and image interpolation.

procedure geo_stran (input, output, geo, sx1, sy1, sx2, sy2, nxblock, nyblock)

pointer	input			#I pointer to input image
pointer	output			#I pointer to output image
pointer	geo			#I pointer to geotran structure
pointer	sx1, sy1		#I pointers to linear surfaces
pointer	sx2, sy2		#I pointers to higher order surfaces
int	nxblock, nyblock	#I working block size

int	nxsample, nysample, ncols, nlines, l1, l2, c1, c2
int	line1, line2, llast1, llast2, nincr
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

	# Compute the sample size.
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
	    call asisinit (msi, GT_INTERPOLANT(geo), GT_NSINC(geo), nincr,
	        shift, 0.0)
	    if (GT_FLUXCONSERVE(geo) == YES)
	        call asiinit (jmsi, II_LINEAR)
	} else {
	    call msiinit (xmsi, II_BILINEAR)
	    call msiinit (ymsi, II_BILINEAR)
	    call msitype (GT_INTERPSTR(geo), GT_INTERPOLANT(geo),
	        GT_NSINC(geo), nincr, shift)
	    call msisinit (msi, GT_INTERPOLANT(geo), GT_NSINC(geo), nincr,
	        nincr, shift, shift, 0.0)
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

	# Initialize the buffers.
	xbuf = NULL
	ybuf = NULL
	jbuf = NULL

	# Loop over the line blocks.
	for (l1 = 1; l1 <= GT_NLINES(geo); l1 = l1 + nyblock) {

	    # Set line limits in the output image.
	    l2 = min (l1 + nyblock - 1, GT_NLINES(geo)) 
	    nlines = l2 - l1 + 1

	    # Line1 and line2 are the coordinates in the interpolation surface
	    line1 = max (1, min (nysample - 1, int (Memr[yinterp+l1-1])))
	    line2 = min (nysample, int (Memr[yinterp+l2-1] + 1.0))

	    if ((xbuf == NULL) || (ybuf == NULL) || (jbuf == NULL) ||
	        (line1 < llast1) || (line2 > llast2)) {
		call geo_xbuffer (sx1, sx2, xmsi, Memr[xsample], Memr[ysample],
		    1, nxsample, line1, line2, xbuf)
		call geo_ybuffer (sy1, sy2, ymsi, Memr[xsample], Memr[ysample],
		    1, nxsample, line1, line2, ybuf)
	        if (GT_FLUXCONSERVE(geo) == YES) {
		    if (IM_NDIM(input) == 1)
		        call geo_jbuffer (sx1, NULL, sx2, NULL, jmsi,
			    Memr[xsample], Memr[ysample], 1, nxsample,
			    line1, line2, jbuf)
		    else
		        call geo_jbuffer (sx1, sy1, sx2, sy2, jmsi,
			    Memr[xsample], Memr[ysample], 1, nxsample,
			    line1, line2, jbuf)
		}
		llast1 = line1
		llast2 = line2
	    }


	    # Loop over the column blocks.
	    for (c1 = 1; c1 <= GT_NCOLS(geo); c1 = c1 + nxblock) {

		# C1 and c2 are the column limits in the output image.
		c2 = min (c1 + nxblock - 1, GT_NCOLS(geo))
		ncols = c2 - c1 + 1

		# Calculate the coordinates of the output pixels in the input
		# image.
		call geo_msivector (input, output, geo, xmsi, ymsi, jmsi, msi, 
		    sx1, sy1, sx2, sy2, Memr[xinterp], c1, c2, nxsample,
		    Memr[yinterp], l1, l2, nysample, 1, line1)
	    }
	}

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
	if (GT_FLUXCONSERVE(geo) == YES)
	    call mfree (jbuf, TY_REAL)
	call sfree (sp)
end


# GEO_REF -- Determine the x and y coordinates at which the coordinate
# surface will be subsampled.

procedure geo_ref (geo, x, c1, c2, nx, y, l1, l2, ny, xmin, xmax, ymin, ymax,
	cmode)

pointer	geo		#I pointer to the geotran structure
real	x[ARB]		#O output x sample coordinates
int	c1, c2, nx	#I the column limits of the sampled array
real	y[ARB]		#O output y sample coordinates
int	l1, l2, ny	#I the line limits of the output coordinates
real	xmin, xmax	#I limits on x coordinates
real	ymin, ymax	#I limits on y coordinates
int	cmode		#I coordinate computation mode

int	i
real	xtempmin, xtempmax, ytempmin, ytempmax, dx, dy

begin

	switch (cmode) {
	case GT_FOUR:
	    if (nx == 1) {
	        xtempmin = min (xmax, max (xmin, GT_XMIN(geo)))
	        xtempmax = min (xmax, max (xmin, GT_XMAX(geo)))
		x[1] = xtempmin
		x[2] = xtempmax
		x[3] = xtempmax
		x[4] = xtempmin
	    } else if (nx == GT_NCOLS(geo)) {
	        if (GT_XMIN(geo) > GT_XMAX(geo))
		    dx = -GT_XSCALE(geo)
	        else
		    dx = GT_XSCALE(geo)
	        do  i = c1, c2 {
	            xtempmin = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 1.5) * dx))
	            xtempmax = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 0.5) * dx))
		    x[4*(i-c1)+1] = xtempmin
		    x[4*(i-c1)+2] = xtempmax
		    x[4*(i-c1)+3] = xtempmax
		    x[4*(i-c1)+4] = xtempmin
		}
	    } else {
	        if (GT_XMIN(geo) > GT_XMAX(geo))
		    dx = -GT_XSCALE(geo) * (GT_NCOLS(geo) - 1.0) / (nx - 1.0)
	        else
		    dx = GT_XSCALE(geo) * (GT_NCOLS(geo) - 1.0) / (nx - 1.0)
	        do  i = c1, c2 {
	            xtempmin = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 1.5) * dx))
	            xtempmax = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 0.5) * dx))
		    x[4*(i-c1)+1] = xtempmin
		    x[4*(i-c1)+2] = xtempmax
		    x[4*(i-c1)+3] = xtempmax
		    x[4*(i-c1)+4] = xtempmin 
		}
	    }

	case GT_TWO:
	    if (nx == 1) {
	        xtempmin = min (xmax, max (xmin, GT_XMIN(geo)))
	        xtempmax = min (xmax, max (xmin, GT_XMAX(geo)))
		x[1] = xtempmin
		x[2] = xtempmax
	    } else if (nx == GT_NCOLS(geo)) {
	        if (GT_XMIN(geo) > GT_XMAX(geo))
		    dx = -GT_XSCALE(geo)
	        else
		    dx = GT_XSCALE(geo)
	        do  i = c1, c2 {
	            xtempmin = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 1.5) * dx))
	            xtempmax = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 0.5) * dx))
		    x[2*(i-c1)+1] = xtempmin
		    x[2*(i-c1)+2] = xtempmax
		}
	    } else {
	        if (GT_XMIN(geo) > GT_XMAX(geo))
		    dx = -GT_XSCALE(geo) * (GT_NCOLS(geo) - 1.0) / (nx - 1.0)
	        else
		    dx = GT_XSCALE(geo) * (GT_NCOLS(geo) - 1.0) / (nx - 1.0)
	        do  i = c1, c2 {
	            xtempmin = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 1.5) * dx))
	            xtempmax = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 0.5) * dx))
		    x[2*(i-c1)+1] = xtempmin
		    x[2*(i-c1)+2] = xtempmax
		}
	    }

	case GT_ONE:
	    if (nx == 1) {
	        x[1] = min (xmax, max (xmin,
		    (GT_XMIN(geo) + GT_XMAX(geo)) / 2.0))
	    } else if (nx == GT_NCOLS(geo)) {
	        if (GT_XMIN(geo) > GT_XMAX(geo))
		    dx = -GT_XSCALE(geo)
	        else
		    dx = GT_XSCALE(geo)
	        do  i = c1, c2
	            x[i-c1+1] = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 1) * dx))
	    } else {
	        if (GT_XMIN(geo) > GT_XMAX(geo))
		    dx = -GT_XSCALE(geo) * (GT_NCOLS(geo) - 1.0) / (nx - 1.0)
	        else
		    dx = GT_XSCALE(geo) * (GT_NCOLS(geo) - 1.0) / (nx - 1.0)
	        do  i = c1, c2
	            x[i-c1+1] = min (xmax, max (xmin, GT_XMIN(geo) +
		        (i - 1) * dx))
	    }

	}

	switch (cmode) {
	case GT_FOUR:
	    if (ny == 1) {
	        ytempmin = min (ymax, max (ymin, GT_YMIN(geo)))
	        ytempmax = min (ymax, max (ymin, GT_YMAX(geo)))
		y[1] = ytempmin
		y[2] = ytempmin
		y[3] = ytempmax
		y[4] = ytempmax
	    } else if (ny == GT_NLINES(geo)) {
	        if (GT_YMIN(geo) > GT_YMAX(geo))
		    dy = -GT_YSCALE(geo)
	        else
		    dy = GT_YSCALE(geo)
	        do i = l1, l2 {
	            ytempmin = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 1.5) * dy))
	            ytempmax = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 0.5) * dy))
		    y[4*(i-l1)+1] = ytempmin
		    y[4*(i-l1)+2] = ytempmin
		    y[4*(i-l1)+3] = ytempmax
		    y[4*(i-l1)+4] = ytempmax
		}
	    } else {
	        if (GT_YMIN(geo) > GT_YMAX(geo))
		    dy = -GT_YSCALE(geo) * (GT_NLINES(geo) - 1.0) / (ny - 1.0)
	        else
		    dy = GT_YSCALE(geo) * (GT_NLINES(geo) - 1.0) / (ny - 1.0)
	        do i = l1, l2 {
	            ytempmin = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 1.5) * dy))
	            ytempmax = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 0.5) * dy))
		    y[4*(i-l1)+1] = ytempmin
		    y[4*(i-l1)+2] = ytempmin
		    y[4*(i-l1)+3] = ytempmax
		    y[4*(i-l1)+4] = ytempmax
		}
	    }

	case GT_TWO:
	    if (ny == 1) {
	        ytempmin = min (ymax, max (ymin, GT_YMIN(geo)))
	        ytempmax = min (ymax, max (ymin, GT_YMAX(geo)))
		y[1] = ytempmin
		y[2] = ytempmax
	    } else if (ny == GT_NLINES(geo)) {
	        if (GT_YMIN(geo) > GT_YMAX(geo))
		    dy = -GT_YSCALE(geo)
	        else
		    dy = GT_YSCALE(geo)
	        do i = l1, l2 {
	            ytempmin = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 1.5) * dy))
	            ytempmax = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 0.5) * dy))
		    y[2*(i-l1)+1] = ytempmin
		    y[2*(i-l1)+2] = ytempmax
		}
	    } else {
	        if (GT_YMIN(geo) > GT_YMAX(geo))
		    dy = -GT_YSCALE(geo) * (GT_NLINES(geo) - 1.0) / (ny - 1.0)
	        else
		    dy = GT_YSCALE(geo) * (GT_NLINES(geo) - 1.0) / (ny - 1.0)
	        do i = l1, l2 {
	            ytempmin = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 1.5) * dy))
	            ytempmax = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 0.5) * dy))
		    y[2*(i-l1)+1] = ytempmin
		    y[2*(i-l1)+2] = ytempmax
		}
	    }
	case GT_ONE:
	    if (ny == 1) {
	        y[1] = min (ymax, max (ymin,
		    (GT_YMIN(geo) + GT_YMAX(geo)) / 2.0))
	    } else if (ny == GT_NLINES(geo)) {
	        if (GT_YMIN(geo) > GT_YMAX(geo))
		    dy = -GT_YSCALE(geo)
	        else
		    dy = GT_YSCALE(geo)
	        do i = l1, l2
	            y[i-l1+1] = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 1) * dy))
	    } else {
	        if (GT_YMIN(geo) > GT_YMAX(geo))
		    dy = -GT_YSCALE(geo) * (GT_NLINES(geo) - 1.0) / (ny - 1.0)
	        else
		    dy = GT_YSCALE(geo) * (GT_NLINES(geo) - 1.0) / (ny - 1.0)
	        do i = l1, l2
	            y[i-l1+1] = min (ymax, max (ymin, GT_YMIN(geo) +
		        (i - 1) * dy))
	    }

	}
end


# GEO_SAMPLE -- Calculate the sampled reference points.

procedure geo_sample (geo, xref, c1, c2, nxsample, yref, l1, l2, nysample,
	cmode)

pointer	geo			#I pointer to geotran structure
real	xref[ARB]		#O x reference values
int	c1, c2, nxsample	#I limits and number of sample points in x
real	yref[ARB]		#O y reference values
int	l1, l2, nysample	#I limits and number of sample points in y
int	cmode			#I coordinate computation mode

int	i
real	xtempmin, xtempmax, ytempmin, ytempmax

begin
	switch (cmode) {
	case GT_FOUR:
	    if (GT_NCOLS(geo) == 1) {
	        xref[1] = 0.5
		xref[2] = 1.5
		xref[3] = 1.5
		xref[4] = 0.5
	    } else {
		do i = c1, c2 {
	            xtempmin = min (real (nxsample), max (1.,
		        real ((nxsample - 1) * (i - 0.5) + (GT_NCOLS(geo) -
			nxsample)) / (GT_NCOLS(geo) - 1)))
	            xtempmax = min (real (nxsample), max (1.,
		        real ((nxsample - 1) * (i + 0.5) + (GT_NCOLS(geo) -
			nxsample)) / (GT_NCOLS(geo) - 1)))
		    xref[4*(i-c1)+1] = xtempmin
		    xref[4*(i-c1)+2] = xtempmax
		    xref[4*(i-c1)+3] = xtempmax
		    xref[4*(i-c1)+4] = xtempmin
		}

	    }
	case GT_TWO:
	    if (GT_NCOLS(geo) == 1) {
	        xref[1] = 0.5
		xref[2] = 1.5
	    } else {
		do i = c1, c2 {
	            xtempmin = min (real (nxsample), max (1.,
		        real ((nxsample - 1) * (i - 0.5) + (GT_NCOLS(geo) -
			nxsample)) / (GT_NCOLS(geo) - 1)))
	            xtempmax = min (real (nxsample), max (1.,
		        real ((nxsample - 1) * (i + 0.5) + (GT_NCOLS(geo) -
			nxsample)) / (GT_NCOLS(geo) - 1)))
		    xref[2*(i-c1)+1] = xtempmin
		    xref[2*(i-c1)+2] = xtempmax
		}
	    }
	case GT_ONE:
	    if (GT_NCOLS(geo) == 1)
	        xref[1] = 1.0
	    else {
	        do i = c1, c2
	            xref[i-c1+1] = min (real (nxsample), max (1.,
		        real ((nxsample - 1) * i + (GT_NCOLS(geo) -
			nxsample)) / (GT_NCOLS(geo) - 1)))
	    }
	}

	switch (cmode) {
	case GT_FOUR:
	    if (GT_NLINES(geo) == 1) {
	        yref[1] = 0.5
		yref[2] = 0.5
		yref[3] = 1.5
		yref[4] = 1.5
	    } else {
	        do i = l1, l2 {
	            ytempmin = min (real (nysample), max (1.,
		        real ((nysample - 1) * (i - 0.5) + (GT_NLINES(geo) -
			nysample)) / (GT_NLINES(geo) - 1)))
	            ytempmax = min (real (nysample), max (1.,
		        real ((nysample - 1) * (i + 0.5) + (GT_NLINES(geo) -
			nysample)) / (GT_NLINES(geo) - 1)))
		    yref[4*(i-l1)+1] = ytempmin
		    yref[4*(i-l1)+2] = ytempmin
		    yref[4*(i-l1)+3] = ytempmax
		    yref[4*(i-l1)+4] = ytempmax
		}
	    }
	case GT_TWO:
	    if (GT_NLINES(geo) == 1) {
	        yref[1] = 0.5
		yref[2] = 1.5
	    } else {
	        do i = l1, l2 {
	            ytempmin = min (real (nysample), max (1.,
		        real ((nysample - 1) * (i - 0.5) + (GT_NLINES(geo) -
			nysample)) / (GT_NLINES(geo) - 1)))
	            ytempmax = min (real (nysample), max (1.,
		        real ((nysample - 1) * (i + 0.5) + (GT_NLINES(geo) -
			nysample)) / (GT_NLINES(geo) - 1)))
		    yref[2*(i-l1)+1] = ytempmin
		    yref[2*(i-l1)+2] = ytempmax
		}
	    }
	case GT_ONE:
	    if (GT_NLINES(geo) == 1)
	        yref[1] = 1.0
	    else {
	        do i = l1, l2
	            yref[i-l1+1] = min (real (nysample), max (1.,
		        real ((nysample - 1) * i + (GT_NLINES(geo) -
			nysample)) / (GT_NLINES(geo) - 1)))
	    }
	}
end


# GEO_XBUFFER -- Compute the x interpolant and coordinates.

procedure geo_xbuffer (s1, s2,  msi, xsample, ysample, c1, c2, l1, l2, buf)

pointer	s1, s2		#I pointers to the x surface
pointer	msi		#I interpolant
real	xsample[ARB]	#I sampled x reference coordinates
real	ysample[ARB]	#I sampled y reference coordinates
int	c1, c2		#I columns of interest in sampled image
int	l1, l2		#I lines of interest in the sampled image
pointer	buf		#I pointer to output buffer

int	i, ncols, nlines, llast1, llast2, nclast, nllast
pointer	sp, sf, y, z, buf1, buf2

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# Combine surfaces.
	if (s2 == NULL)
	    call gscopy (s1, sf)
	else
	    call gsadd (s1, s2, sf)

	# Allocate working space.
	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (z, ncols, TY_REAL)

	# If buffer undefined then allocate memory for the buffer. Reallocate
	# the buffer if the number of lines or columns changes.
	if (buf ==  NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	}

	# Compute the coordinates.
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

	# Fit the interpolant.
	if (nlines == 1)
	    call asifit (msi, Memr[buf], ncols)
	else
	    call msifit (msi, Memr[buf], ncols, nlines, ncols)

	call gsfree (sf)
	call sfree (sp)
end


# GEO_YBUFFER -- Compute the y interpolant and coordinates.

procedure geo_ybuffer (s1, s2,  msi, xsample, ysample, c1, c2, l1, l2, buf)

pointer	s1, s2		#I pointers to the y surface
pointer	msi		#I interpolant
real	xsample[ARB]	#I sampled x reference coordinates
real	ysample[ARB]	#I sampled y reference coordinates
int	c1, c2		#I columns of interest in sampled image
int	l1, l2		#I lines of interest in the sampled image
pointer	buf		#I pointer to output buffer

int	i, ncols, nlines, llast1, llast2, nclast, nllast
pointer	sp, sf, y, z, buf1, buf2

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# Combine surfaces.
	if (s2 == NULL)
	    call gscopy (s1, sf)
	else
	    call gsadd (s1, s2, sf)

	# Allocate working space.
	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (z, ncols, TY_REAL)

	# If buffer undefined then allocate memory for the buffer. Reallocate
	# the buffer if the number of lines or columns changes.
	if (buf ==  NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	}

	# Compute the coordinates.
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

	# Fit the interpolant.
	if (nlines == 1)
	    call asifit (msi, Memr[buf], ncols)
	else
	    call msifit (msi, Memr[buf], ncols, nlines, ncols)

	call gsfree (sf)
	call sfree (sp)
end


# GEO_JBUFFER -- Fit the jacobian surface.

procedure geo_jbuffer (sx1, sy1, sx2, sy2, jmsi, xsample, ysample, c1, c2, l1,
        l2, jbuf)

pointer	sx1, sy1	#I pointers to the linear surfaces
pointer	sx2, sy2	#I pointers to the distortion surfaces
pointer	jmsi		#I interpolant
real	xsample[ARB]	#I sampled x reference coordinates
real	ysample[ARB]	#I sampled y reference coordinates
int	c1, c2		#I columns of interest in sampled image
int	l1, l2		#I lines of interest in the sampled image
pointer	jbuf		#I pointer to output buffer

int	i, ncols, nlines, llast1, llast2, nclast, nllast
pointer	sp, sx, sy, y, z, buf1, buf2

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# Combine surfaces.
	if (sx2 == NULL)
	    call gscopy (sx1, sx)
	else
	    call gsadd (sx1, sx2, sx)
	if (sy1 == NULL)
	    sy = NULL
	else if (sy2 == NULL)
	    call gscopy (sy1, sy)
	else
	    call gsadd (sy1, sy2, sy)

	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (z, ncols, TY_REAL)

	# If buffer undefined then allocate memory for the buffer. Reallocate
	# the buffer if the number of lines or columns changes.
	if (jbuf ==  NULL) {
	    call malloc (jbuf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (jbuf, ncols * nlines, TY_REAL)
	    llast1 = l1 - nlines
	    llast2 = l2 - nlines
	}

	# Compute surface.
	if (l1 < llast1) {
	    do i = l2, l1, -1 {
		if (i > llast1)
		    buf1 = jbuf + (i - llast1) * ncols
		else {
		    buf1 = z
		    call amovkr (ysample[i], Memr[y], ncols)
		    call geo_jgsvector (sx, sy, xsample[c1], Memr[y],
		        Memr[buf1], ncols)
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
		    call geo_jgsvector (sx, sy, xsample[c1], Memr[y],
		        Memr[buf1], ncols)
		}
		buf2 = jbuf + (i - l1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	}

	# Update buffer pointers.
	llast1 = l1
	llast2 = l2
	nclast = ncols
	nllast = nlines

	# Fit the interpolant.
	if (nlines == 1)
	    call asifit (jmsi, Memr[jbuf], ncols)
	else
	    call msifit (jmsi, Memr[jbuf], ncols, nlines, ncols)

	call gsfree (sx)
	call gsfree (sy)
	call sfree (sp)
end


# GEO_JGSVECTOR -- Procedure to compute the Jacobian of the transformation.

procedure geo_jgsvector (sx, sy, x, y, out, ncols)

pointer	sx, sy			#I surface descriptors
real	x[ARB]			#I x values
real	y[ARB]			#I y values
real	out[ARB]		#O output values
int	ncols			#I number of points

pointer	sp, der1, der2

begin
	call smark (sp)

	if (sy == NULL) {
	    call gsder (sx, x, y, out, ncols, 1, 0)
	} else {
	    call salloc (der1, ncols, TY_REAL)
	    call salloc (der2, ncols, TY_REAL)
	    call gsder (sx, x, y, Memr[der1], ncols, 1, 0)
	    call gsder (sy, x, y, Memr[der2], ncols, 0, 1)
	    call amulr (Memr[der1], Memr[der2], out, ncols)
	    call gsder (sx, x, y, Memr[der1], ncols, 0, 1)
	    call gsder (sy, x, y, Memr[der2], ncols, 1, 0)
	    call amulr (Memr[der1], Memr[der2], Memr[der1], ncols)
	    call asubr (out, Memr[der1], out, ncols)
	}

	call sfree (sp)
end


# GEO_MSIVECTOR -- Procedure to interpolate the surface coordinates

procedure geo_msivector (in, out, geo, xmsi, ymsi, jmsi, msi, sx1, sy1, sx2,
        sy2, xref, c1, c2, nxsample, yref, l1, l2, nysample, x0, y0)

pointer	in		#I pointer to input image
pointer	out		#I pointer to output image
pointer	geo		#I pointer to geotran structure
pointer	xmsi, ymsi	#I pointer to the interpolation cord surfaces
pointer	jmsi		#I pointer to Jacobian surface
pointer	msi		#I pointer to interpolation surface
pointer	sx1, sy1	#I pointers to linear surfaces
pointer	sx2, sy2	#I pointer to higher order surfaces
real	xref[ARB]	#I x reference coordinates
int	c1, c2		#I column limits in output image
int	nxsample	#I the x sample size
real	yref[ARB]	#I y reference coordinates
int	l1, l2		#I line limits in output image
int	nysample	#I the y sample size
int	x0, y0		#I zero points of interpolation coordinates

int	j, ncols, nlines, ncols4, nlines4
int	imc1, imc2, iml1, iml2, nicols, nilines
pointer	sp, txref, tyref, x, y, xin, yin, inbuf, outbuf 
real	xmin, xmax, ymin, ymax, factor
pointer	imgs1r(), imgs2r(), imps1r(), imps2r()
real	geo_jfactor()

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# Find min max of interpolation coords.
	if (IM_NDIM(in) == 1)
	    call geo_iminmax (xref, yref, c1, c2, l1, l2, x0, 0,
	        xmsi, ymsi, xmin, xmax, ymin, ymax)
	else
	    call geo_iminmax (xref, yref, c1, c2, l1, l2, x0, y0,
	        xmsi, ymsi, xmin, xmax, ymin, ymax)

	# Get the appropriate image section and fit the interpolant.
	imc1 = int(xmin) - GT_NXYMARGIN(geo)
	if (imc1 <= 0)
	    imc1 = imc1 - 1
	imc2 = nint (xmax) + GT_NXYMARGIN(geo) + 1
	nicols = imc2 - imc1 + 1
	if (IM_NDIM(in) == 1) {
	    ncols4 = 2 * ncols
	    nlines4 = 2 * nlines
	    iml1 = 1
	    iml2 = 1
	    nilines = 1
	    inbuf = imgs1r (in, imc1, imc2)
	    if (inbuf == EOF)
	        call error (0, "Error reading image")
	    call asifit (msi, Memr[inbuf], nicols)
	} else {
	    ncols4 = 4 * ncols
	    nlines4 = 4 * nlines
	    iml1 = int(ymin) - GT_NXYMARGIN(geo)
	    if (iml1 <= 0)
	        iml1 = iml1 - 1
	    iml2 = nint (ymax) + GT_NXYMARGIN(geo) + 1
	    nilines = iml2 - iml1 + 1
	    inbuf = imgs2r (in, imc1, imc2, iml1, iml2)
	    if (inbuf == EOF)
	        call error (0, "Error reading image")
	    call msifit (msi, Memr[inbuf], nicols, nilines, nicols)
	}

	# Allocate working space.
	call smark (sp)
	if (GT_INTERPOLANT(geo) == II_DRIZZLE || GT_INTERPOLANT(geo) ==
	    II_BIDRIZZLE) {
	    call salloc (txref, ncols4, TY_REAL)
	    call salloc (tyref, nlines4, TY_REAL)
	    call salloc (x, ncols4, TY_REAL)
	    call salloc (y, ncols4, TY_REAL)
	    call salloc (xin, ncols4, TY_REAL)
	    call salloc (yin, ncols4, TY_REAL)
	    if (IM_NDIM(in) == 1)
	        call geo_sample (geo, Memr[txref], c1, c2, nxsample,
		    Memr[tyref], l1, l2, nysample, GT_TWO)
	    else
	        call geo_sample (geo, Memr[txref], c1, c2, nxsample,
		    Memr[tyref], l1, l2, nysample, GT_FOUR)
	    call aaddkr (Memr[txref], real (-x0 + 1), Memr[x], ncols4)
	} else {
	    call salloc (x, ncols, TY_REAL)
	    call salloc (y, ncols, TY_REAL)
	    call salloc (xin, ncols, TY_REAL)
	    call salloc (yin, ncols, TY_REAL)
	    call aaddkr (xref[c1], real (-x0 + 1), Memr[x], ncols)
	}

	# Compute the output buffer.
	do j = l1, l2 {

	    # Write the output image.
	    if (IM_NDIM(in) == 1)
	        outbuf = imps1r (out, c1, c2)
	    else
	        outbuf = imps2r (out, c1, c2, j, j)
	    if (outbuf == EOF)
		call error (0, "Error writing output image")

	    # Compute the interpolation coordinates.
	    if (GT_INTERPOLANT(geo) == II_DRIZZLE || GT_INTERPOLANT(geo) ==
	        II_BIDRIZZLE) {
	        if (IM_NDIM(in) == 1) {
		    call asivector (xmsi, Memr[x], Memr[xin], ncols4)
		    call amovkr (1.0, Memr[yin], ncols4)
	        } else {
	            #call amovkr (yref[j] + real (-y0 + 1), Memr[y], ncols)
		    call geo_repeat (Memr[tyref+4*(j-l1)], 4, Memr[y], ncols)
		    call aaddkr (Memr[y], real(-y0 + 1), Memr[y], ncols4)
	            call msivector (xmsi, Memr[x], Memr[y], Memr[xin], ncols4)
	            call msivector (ymsi, Memr[x], Memr[y], Memr[yin], ncols4)
	        }
	        if (imc1 != 1)
		    call aaddkr (Memr[xin], real (-imc1 + 1), Memr[xin], ncols4)
	        if (iml1 != 1)
		    call aaddkr (Memr[yin], real (-iml1 + 1), Memr[yin], ncols4)
	    } else {
	        if (IM_NDIM(in) == 1) {
		    call asivector (xmsi, Memr[x], Memr[xin], ncols)
		    call amovkr (1.0, Memr[yin], ncols)
	        } else {
	            call amovkr (yref[j] + real (-y0 + 1), Memr[y], ncols)
	            call msivector (xmsi, Memr[x], Memr[y], Memr[xin], ncols)
	            call msivector (ymsi, Memr[x], Memr[y], Memr[yin], ncols)
	        }
	        if (imc1 != 1)
		    call aaddkr (Memr[xin], real (-imc1 + 1), Memr[xin], ncols)
	        if (iml1 != 1)
		    call aaddkr (Memr[yin], real (-iml1 + 1), Memr[yin], ncols)
	    }

	    # Interpolate in the input image.
	    if (IM_NDIM(in) == 1)
	        call asivector (msi, Memr[xin], Memr[outbuf], ncols)
	    else
	        call msivector (msi, Memr[xin], Memr[yin], Memr[outbuf], ncols)

	    # Preserve flux in image.
	    if (GT_FLUXCONSERVE(geo) == YES) {
		factor = GT_XSCALE(geo) * GT_YSCALE(geo)
		if (GT_GEOMODE(geo) == GT_LINEAR || (sx2 == NULL && sy2 ==
		    NULL)) {
		    if (IM_NDIM(in) == 1)
		        call amulkr (Memr[outbuf], factor * geo_jfactor (sx1,
			    NULL), Memr[outbuf], ncols)
		    else
		        call amulkr (Memr[outbuf], factor * geo_jfactor (sx1,
			    sy1), Memr[outbuf], ncols)
		} else {
		    if (IM_NDIM(in) == 1)
		        call geo_msiflux (jmsi, xref, yref, Memr[outbuf],
			    c1, c2, 0, x0, y0)
		    else
		        call geo_msiflux (jmsi, xref, yref, Memr[outbuf],
			    c1, c2, j, x0, y0)
		    call amulkr (Memr[outbuf], factor, Memr[outbuf], ncols)
		}
	    }
	}

	call sfree (sp)
end


# GEO_GSVECTOR -- Evaluate the output image pixels using fitted coordinate
# values and image interpolation.

procedure geo_gsvector (input, output, geo, msi, xref, c1, c2, yref, l1, l2,
        sx1, sy1, sx2, sy2)

pointer	input			#I pointer to input image
pointer	output			#I pointer to output image
pointer	geo			#I pointer to geotran structure
pointer	msi			#I pointer to interpolant
real	xref[ARB]		#I x reference array
int	c1, c2			#I columns of interest in output image
real	yref[ARB]		#I y reference array
int	l1, l2			#I lines of interest in the output image
pointer	sx1, sy1		#I linear surface descriptors
pointer	sx2, sy2		#I distortion surface descriptors

int	j, ncols, nlines, ncols4, nlines4, nicols, nilines
int	imc1, imc2, iml1, iml2
pointer	sp, txref, tyref, y, xin, yin, temp, inbuf, outbuf
real	xmin, xmax, ymin, ymax, factor
pointer	imgs1r(), imgs2r(), imps1r(), imps2r()
real	gsgetr(), geo_jfactor()

begin
	# Compute the number of columns.
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# Compute the maximum and minimum coordinates.
	call geo_minmax (xref, yref, c1, c2, l1, l2, sx1, sy1, sx2, sy2,
	    xmin, xmax, ymin, ymax)

	# Get the appropriate image section and fill the buffer.
	imc1 = int(xmin) - GT_NXYMARGIN(geo)
	if (imc1 <= 0)
	    imc1 = imc1 - 1
	imc2 = nint (xmax) + GT_NXYMARGIN(geo) + 1
	nicols = imc2 - imc1 + 1
	if (IM_NDIM(input) == 1) {
	    iml1 = 1
	    iml2 = 1
	    nilines = 1
	    ncols4 = 2 * ncols
	    nlines4 = 2 * nlines
	    inbuf = imgs1r (input, imc1, imc2)
	    if (inbuf == EOF)
	        call error (0, "Error reading image")
	    call asifit (msi, Memr[inbuf], nicols)
	} else {
	    iml1 = int(ymin) - GT_NXYMARGIN(geo)
	    if (iml1 <= 0)
	        iml1 = iml1 - 1
	    iml2 = nint (ymax) + GT_NXYMARGIN(geo) + 1
	    nilines = iml2 - iml1 + 1
	    ncols4 = 4 * ncols
	    nlines4 = 4 * nlines
	    inbuf = imgs2r (input, imc1, imc2, iml1, iml2)
	    if (inbuf == EOF)
	        call error (0, "Error reading image")
	    call msifit (msi, Memr[inbuf], nicols, nilines, nicols)
	}

	# Allocate working space.
	call smark (sp)
	if (GT_INTERPOLANT(geo) == II_DRIZZLE || GT_INTERPOLANT(geo) ==
		II_BIDRIZZLE) {
	    call salloc (txref, ncols4, TY_REAL)
	    call salloc (tyref, nlines4, TY_REAL)
	    call salloc (y, ncols4, TY_REAL)
	    call salloc (xin, ncols4, TY_REAL)
	    call salloc (yin, ncols4, TY_REAL)
	    call salloc (temp, ncols4, TY_REAL)
	    if (IM_NDIM(input) == 1)
	        call geo_ref (geo, Memr[txref], c1, c2, GT_NCOLS(geo),
	            Memr[tyref], l1, l2, GT_NLINES(geo), gsgetr (sx1, GSXMIN),
	            gsgetr (sx1, GSXMAX), gsgetr (sx1, GSYMIN), gsgetr (sx1,
	            GSYMAX), GT_TWO)
	    else
	        call geo_ref (geo, Memr[txref], c1, c2, GT_NCOLS(geo),
	            Memr[tyref], l1, l2, GT_NLINES(geo), gsgetr (sx1, GSXMIN),
	            gsgetr (sx1, GSXMAX), gsgetr (sx1, GSYMIN), gsgetr (sx1,
	            GSYMAX), GT_FOUR)
	} else {
	    call salloc (y, ncols, TY_REAL)
	    call salloc (xin, ncols, TY_REAL)
	    call salloc (yin, ncols, TY_REAL)
	    call salloc (temp, ncols, TY_REAL)
	}

	# Compute the pixels.
	do j = l1, l2 {

	    # Get output image buffer.
	    if (IM_NDIM(input) == 1)
	        outbuf = imps1r (output, c1, c2)
	    else
	        outbuf = imps2r (output, c1, c2, j, j)
	    if (output == EOF)
		call error (0, "Error writing output image")

	    # Compute the interpolation coordinates.
	    if (GT_INTERPOLANT(geo) == II_DRIZZLE || GT_INTERPOLANT(geo) ==
		II_BIDRIZZLE) {

		# Set the y coordinate.
	        if (IM_NDIM(input) == 1)
	            call geo_repeat (Memr[tyref+2*(j-l1)], 2, Memr[y], ncols)
		else
	            call geo_repeat (Memr[tyref+4*(j-l1)], 4, Memr[y], ncols)

	        # Fit x coords.
	        call gsvector (sx1, Memr[txref], Memr[y], Memr[xin], ncols4)
	        if (sx2 != NULL) {
		    call gsvector (sx2, Memr[txref], Memr[y], Memr[temp],
		        ncols4)
		    call aaddr (Memr[xin], Memr[temp], Memr[xin], ncols4)
	        }
	        if (imc1 != 1)
		    call aaddkr (Memr[xin], real (-imc1 + 1), Memr[xin], ncols4)

	        # Fit y coords.
	        call gsvector (sy1, Memr[txref], Memr[y], Memr[yin], ncols4)
	        if (sy2 != NULL) {
		    call gsvector (sy2, Memr[txref], Memr[y], Memr[temp],
		        ncols4)
		    call aaddr (Memr[yin], Memr[temp], Memr[yin], ncols4)
	        }
	        if (iml1 != 1)
		    call aaddkr (Memr[yin], real (-iml1 + 1), Memr[yin], ncols4)

	    } else {

		# Set the y coordinate.
	        call amovkr (yref[j], Memr[y], ncols)

	        # Fit x coords.
	        call gsvector (sx1, xref[c1], Memr[y], Memr[xin], ncols)
	        if (sx2 != NULL) {
		    call gsvector (sx2, xref[c1], Memr[y], Memr[temp], ncols)
		    call aaddr (Memr[xin], Memr[temp], Memr[xin], ncols)
	        }
	        if (imc1 != 1)
		    call aaddkr (Memr[xin], real (-imc1 + 1), Memr[xin], ncols)

	        # Fit y coords.
	        call gsvector (sy1, xref[c1], Memr[y], Memr[yin], ncols)
	        if (sy2 != NULL) {
		    call gsvector (sy2, xref[c1], Memr[y], Memr[temp], ncols)
		    call aaddr (Memr[yin], Memr[temp], Memr[yin], ncols)
	        }
	        if (iml1 != 1)
		    call aaddkr (Memr[yin], real (-iml1 + 1), Memr[yin], ncols)
	    }

	    # Interpolate in input image.
	    if (IM_NDIM(input) == 1)
	        call asivector (msi, Memr[xin], Memr[outbuf], ncols)
	    else
	        call msivector (msi, Memr[xin], Memr[yin], Memr[outbuf], ncols)

	    # Preserve flux in image.
	    if (GT_FLUXCONSERVE(geo) == YES) {
		factor = GT_XSCALE(geo) * GT_YSCALE(geo)
		if (GT_GEOMODE(geo) == GT_LINEAR || (sx2 == NULL && sy2 ==
		    NULL)) {
		    if (IM_NDIM(input) == 1)
		        call amulkr (Memr[outbuf], factor * geo_jfactor (sx1,
			    NULL), Memr[outbuf], ncols)
		    else
		        call amulkr (Memr[outbuf], factor * geo_jfactor (sx1,
			    sy1), Memr[outbuf], ncols)
		} else {
		    if (IM_NDIM(input) == 1)
		        call geo_gsflux (xref, yref, Memr[outbuf], c1, c2, j,
		            sx1, NULL, sx2, NULL)
		    else
		        call geo_gsflux (xref, yref, Memr[outbuf], c1, c2, j,
		            sx1, sy1, sx2, sy2)
		    call amulkr (Memr[outbuf], factor, Memr[outbuf], ncols)
		}
	    }
	}

	call sfree (sp)
end


# GEO_IMINMAX -- Find minimum and maximum interpolation coordinates.

procedure geo_iminmax (xref, yref, c1, c2, l1, l2, x0, y0, xmsi, ymsi, xmin,
        xmax, ymin, ymax)

real	xref[ARB]		#I x reference coords
real	yref[ARB]		#I y reference coords
int	c1, c2			#I columns limits
int	l1, l2			#I line limits
int	x0, y0			#I interpolation coord zero points
pointer	xmsi, ymsi		#I coord surfaces
real	xmin, xmax		#O output xmin and xmax
real	ymin, ymax		#O output ymin and ymax

int	j, ncols
pointer	sp, x, y, xin, yin
real	mintemp, maxtemp, x1, x2, y1, y2
real	asieval(), msieval()

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

		call aaddkr (xref[c1], real (-x0 + 1), Memr[x], ncols)
		if (y0 <= 0) {
		    call asivector (xmsi, Memr[x], Memr[xin], ncols)
		    ymin = 1.0
		    ymax = 1.0
		} else {
	            call amovkr (yref[j] + real (-y0 + 1), Memr[y], ncols)
		    call msivector (xmsi, Memr[x], Memr[y], Memr[xin], ncols)
		    call msivector (ymsi, Memr[x], Memr[y], Memr[yin], ncols)
		    call alimr (Memr[yin], ncols, mintemp, maxtemp)
		    ymin = min (ymin, mintemp)
		    ymax = max (ymax, maxtemp)
		}
		call alimr (Memr[xin], ncols, mintemp, maxtemp)
		xmin = min (xmin, mintemp)
		xmax = max (xmax, maxtemp)
	    } else {
		if (y0 <= 0) {
		    x1 = asieval (xmsi, xref[c1] + real (-x0 + 1))
		    x2 = asieval (xmsi, xref[c1+ncols-1] + real (-x0 + 1))
		    ymin = 1.0
		    ymax = 1.0
		} else {
		    x1 = msieval (xmsi, xref[c1] + real (-x0 + 1),
		        yref[j] + real (-y0 + 1))
		    x2 = msieval (xmsi, xref[c1+ncols-1] + real (-x0 + 1),
		        yref[j] + real (-y0 + 1))
		    y1 = msieval (ymsi, xref[c1] + real (-x0 + 1),
		        yref[j]  + real (-y0 + 1))
		    y2 = msieval (ymsi, xref[c1+ncols-1] + real (-x0 + 1),
		        yref[j] + real (-y0 + 1))
		    ymin = min (ymin, y1, y2)
		    ymax = max (ymax, y1, y2)
		}
		xmin = min (xmin, x1, x2)
		xmax = max (xmax, x1, x2)

	    }
	}

	call sfree (sp)

end


# GEO_MINMAX -- Compute the minimum and maximum fitted coordinates.

procedure geo_minmax (xref, yref, c1, c2, l1, l2, sx1, sy1, sx2, sy2,
	xmin, xmax, ymin, ymax)

real	xref[ARB]		#I x reference coords
real	yref[ARB]		#I y reference coords
int	c1, c2			#I columns limits
int	l1, l2			#I line limits
pointer	sx1, sy1		#I linear surface descriptors
pointer	sx2, sy2		#I distortion surface descriptors
real	xmin, xmax		#O output xmin and xmax
real	ymin, ymax		#O output ymin and ymax

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

	# Find the maximum and minimum coordinates.
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


# GEO_MARGSET -- Set up interpolation margin

procedure geo_margset (sx1, sy1, sx2, sy2, xmin, xmax, ncols, ymin, ymax,
	nlines, interpolant, nsinc, nxymargin)

pointer	sx1, sy1		#I linear surface descriptors
pointer	sx2, sy2		#I distortion surface descriptors
real	xmin, xmax		#I the reference coordinate x limits
int	ncols			#I the number of output image columns
real	ymin, ymax		#I the reference coordinate y limits
int	nlines			#I the number of output image lines
int	interpolant		#I the interpolant type
int	nsinc			#I the sinc width
int	nxymargin		#O the interpolation margin

int	dist1, dist2, dist3, dist4, dist5, dist6
pointer	newsx, newsy
real	x1, y1, x2, y2
real	gseval()

begin
	if (interpolant == II_SPLINE3 || interpolant == II_BISPLINE3) {
	    nxymargin = NMARGIN_SPLINE3
	} else if (interpolant == II_LSINC || interpolant == II_BILSINC) {
	    nxymargin = nsinc
	} else if (interpolant == II_SINC || interpolant == II_BISINC) {
	    nxymargin = nsinc
	} else if (interpolant == II_DRIZZLE || interpolant == II_BIDRIZZLE) {
	    if (sx2 == NULL)
		call gscopy (sx1, newsx)
	    else
		call gsadd (sx1, sx2, newsx)
	    if (sy2 == NULL)
		call gscopy (sy1, newsy)
	    else
		call gsadd (sy1, sy2, newsy)
	    x1 = gseval (newsx, xmin, ymin)
	    y1 = gseval (newsy, xmin, ymin)
	    x2 = gseval (newsx, xmax, ymin)
	    y2 = gseval (newsy, xmax, ymin)
	    dist1 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2) / ncols
	    x1 = gseval (newsx, xmax, ymax)
	    y1 = gseval (newsy, xmax, ymax)
	    dist2 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2) / nlines
	    x2 = gseval (newsx, xmin, ymax)
	    y2 = gseval (newsy, xmin, ymax)
	    dist3 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2) / ncols
	    x1 = gseval (newsx, xmin, ymin)
	    y1 = gseval (newsy, xmin, ymin)
	    dist4 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2) / nlines
	    x1 = gseval (newsx, xmin, (ymin + ymax) / 2.0)
	    y1 = gseval (newsy, xmin, (ymin + ymax) / 2.0)
	    x2 = gseval (newsx, xmax, (ymin + ymax) / 2.0)
	    y2 = gseval (newsy, xmax, (ymin + ymax) / 2.0)
	    dist5 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2) / ncols
	    x1 = gseval (newsx, (xmin + xmax) / 2.0, ymin)
	    y1 = gseval (newsy, (xmin + xmax) / 2.0, ymin)
	    x2 = gseval (newsx, (xmin + xmax) / 2.0, ymax)
	    y2 = gseval (newsy, (xmin + xmax) / 2.0, ymax)
	    dist6 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2) / nlines
	    nxymargin = max (NMARGIN, dist1, dist2, dist3, dist4,
	        dist5, dist6)
	    call gsfree (newsx)
	    call gsfree (newsy)
	} else {
	    nxymargin = NMARGIN
	}
end


# GEO_IMSET -- Set up input image boundary conditions.

procedure geo_imset (im, geo, sx1, sy1, sx2, sy2, xref, nx, yref, ny)

pointer	im			#I pointer to image
pointer	geo			#I pointer to geotran structure
pointer	sx1, sy1		#I linear surface descriptors
pointer	sx2, sy2		#I distortion surface descriptors
real	xref[ARB]		#I x reference coordinates
int	nx			#I number of x reference coordinates
real	yref[ARB]		#I y reference coordinates
int	ny			#I number of y reference coordinates

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

	yn1 = gseval (sy1, GT_XMIN(geo), GT_YMIN(geo))
	yn2 = gseval (sy1, GT_XMAX(geo), GT_YMIN(geo))
	yn3 = gseval (sy1, GT_XMAX(geo), GT_YMAX(geo))
	yn4 = gseval (sy1, GT_XMIN(geo), GT_YMAX(geo))

	xmin = min (xn1, xn2, xn3, xn4)
	ymin = min (yn1, yn2, yn3, yn4)
	xmax = max (xn1, xn2, xn3, xn4)
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

	# Compute the out-of-bounds limit.
	if (IM_NDIM(im) == 1) {
	    if (xmin < 1.0 || xmax > real (IM_LEN(im,1)))
	        bndry = max (1.0 - xmin, xmax - IM_LEN(im,1)) + 1
	    else
	        bndry = 1
	} else {
	    if (xmin < 1.0 || ymin < 1.0 || xmax > real (IM_LEN(im,1)) ||
	        ymax > real (IM_LEN(im,2)))
	        bndry = max (1.0 - xmin, 1.0 - ymin, xmax - IM_LEN(im,1),
		    ymax - IM_LEN(im,2)) + 1
	    else
	        bndry = 1
	}

	call imseti (im, IM_NBNDRYPIX, bndry + GT_NXYMARGIN(geo) + 1)
	call imseti (im, IM_TYBNDRY, GT_BOUNDARY(geo))
	call imsetr (im, IM_BNDRYPIXVAL, GT_CONSTANT(geo))
end


# GEO_GSFLUX -- Preserve the image flux after a transformation.

procedure geo_gsflux (xref, yref, buf, c1, c2, line, sx1, sy1, sx2, sy2)

real	xref[ARB]		#I x reference coordinates
real	yref[ARB]		#I y reference coordinates
real	buf[ARB]		#O output image buffer
int	c1, c2			#I column limits in the output image
int	line			#I line in the output image
pointer	sx1, sy1		#I linear surface descriptors
pointer	sx2, sy2		#I distortion surface descriptors

int	ncols
pointer	sp, y, der1, der2, jacob, sx, sy

begin
	ncols = c2 - c1 + 1

	# Get the reference coordinates.
	call smark (sp)
	call salloc (y, ncols, TY_REAL)
	call salloc (jacob, ncols, TY_REAL)

	# Add the two surfaces together for efficiency.
	if (sx2 != NULL)
	    call gsadd (sx1, sx2, sx)
	else
	    call gscopy (sx1, sx)
	if (sy1 == NULL)
	    sy = NULL
	else if (sy2 != NULL)
	    call gsadd (sy1, sy2, sy)
	else
	    call gscopy (sy1, sy)

	# Multiply the output buffer by the Jacobian.
	call amovkr (yref[line], Memr[y], ncols)
	if (sy == NULL)
	    call gsder (sx, xref[c1], Memr[y], Memr[jacob], ncols, 1, 0)
	else {
	    call salloc (der1, ncols, TY_REAL)
	    call salloc (der2, ncols, TY_REAL)
	    call gsder (sx, xref[c1], Memr[y], Memr[der1], ncols, 1, 0)
	    call gsder (sy, xref[c1], Memr[y], Memr[der2], ncols, 0, 1)
	    call amulr (Memr[der1], Memr[der2], Memr[jacob], ncols)
	    call gsder (sx, xref[c1], Memr[y], Memr[der1], ncols, 0, 1) 
	    call gsder (sy, xref[c1], Memr[y], Memr[der2], ncols, 1, 0)
	    call amulr (Memr[der1], Memr[der2], Memr[der1], ncols)
	    call asubr (Memr[jacob], Memr[der1], Memr[jacob], ncols)
	}
	call aabsr (Memr[jacob], Memr[jacob], ncols)
	call amulr (buf, Memr[jacob], buf, ncols)

	# Clean up.
	call gsfree (sx)
	if (sy != NULL)
	    call gsfree (sy)
	call sfree (sp)
end


# GEO_MSIFLUX -- Procedure to interpolate the surface coordinates

procedure geo_msiflux (jmsi, xinterp, yinterp, outdata, c1, c2, line, x0, y0)

pointer	jmsi	        	#I pointer to the jacobian interpolant
real	xinterp[ARB]		#I x reference coordinates
real	yinterp[ARB]		#I y reference coordinates
real	outdata[ARB]		#O output data
int	c1, c2			#I column limits in output image
int	line			#I line to be flux corrected
int	x0, y0			#I zero points of interpolation coordinates

int	ncols
pointer	sp, x, y, jacob

begin
	# Allocate tempoaray space.
	call smark (sp)
	ncols = c2 - c1 + 1
	call salloc (x, ncols, TY_REAL)
	call salloc (jacob, ncols, TY_REAL)

	# Calculate the x points.
	if (x0 == 1)
	    call amovr (xinterp[c1], Memr[x], ncols)
	else
	    call aaddkr (xinterp[c1], real (-x0 + 1), Memr[x], ncols)

	# Multiply the data by the Jacobian.
	if (line == 0) {
	    call asivector (jmsi, Memr[x], Memr[jacob], ncols)
	} else {
	    call salloc (y, ncols, TY_REAL)
	    call amovkr ((yinterp[line] + real (-y0 + 1)), Memr[y], ncols)
	    call msivector (jmsi, Memr[x], Memr[y], Memr[jacob], ncols)
	}
	call aabsr (Memr[jacob], Memr[jacob], ncols)
	call amulr (outdata, Memr[jacob], outdata, ncols)

	call sfree (sp)
end


# GEO_JFACTOR -- Compute the Jacobian of a linear transformation.

real procedure geo_jfactor (sx1, sy1)

pointer	sx1			#I pointer to x surface
pointer	sy1			#I pointer to y surface

real	xval, yval, xx, xy, yx, yy
real	gsgetr()

begin
	xval = (gsgetr (sx1, GSXMIN) + gsgetr (sx1, GSXMAX)) / 2.0
	if (sy1 == NULL)
	    yval = 1.0
	else
	    yval = (gsgetr (sy1, GSYMIN) + gsgetr (sy1, GSYMIN)) / 2.0 

	call gsder (sx1, xval, yval, xx, 1, 1, 0)
	if (sy1 == NULL) {
	    xy = 0.0
	    yy = 1.0
	    yx = 0.0
	} else {
	    call gsder (sx1, xval, yval, xy, 1, 0, 1)
	    call gsder (sy1, xval, yval, yx, 1, 1, 0)
	    call gsder (sy1, xval, yval, yy, 1, 0, 1)
	}

	return (abs (xx * yy - xy * yx))
end


# GEO_REPEAT -- Copy a small repeated pattern into the output buffer.

procedure geo_repeat (pat, npat, output, ntimes)

real	pat[ARB]	#I the input pattern to be repeated 
int	npat		#I the size of the pattern
real	output[ARB]	#O the output array
int	ntimes		#I the number of times the pattern is to be repeated

int	j, i, offset

begin
	do j = 1, ntimes {
	    offset = npat * j - npat
	    do i = 1, npat
		output[offset+i] = pat[i]
	}
end
