# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"iis.h"

# FINDZ -- Estimate the range of greylevels Z1 to Z2 containing a specified
# fraction of the greylevels in the image.  The technique is to sample the
# image at some interval, computing the values of the greylevels a fixed
# distance either side of the median.  Since it is not necessary to compute
# the full histogram we do not need to know the image zmin, zmax in advance.
# Works for images of any dimensionality, size, or datatype.

procedure findz (im, z1, z2, zfrac, maxcols, nsample_lines)

pointer	im
real	z1, z2, zfrac
int	maxcols, nsample_lines

real	rmin, rmax
real	frac
int	imin, imax, ncols, nlines
int	i, n, step, sample_size, imlines

pointer	sp, buf
pointer	imgl2r()
include	"iis.com"

begin
	call smark (sp)
	call salloc (buf, ncols, TY_REAL)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Try to include a constant number of pixels in the sample
	# regardless of the image size.  The entire image is used if we
	# have a small image, and at least sample_lines lines are read
	# if we have a large image.

	sample_size = iis_ydim * nsample_lines
	imlines = min(nlines, max(nsample_lines, sample_size / ncols))
	step = nlines / (imlines + 1)

	frac = (1.0 - zfrac) / 2.
	imin = frac * (ncols - 1)
	imax = (1.0 - frac) * (ncols - 1)
	rmin = 0.0
	rmax = 0.0
	n = 0

	do i = 1 + step, nlines, max (1, step) {
	    call asrtr (Memr[imgl2r (im, i)], Memr[buf], ncols)
	    rmin = rmin + Memr[buf + imin]
	    rmax = rmax + Memr[buf + imax]
	    n = n + 1
	}

	z1 = rmin / n
	z2 = rmax / n

	call sfree (sp)
end
