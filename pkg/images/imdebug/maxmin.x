# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>

# MAXMIN -- Compute the minimum and maximum pixel values of an image.
# Works for images of any dimensionality, size, or datatype.

procedure t_maxmin()

char	imname[SZ_FNAME]
real	minval, maxval
long	v[IM_MAXDIM], clktime()
pointer	im, buf, immap(), imgnlr()

begin
	call clgstr ("imname", imname, SZ_FNAME)
	call amovkl (long(1), v, IM_MAXDIM)		# start vector

	im = immap (imname, READ_WRITE, 0)

	# Only calculate minimum, maximum pixel values if the current
	# values are unknown, or if the image was modified since the
	# old values were computed.

	if (IM_LIMTIME(im) < IM_MTIME(im)) {
	    IM_MIN(im) = MAX_REAL
	    IM_MAX(im) = -MAX_REAL

	    while (imgnlr (im, buf, v) != EOF) {
		call alimr (Memr[buf], IM_LEN(im,1), minval, maxval)
		IM_MIN(im) = min (IM_MIN(im), minval)
		IM_MAX(im) = max (IM_MAX(im), maxval)
	    }

	    IM_LIMTIME(im) = clktime (long(0))
	}

	call clputr ("minval", IM_MIN(im))
	call clputr ("maxval", IM_MAX(im))

	call imunmap (im)
end
