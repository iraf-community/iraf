# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>

# IMPNLN -- Put the next line to an image of any dimension or datatype.
# This is a sequential operator.  The index vector V should be initialized
# to the first line to be written before the first call.  Each call increments
# the leftmost subscript by one, until V equals IM_LEN.  EOF is returned
# when the last line in the image has been written.

int procedure impnln (im, lineptr, v, dtype)

pointer	im
pointer	lineptr				# on output, points to the pixels
long	v[IM_MAXDIM]			# loop counter
int	dtype				# eventual datatype of pixels

long	lineoff, line, band, offset
int	dim, ndim, junk, sz_pixel, sz_dtype, fd, nchars, pixtype
long	vs[IM_MAXDIM], ve[IM_MAXDIM], unit_v[IM_MAXDIM], npix

int	imloop()
pointer	imgobf(), fwritep()
errchk	imgobf, fwritep, imerr, imopsf
define	retry_ 91
define	oob_ 92
define	misaligned_ 93

int	sizeof()
include	<szpixtype.inc>
data	unit_v /IM_MAXDIM * 1/

begin
	ndim = IM_NDIM(im)
	if (ndim == 0)
	    return (EOF)

	npix = IM_LEN(im,1)			# write entire line
	pixtype = IM_PIXTYPE(im)
	sz_pixel = pix_size[pixtype]
	sz_dtype = sizeof(pixtype)

	# Perform "zero trip" check (V >= VE), before entering "loop".
	if (v[ndim] > IM_LEN(im,ndim))
	    return (EOF)
retry_
	if (IM_FAST(im) == YES && pixtype == dtype && ndim <= 3) {
	    fd = IM_PFD(im)
	    if (fd == NULL) {
		call imopsf (im)
		goto retry_
	    }

	    # Lineoff is the dimensionless line offset in the pixel storage
	    # file (which we assume to be in line storage mode).

	    lineoff = 0
	    if (ndim > 1) {
		line = v[2]
		if (line < 1 || line  > IM_LEN(im,2))
		    goto oob_
		lineoff = line - 1
		if (ndim > 2) {
		    band = v[3]
		    if (band < 1 || band > IM_LEN(im,3))
oob_			call imerr (IM_NAME(im), SYS_IMREFOOB)
		    lineoff = lineoff + (band - 1) * IM_PHYSLEN(im,2)
		}
	    }

	    # Reference directly into the FIO buffer.  If the line straddles
	    # FIO block boundaries then fwritep will return error and we must
	    # use a separate buffer.

	    offset = lineoff * IM_PHYSLEN(im,1) * sz_pixel + IM_PIXOFF(im)
	    nchars = IM_PHYSLEN(im,1) * sz_pixel
	    iferr (lineptr = (fwritep (fd, offset, nchars) - 1) / sz_pixel + 1)
		goto misaligned_

	} else {
misaligned_
	    # Prepare section descriptor vectors.
	    vs[1] = 1
	    ve[1] = npix
	    do dim = 2, ndim {
		vs[dim] = v[dim]
		ve[dim] = v[dim]
	    }
	    
	    # Get the output line buffer.
	    lineptr = imgobf (im, vs, ve, ndim, dtype)
	    IM_FLUSH(im) = YES
	}

	# Increment loop vector (cannot use nested loops since the dimension
	# of the image is variable).  Note this loop vector references
	# logical section coordinates.

	if (ndim == 1)
	    v[1] = IM_LEN(im,1) + 1
	else if (ndim == 2 && IM_FAST(im) == YES)
	    v[2] = v[2] + 1
	else
	    junk = imloop (v, unit_v, IM_LEN(im,1), unit_v, ndim)

	return (npix)
end
