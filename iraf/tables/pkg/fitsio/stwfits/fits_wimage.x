include <imhdr.h>
include "wfits.h"

# WFT_WRITE_IMAGE -- Procedure to convert IRAF image data to FITS format line by
# line.

# Nelson Zarate	??		original
# Phil Hodge	12-July-2005	in wft_scale_line, treat complex data the
#				same as real

procedure wft_write_image (im, fits, fits_fd)

pointer	im			# IRAF image descriptor
pointer	fits			# FITS data structure
int	fits_fd			# FITS file descriptor

long	v[IM_MAXDIM]
pointer	tempbuf, buf
int	npix, nlines, npix_record, i, stat, nrecords

int	wft_get_image_line()
errchk	malloc, mfree, wft_get_image_line, wft_scale_line, wft_long_line
errchk	wft_init_write_pixels, wft_write_pixels, wft_write_last_record

data	tempbuf /NULL/
double  dtemp

include "wfits.com"

begin
	if (NAXIS(im) == 0)
	    return

	npix = NAXISN(im,1)
	nlines = 1
	do i = 2, NAXIS(im)
	    nlines = nlines * NAXISN(im, i)

	npix_record = len_record * FITS_BYTE / FITS_BITPIX(fits)
	if (ieee == YES && PIXTYPE(im) == TY_DOUBLE)
	  call wft_init_write_pixels (npix_record, TY_DOUBLE, FITS_BITPIX(fits))
	else
	  call wft_init_write_pixels (npix_record, TY_LONG, FITS_BITPIX(fits))

	if (tempbuf != NULL)
	    call mfree (tempbuf, TY_LONG)
	if (ieee == YES && PIXTYPE(im) == TY_DOUBLE)
	   call malloc (tempbuf, 2*npix, TY_LONG)
	else
	   call malloc (tempbuf, npix, TY_LONG)

	call amovkl (long(1), v, IM_MAXDIM)
	do i = 1, nlines {

	    # Get an image line.
	    stat =  wft_get_image_line (im, buf, v, PIXTYPE(im))
	    if (stat == EOF )
		return
	    if (stat != npix) {
		call flush (STDOUT)
		call error (10, "WRT_IMAGE: Error writing IRAF image.")
	    }

	    # Scale the line.
	    if (ieee == YES) {
	        call wft_ieee (buf, tempbuf, npix, PIXTYPE(im))
	    } else {
	       if (SCALE(fits) == YES) {
		  dtemp = 1.0d0/ BSCALE(fits)

	          call wft_scale_line (buf, Meml[tempbuf], npix,
		        dtemp, -BZERO(fits), PIXTYPE(im))
	       } else
		 call wft_long_line (buf, Meml[tempbuf], npix, PIXTYPE(im)) 
	    }
	    # not quite sure how to handle blanks at moment
	    # call map_blanks (im, Meml[tempbuf], blank)

	    # write the pixels
	    call wft_write_pixels (fits_fd, Meml[tempbuf], npix)
	}

	# write the final record
	call wft_write_last_record (fits_fd, nrecords)
	if (long_header == YES) {
	    call printf ("%d  Data records(s) written\n")
	        call pargi (nrecords)
	}

end


# WFT_GET_IMAGE_LINE -- Procedure to fetch the next image line.

int procedure wft_get_image_line (im, buf, v, datatype)

pointer	im			# IRAF image descriptor
pointer	buf			# pointer to image line
long	v[ARB]			# imio dimension indicator
int	datatype		# IRAF image data type

int	npix
int	imgnll(), imgnlr(), imgnld()
errchk	imgnll, imgnlr, imgnld, imgnlx

begin
	switch (datatype) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:	
	    npix = imgnll (im, buf, v)
	case TY_REAL, TY_COMPLEX:
	    npix = imgnlr (im, buf, v)
	case TY_DOUBLE:
	    npix = imgnld (im, buf, v)
	default:
	    call flush (STDOUT)
	    call error (11, "GET_IMAGE_LINE: Unknown IRAF image type.")
	}

	return (npix)
end


# WFT_SCALE_LINE -- This procedure converts the IRAF data to type long with
# after scaling by the FITS parameters bscale and bzero.

procedure wft_scale_line (buf, outbuffer, npix, bscale, bzero, datatype)

pointer	buf			# pointer to IRAF image line
long	outbuffer[ARB]		# FITS integer buffer
int	npix			# number of pixels
double	bscale, bzero		# FITS bscale and bzero parameters
int	datatype		# data type of image

errchk	achtll, achtrd, achtrl, achtdl, achtxl
errchk  altal, altar, altad

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call altal (Meml[buf], Meml[buf], npix, bzero, bscale)
	    call achtll (Meml[buf], outbuffer, npix)
	case TY_REAL, TY_COMPLEX:
	    # complex data were read into a buffer of type real
	    call altarl (Memr[buf], outbuffer, npix, bzero, bscale)
	case TY_DOUBLE:
	    call altad (Memd[buf], Memd[buf], npix, bzero, bscale)
	    call achtdl (Memd[buf], outbuffer, npix)
	default:
	    call flush(STDOUT)
	    call error (12, "SCALE_LINE: Unknown IRAF image type.")
	}
end

# ALTARL -- Procedure to linearly scale a real vector in double precision

procedure altarl (a, b, npix, k1, k2)

real	a[ARB]		# input vector
long	b[ARB]		# output vector
int	npix		# number of pixels
double	k1, k2		# scaling factors

int	i

begin
	do i = 1, npix
	    b[i] = (a[i] + k1) * k2
end

# ALTADR -- Procedure to linearly scale a real vector in double precision

procedure altadr (a, b, npix, k1, k2)

real	a[ARB]		# input vector
real	b[ARB]		# output vector
int	npix		# number of pixels
double	k1, k2		# scaling factors

int	i

begin
	do i = 1, npix
	    b[i] = (a[i] + k1) * k2
end


# WFT_LONG_LINE -- This procedure converts the IRAF image line to type long with
# no scaling.

procedure wft_long_line (buf, outbuffer, npix, datatype)

pointer	buf			# pointer to IRAF image line
long	outbuffer[ARB]		# buffer of FITS integers
int	npix			# number of pixels
int	datatype		# IRAF image datatype

errchk	achtll, achtrl, achtdl, achtxl

begin
	switch (datatype) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    call achtll (Meml[buf], outbuffer, npix)
	case TY_REAL, TY_COMPLEX:
	    call achtrl (Memr[buf], outbuffer, npix)
	case TY_DOUBLE:
	    call achtdl (Memd[buf], outbuffer, npix)
	default:
	    call flush (STDOUT)
	    call error (13, "LONG_LINE: Unknown IRAF data type.")
	}
end
procedure wft_ieee (buf, outbuf, npix, datatype)

pointer buf		# pointer to IRAF image line
pointer	outbuf		# pointer to ieee floating point data line
int	npix		# number of pixels
int	datatype	# Iraf image datatype

begin
	# Datatype of SHORT, USHORT, INT and LONG are not treated here.
	# The flag ieee is reset to NO for this types.
	switch (datatype) {
	case TY_REAL:
#	    call vx2sur (Memr[buf], Meml[outbuf], npix)
	    call ieevpakr (Memr[buf], Meml[outbuf], npix)
	case TY_DOUBLE:
#	    call vx2sund (Memd[buf], Meml[outbuf], npix)
	    call ieevpakd (Memd[buf], Meml[outbuf], npix)
	default:
	    call flush (STDOUT)
	    call error (13, "WFT_IEEE: Datatype not supported for change.")
	}
end
