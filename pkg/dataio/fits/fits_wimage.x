# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include "wfits.h"

# WFT_WRITE_IMAGE -- Procedure to convert IRAF image data to FITS format line by
# line.

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

include "wfits.com"

begin
	if (NAXIS(im) == 0)
	    return

	npix = NAXISN(im,1)
	nlines = 1
	do i = 2, NAXIS(im)
	    nlines = nlines * NAXISN(im, i)

	npix_record = len_record * FITS_BYTE / FITS_BITPIX(fits)
	call wft_init_write_pixels (npix_record, TY_LONG, FITS_BITPIX(fits),
	    blkfac)

	if (tempbuf != NULL)
	    call mfree (tempbuf, TY_LONG)
	call malloc (tempbuf, npix, TY_LONG)

	call amovkl (long(1), v, IM_MAXDIM)
	do i = 1, nlines {

	    # Get an image line.
	    iferr (stat =  wft_get_image_line (im, buf, v, PIXTYPE(im))) {
		call erract (EA_WARN)
		call error (10, "WRT_IMAGE: Error writing IRAF image.")
	    }
	    if (stat == EOF )
		return
	    if (stat != npix)
		call error (10, "WRT_IMAGE: Error writing IRAF image.")

	    # Scale the line.
	    if (SCALE(fits) == YES)
	        call wft_scale_line (buf, Meml[tempbuf], npix,
		        1. / BSCALE(fits), -BZERO(fits), PIXTYPE(im))
	    else
		call wft_long_line (buf, Meml[tempbuf], npix, PIXTYPE(im)) 

	    # There is no good mechanism for handling blanks at the moment.
	    # call map_blanks (im, Meml[tempbuf], blank)

	    # Write the pixels.
	    call wft_write_pixels (fits_fd, Meml[tempbuf], npix)
	}

	# Write the final record.
	call wft_write_last_record (fits_fd, nrecords)
	if (short_header == YES || long_header == YES) {
	    call printf ("%d  Data logical (2880 byte) records written\n")
	        call pargi (nrecords)
	    #if (blkfac <= 10) {
		#call printf ("\twith a blocking factor of %d\n")
		    #call pargi (blkfac)
	    #} else {
		#call printf ("\twith the nonstandard block size of %d bytes\n")
		    #call pargi (blkfac)
	    #}
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
	case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	    npix = imgnll (im, buf, v)
	case TY_REAL, TY_COMPLEX:
	    npix = imgnlr (im, buf, v)
	case TY_DOUBLE:
	    npix = imgnld (im, buf, v)
	default:
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

errchk	achtll, achtrl, achtdl, achtxl
errchk  altal, altar, altad

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call altal (Meml[buf], Meml[buf], npix, bzero, bscale)
	    call achtll (Meml[buf], outbuffer, npix)
	case TY_REAL, TY_COMPLEX:
	    call altadr (Memr[buf], Memr[buf], npix, bzero, bscale)
	    call achtrl (Memr[buf], outbuffer, npix)
	case TY_DOUBLE:
	    call altad (Memd[buf], Memd[buf], npix, bzero, bscale)
	    call achtdl (Memd[buf], outbuffer, npix)
	default:
	    call error (12, "SCALE_LINE: Unknown IRAF image type.")
	}
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
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call achtll (Meml[buf], outbuffer, npix)
	case TY_REAL, TY_COMPLEX:
	    call achtrl (Memr[buf], outbuffer, npix)
	case TY_DOUBLE:
	    call achtdl (Memd[buf], outbuffer, npix)
	default:
	    call error (13, "LONG_LINE: Unknown IRAF data type.")
	}
end
