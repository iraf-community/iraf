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

int	npix, nlines, npix_record, i, stat, nrecords
long	v[IM_MAXDIM]
pointer	tempbuf, buf

int	wft_get_image_line()
errchk	malloc, mfree, wft_get_image_line, wft_lscale_line, wft_long_line
errchk	wft_init_write_pixels, wft_write_pixels, wft_write_last_record
errchk	wft_rscale_line, wft_dscale_line

include "wfits.com"

begin
	if (NAXIS(im) == 0) {
	    if (short_header == YES || long_header == YES) {
	        call printf ("0  Data logical (2880 byte) records written\n")
	    }
	    return
	}

	# Initialize.
	npix = NAXISN(im,1)
	nlines = 1
	do i = 2, NAXIS(im)
	    nlines = nlines * NAXISN(im, i)
	npix_record = len_record * FITS_BYTE / abs (FITS_BITPIX(fits))

	call amovkl (long(1), v, IM_MAXDIM)
	switch (FITS_BITPIX(fits)) {
	case FITS_REAL:

	    # Allocate temporary space.
	    call malloc (tempbuf, npix, TY_REAL)

	    # Initialize the pixel write.
	    call wft_init_write_pixels (npix_record, TY_REAL,
	        FITS_BITPIX(fits), blkfac)

	    # For the time being explicitly turn off ieee NaN mapping.
	    call ieemapr (NO, NO)

	    # Scale the lines, deal with the blanks via the ieee code which
	    # is currently turned off, and write the output records.

	    do i = 1, nlines {
	        iferr (stat =  wft_get_image_line (im, buf, v, PIXTYPE(im))) {
		    call erract (EA_WARN)
		    call error (10, "WRT_IMAGE: Error writing IRAF image.")
	        }
	        if (stat == EOF )
		    return
	        if (stat != npix)
		    call error (10, "WRT_IMAGE: Error writing IRAF image.")
	        if (SCALE(fits) == YES)
	            call wft_rscale_line (buf, Memr[tempbuf], npix,
		        1. / BSCALE(fits), -BZERO(fits), PIXTYPE(im))
		else
		    call wft_real_line (buf, Memr[tempbuf], npix, PIXTYPE(im)) 
	        call wft_write_pixels (fits_fd, Memr[tempbuf], npix)
	    }

	    # Free space.
	    call mfree (tempbuf, TY_REAL)

	case FITS_DOUBLE:

	    # Allocate temporary space.
	    call malloc (tempbuf, npix, TY_DOUBLE)

	    # Initialize the pixel write.
	    call wft_init_write_pixels (npix_record, TY_DOUBLE,
	        FITS_BITPIX(fits), blkfac)

	    # For the time being explicitly turn off ieee NaN mapping.
	    call ieemapd (NO, NO)

	    # Scale the lines, deal with the blanks via the ieee code which
	    # is currently turned off, and write the output records.

	    do i = 1, nlines {
	        iferr (stat =  wft_get_image_line (im, buf, v, PIXTYPE(im))) {
		    call erract (EA_WARN)
		    call error (10, "WRT_IMAGE: Error writing IRAF image.")
	        }
	        if (stat == EOF )
		    return
	        if (stat != npix)
		    call error (10, "WRT_IMAGE: Error writing IRAF image.")
	        if (SCALE(fits) == YES)
	            call wft_dscale_line (buf, Memd[tempbuf], npix,
		        1. / BSCALE(fits), -BZERO(fits), PIXTYPE(im))
		else
		    call wft_double_line (buf, Memd[tempbuf], npix,
		        PIXTYPE(im)) 
	        call wft_write_pixels (fits_fd, Memd[tempbuf], npix)
	    }

	    # Free space.
	    call mfree (tempbuf, TY_DOUBLE)

	default:

	    # Allocate temporary space.
	    call malloc (tempbuf, npix, TY_LONG)

	    # Scale the line, deal with the blanks, and write the output
	    # record. At the moement blanks are not dealt with.

	    call wft_init_write_pixels (npix_record, TY_LONG, FITS_BITPIX(fits),
	         blkfac)
	    do i = 1, nlines {
	        iferr (stat =  wft_get_image_line (im, buf, v, PIXTYPE(im))) {
		    call erract (EA_WARN)
		    call error (10, "WRT_IMAGE: Error writing IRAF image.")
	        }
	        if (stat == EOF )
		    return
	        if (stat != npix)
		    call error (10, "WRT_IMAGE: Error writing IRAF image.")
	        if (SCALE(fits) == YES)
	            call wft_lscale_line (buf, Meml[tempbuf], npix,
		        1. / BSCALE(fits), -BZERO(fits), PIXTYPE(im))
	        else
		    call wft_long_line (buf, Meml[tempbuf], npix, PIXTYPE(im)) 
	        # call map_blanks (im, Meml[tempbuf], blank)
	        call wft_write_pixels (fits_fd, Meml[tempbuf], npix)
	    }
	    # Free space.
	    call mfree (tempbuf, TY_LONG)
	}

	# Write the final record.
	call wft_write_last_record (fits_fd, nrecords)
	if (short_header == YES || long_header == YES) {
	    call printf ("%d  Data logical (2880 byte) records written\n")
	        call pargi (nrecords)
	}
end


# WFT_GET_IMAGE_LINE -- Procedure to fetch the next image line.

int procedure wft_get_image_line (im, buf, v, datatype)

pointer	im			# IRAF image descriptor
pointer	buf			# pointer to image line
long	v[ARB]			# imio dimension descriptor
int	datatype		# IRAF image data type

int	npix
int	imgnll(), imgnlr(), imgnld(), imgnlx()
errchk	imgnll, imgnlr, imgnld, imgnlx

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	    npix = imgnll (im, buf, v)
	case TY_REAL:
	    npix = imgnlr (im, buf, v)
	case TY_DOUBLE:
	    npix = imgnld (im, buf, v)
	case TY_COMPLEX:
	    npix = imgnlx (im, buf, v)
	default:
	    call error (11, "GET_IMAGE_LINE: Unknown IRAF image type.")
	}

	return (npix)
end


# WFT_RSCALE_LINE -- This procedure converts the IRAF data to type real
# and scales by the FITS parameters bscale and bzero.

procedure wft_rscale_line (buf, outbuffer, npix, bscale, bzero, datatype)

pointer	buf			# pointer to IRAF image line
real	outbuffer[ARB]		# FITS integer buffer
int	npix			# number of pixels
double	bscale, bzero		# FITS bscale and bzero parameters
int	datatype		# data type of image

errchk	achtlr, altadr, amovr, achtdr, acthxr

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call achtlr (Meml[buf], outbuffer, npix)
	    call altadr (outbuffer, outbuffer, npix, bzero, bscale)
	case TY_REAL:
	    call amovr (Memr[buf], outbuffer, npix)
	    call altadr (outbuffer, outbuffer, npix, bzero, bscale)
	case TY_DOUBLE:
	    call achtdr (Memd[buf], outbuffer, npix)
	    call altadr (outbuffer, outbuffer, npix, bzero, bscale)
	case TY_COMPLEX:
	    call achtxr (Memx[buf], outbuffer, npix)
	    call altadr (outbuffer, outbuffer, npix, bzero, bscale)
	default:
	    call error (12, "WFT_RSCALE_LINE: Unknown IRAF image type.")
	}
end


# WFT_DSCALE_LINE -- This procedure converts the IRAF data to type double with
# after scaling by the FITS parameters bscale and bzero.

procedure wft_dscale_line (buf, outbuffer, npix, bscale, bzero, datatype)

pointer	buf			# pointer to IRAF image line
double	outbuffer[ARB]		# FITS integer buffer
int	npix			# number of pixels
double	bscale, bzero		# FITS bscale and bzero parameters
int	datatype		# data type of image

errchk	achtld, altad, amovd, achtrd, achtxd

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call achtld (Meml[buf], outbuffer, npix)
	    call altad (outbuffer, outbuffer, npix, bzero, bscale)
	case TY_REAL:
	    call achtrd (Memr[buf], outbuffer, npix)
	    call altad (outbuffer, outbuffer, npix, bzero, bscale)
	case TY_DOUBLE:
	    call amovd (Memd[buf], outbuffer, npix)
	    call altad (outbuffer, outbuffer, npix, bzero, bscale)
	case TY_COMPLEX:
	    call achtxd (Memx[buf], outbuffer, npix)
	    call altad (outbuffer, outbuffer, npix, bzero, bscale)
	default:
	    call error (12, "WFT_DSCALE_LINE: Unknown IRAF image type.")
	}
end


# WFT_LSCALE_LINE -- This procedure converts the IRAF data to type long with
# after scaling by the FITS parameters bscale and bzero.

procedure wft_lscale_line (buf, outbuffer, npix, bscale, bzero, datatype)

pointer	buf			# pointer to IRAF image line
long	outbuffer[ARB]		# FITS integer buffer
int	npix			# number of pixels
double	bscale, bzero		# FITS bscale and bzero parameters
int	datatype		# data type of image

errchk  altall, altarl, altadl, altaxl

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call altall (Meml[buf], outbuffer, npix, bzero, bscale)
	case TY_REAL:
	    call altarl (Memr[buf], outbuffer, npix, bzero, bscale)
	case TY_DOUBLE:
	    call altadl (Memd[buf], outbuffer, npix, bzero, bscale)
	case TY_COMPLEX:
	    call altaxl (Memx[buf], outbuffer, npix, bzero, bscale)
	default:
	    call error (12, "WFT_LSCALE_LINE: Unknown IRAF image type.")
	}
end


# WFT_REAL_LINE -- This procedure converts the IRAF image line to type long with
# no scaling.

procedure wft_real_line (buf, outbuffer, npix, datatype)

pointer	buf			# pointer to IRAF image line
real	outbuffer[ARB]		# buffer of FITS integers
int	npix			# number of pixels
int	datatype		# IRAF image datatype

errchk	achtlr, achtdr, amovr, achtxr

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call achtlr (Meml[buf], outbuffer, npix)
	case TY_REAL:
	    call amovr (Memr[buf], outbuffer, npix)
	case TY_DOUBLE:
	    call achtdr (Memd[buf], outbuffer, npix)
	case TY_COMPLEX:
	    call achtxr (Memx[buf], outbuffer, npix)
	default:
	    call error (13, "WFT_REAL_LINE: Unknown IRAF data type.")
	}
end


# WFT_DOUBLE_LINE -- This procedure converts the IRAF image line to type long
# with no scaling.

procedure wft_double_line (buf, outbuffer, npix, datatype)

pointer	buf			# pointer to IRAF image line
double  outbuffer[ARB]		# buffer of FITS integers
int	npix			# number of pixels
int	datatype		# IRAF image datatype

errchk	achtld, achtrd, amovd, achtxd

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call achtld (Meml[buf], outbuffer, npix)
	case TY_REAL:
	    call achtrd (Memr[buf], outbuffer, npix)
	case TY_DOUBLE:
	    call amovd (Memd[buf], outbuffer, npix)
	case TY_COMPLEX:
	    call achtxd (Memx[buf], outbuffer, npix)
	default:
	    call error (13, "WFT_DOUBLE_LINE: Unknown IRAF data type.")
	}
end


# WFT_LONG_LINE -- This procedure converts the IRAF image line to type long with
# no scaling.

procedure wft_long_line (buf, outbuffer, npix, datatype)

pointer	buf			# pointer to IRAF image line
long	outbuffer[ARB]		# buffer of FITS integers
int	npix			# number of pixels
int	datatype		# IRAF image datatype

errchk	amovl, achtrl, achtdl, achtxl

begin
	switch (datatype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call amovl (Meml[buf], outbuffer, npix)
	case TY_REAL:
	    call achtrl (Memr[buf], outbuffer, npix)
	case TY_DOUBLE:
	    call achtdl (Memd[buf], outbuffer, npix)
	case TY_COMPLEX:
	    call achtxl (Memx[buf], outbuffer, npix)
	default:
	    call error (13, "WFT_LONG_LINE: Unknown IRAF data type.")
	}
end


# ALTALL -- Procedure to linearly scale a long vector into a long vector
# using double precision constants to preserve precision.

procedure altall (a, b, npix, k1, k2)

long	a[ARB]		# input vector
long	b[ARB]		# output vector
int	npix		# number of pixels
double	k1, k2		# scaling factors

double	dtemp
int	i

begin
	do i = 1, npix {
	    dtemp = (a[i] + k1) * k2
	    if (dtemp >= 0.0d0)
		dtemp = dtemp + 0.5d0
	    else
		dtemp = dtemp - 0.5d0
	    b[i] = dtemp
	}
end


# ALTARL -- Procedure to linearly scale a real vector into a long vector
# using double precision constants to preserve precision.

procedure altarl (a, b, npix, k1, k2)

real	a[ARB]		# input vector
long	b[ARB]		# output vector
int	npix		# number of pixels
double	k1, k2		# scaling factors

int	i
double	dtemp

begin
	do i = 1, npix {
	    dtemp = (a[i] + k1) * k2
	    if (dtemp >= 0.0d0)
		dtemp = dtemp + 0.5d0
	    else
		dtemp = dtemp - 0.5d0
	    b[i] = dtemp
	}
end


# ALTADL -- Procedure to linearly scale a double vector into a long vector
# using double precision constants to preserve precision.

procedure altadl (a, b, npix, k1, k2)

double	a[ARB]		# input vector
long	b[ARB]		# output vector
int	npix		# number of pixels
double	k1, k2		# scaling factors

int	i
double	dtemp

begin
	do i = 1, npix {
	    dtemp = (a[i] + k1) * k2
	    if (dtemp >= 0.0d0)
		dtemp = dtemp + 0.5d0
	    else
		dtemp = dtemp - 0.5d0
	    b[i] = dtemp
	}
end


# ALTAXL -- Procedure to linearly scale a complex vector into a long vector
# using double precision constants to preserve precision.

procedure altaxl (a, b, npix, k1, k2)

complex	a[ARB]		# input vector
long	b[ARB]		# output vector
int	npix		# number of pixels
double	k1, k2		# scaling factors

int	i
double	dtemp

begin
	do i = 1, npix {
	    dtemp = (a[i] + k1) * k2
	    if (dtemp >= 0.0d0)
		dtemp = dtemp + 0.5d0
	    else
		dtemp = dtemp - 0.5d0
	    b[i] = dtemp
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


# ALTADX -- Procedure to linearly scale a complex vector in double precision

procedure altadx (a, b, npix, k1, k2)

complex	a[ARB]		# input vector
complex	b[ARB]		# output vector
int	npix		# number of pixels
double	k1, k2		# scaling factors

int	i

begin
	do i = 1, npix
	    b[i] = (a[i] + k1) * k2
end

