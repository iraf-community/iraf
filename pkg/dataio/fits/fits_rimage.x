# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include <mach.h>
include <fset.h>
include	"rfits.h"

# RFT_READ_IMAGE --  Read FITS image pixels to IRAF image file

procedure rft_read_image (fits_fd, fits, im)

int	fits_fd		# FITS file descriptor
pointer	fits		# FITS data structure
pointer	im		# IRAF image descriptor

int	i, npix, npix_record, blksize
long	v[IM_MAXDIM], nlines, il
pointer	tempbuf, buf
real	linemax, linemin

long	clktime()
int	fstati(), rft_init_read_pixels(), rft_read_pixels()
data	tempbuf /NULL/

errchk	malloc, mfree, rft_init_read_pixels, rft_read_pixels, rft_scale_pix
errchk	rft_change_pix, rft_put_image_line, rft_pix_limits

include	"rfits.com"

begin
	if (NAXIS(im) == 0) {
	    call printf ("Warning: No pixel file created\n")
	    return
	}

	# intialize
	call rft_set_image_header (fits, im)

	npix = NAXISN(im, 1)
	nlines = 1
	do i = 2, NAXIS(im)
	    nlines = nlines * NAXISN(im, i)

	IRAFMAX(im) = -MAX_REAL
	IRAFMIN(im) = MAX_REAL

	# FITS data is converted to type  LONG.  If BITPIX is not one
	# of the MII types then rft_read_pixels returns an ERROR.

	if (tempbuf != NULL)
	    call mfree (tempbuf, TY_LONG)
	call malloc (tempbuf, npix, TY_LONG)

	npix_record = len_record * FITS_BYTE / BITPIX(fits)
	i = rft_init_read_pixels (npix_record, BITPIX(fits), LSBF, TY_LONG)
	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, 2880) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1
	call amovkl (long(1), v, IM_MAXDIM)

	do il = 1, nlines {

	    # Write image line
	    call rft_put_image_line (im, buf, v, PIXTYPE(im))

	    # Read in image line
	    if (rft_read_pixels (fits_fd, Meml[tempbuf], npix,
	        NRECORDS(fits), blksize) != npix)
		call printf ("Error reading FITS data\n")

	    # Scale data
	    if (SCALE(fits) == YES)
	        call rft_scale_pix (Meml[tempbuf], buf, npix, FITS_BSCALE(fits),
		    FITS_BZERO(fits), PIXTYPE(im))
	    else
		call rft_change_pix (Meml[tempbuf], buf, npix, PIXTYPE(im))

	    # Map blanks
	    if (BLANKS(fits) == YES)
	        call rft_map_blanks (Meml[tempbuf], buf, npix, PIXTYPE(im),
		    BLANK_VALUE(fits), blank, NBPIX(im))

	    # Calculate image maximum and minimum
	    call rft_pix_limits (buf, npix, PIXTYPE(im), linemin, linemax)
	    IRAFMAX(im) = max (IRAFMAX(im), linemax)
	    IRAFMIN(im) = min (IRAFMIN(im), linemin)
	}

	LIMTIME(im) = clktime (long(0))

	if (NBPIX (im) != 0) {
	    call printf ("Warning: %d bad pixels in image\n")
		call pargl (NBPIX (im))
	}
end


# RFT_SET_IMAGE_HEADER -- Set remaining header fields not set in
# rft_read_header.

procedure rft_set_image_header (fits, im)

pointer	fits		# FITS data structure
pointer	im		# IRAF image pointer

int	precision
errchk	rft_set_precision
include	"rfits.com"

begin
	# Determine data type from BITPIX if user data type not specified.

	if (data_type == ERR) {
	    if (SCALE(fits) == YES) {
		call rft_set_precision (BITPIX(fits), precision)
		# if (precision <= NDIGITS_RP)
		    PIXTYPE(im) = TY_REAL
		# else
		    # PIXTYPE(im) = TY_DOUBLE
	    } else {
	        if (BITPIX(fits) <= SZ_SHORT * SZB_CHAR * NBITS_BYTE)
		    PIXTYPE(im) = TY_SHORT
		else
		    PIXTYPE(im) = TY_LONG
	    }

	} else
	    PIXTYPE(im) = data_type
end


# RFT_SET_PRECISION -- Procedure to determine the precision of the FITS data
# type.

procedure rft_set_precision (bitpix, precision)

int	bitpix			# FITS bits per pixel
int	precision		# FITS decimal digits of precision

begin
	switch (bitpix) {
	case FITS_BYTE:
	    precision = FITSB_PREC
	case FITS_SHORT:
	    precision = FITSS_PREC
	case FITS_LONG:
	    precision = FITSL_PREC
	default:
	    call error (16, "RFT_SET_PRECISION: Unknown FITS type")
	}
end


# RFT_MAP_BLANKS -- Map the blank pixels.  Currently only the number of blank
# pixels is determined without an further mapping.

procedure rft_map_blanks (a, buf, npts, pixtype, blank_value, blank, nbadpix)

long	a[ARB]		# integer input buffer
pointer	buf		# pointer to output image buffer
int	npts		# number of points
int	pixtype		# image data type
long	blank_value	# FITS blank value
real	blank		# user blank value
long	nbadpix		# number of bad pixels

int	i

begin
	# Do blank mapping here
	switch (pixtype) {
	case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	    do i = 1, npts {
	        if (a[i] == blank_value) {
		    nbadpix = nbadpix + 1
		    Meml[buf+i-1] = blank
		}
	    }
	case TY_REAL:
	    do i = 1, npts {
	        if (a[i] == blank_value) {
		    nbadpix = nbadpix + 1
		    Memr[buf+i-1] = blank
		}
	    }
	case TY_DOUBLE:
	    do i = 1, npts {
	        if (a[i] == blank_value) {
		    nbadpix = nbadpix + 1
		    Memd[buf+i-1] = blank
		}
	    }
	case TY_COMPLEX:
	    do i = 1, npts {
	        if (a[i] == blank_value) {
		    nbadpix = nbadpix + 1
		    Memx[buf+i-1] = blank
		}
	    }
	}
end


# RFT_PUT_IMAGE_LINE -- Procedure to output an image line to and IRAF file.

procedure rft_put_image_line (im, buf, v, data_type)

pointer	im			# IRAF image descriptor
pointer	buf			# Pointer to output image line
long	v[ARB]			# imio pointer
int	data_type		# output pixel type

int	impnll(), impnlr(), impnld(), impnlx()
errchk	impnll, impnlr, impnld, impnlx

begin
	switch (data_type) {
	case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	    if (impnll (im, buf, v) == EOF)
		call error (3, "RFT_PUT_IMAGE_LINE: Error writing FITS data")
	case TY_REAL:
	    if (impnlr (im, buf, v) == EOF)
		call error (3, "RFT_PUT_IMAGE_LINE: Error writing FITS data")
	case TY_DOUBLE:
	    if (impnld (im, buf, v) == EOF)
		call error (3, "RFT_PUT_IMAGE_LINE: Error writing FITS data")
	case TY_COMPLEX:
	    if (impnlx (im, buf, v) == EOF)
		call error (3, "RFT_PUT_IMAGE_LINE: Error writing FITS data")
	default:
	    call error (10, "RFT_PUT_IMAGE_LINE: Unsupported IRAF image type")
	}
end


# RFT_SCALE_PIX -- Procedure to convert an IRAF image line from type long
# to the requested output data type with optional scaling using the
# FITS parameters BSCALE and BZERO.

procedure rft_scale_pix (inbuf, outbuf, npix, bscale, bzero, data_type)

long	inbuf[ARB]		# buffer of FITS integers
pointer	outbuf			# pointer to output image line
int	npix			# number of pixels
double	bscale, bzero		# FITS bscale and bzero
int	data_type		# IRAF image pixel type

errchk	achtll, achtlr, achtld, achtlx
errchk	altml, altmr, altmd, altmx

begin
	switch (data_type) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    call achtll (inbuf, Meml[outbuf], npix)
	    call altml (Meml[outbuf], Meml[outbuf], npix, bscale, bzero)
	case TY_REAL:
	    call achtlr (inbuf, Memr[outbuf], npix)
	    #call altmr (Memr[outbuf], Memr[outbuf], npix, real (bscale),
	        #real (bzero))
	    call altmdr (Memr[outbuf], Memr[outbuf], npix, bscale, bzero)
	case TY_DOUBLE:
	    call achtld (inbuf, Memd[outbuf], npix)
	    call altmd (Memd[outbuf], Memd[outbuf], npix, bscale, bzero)
	case TY_COMPLEX:
	    call achtlx (inbuf, Memx[outbuf], npix)
	    call altmx (Memx[outbuf], Memx[outbuf], npix, real (bscale),
	        real (bzero))
	default:
	    call error (10, "RFT_SCALE_LINE: Illegal IRAF image type")
	}
end

# ALTMDR -- procedure to scale lines

procedure altmdr (a, b, npix, bscale, bzero)

real	a[ARB]		# input array
real	b[ARB]		# output array
int	npix		# number of pixels
double	bscale, bzero	# scaling parameters

int	i

begin
	do i = 1, npix
	    b[i] = a[i] * bscale + bzero
end

# RFT_CHANGE_PIX -- Procedure to change a line of long integers to the
# IRAF image type.

procedure rft_change_pix (inbuf, outbuf, npix, data_type)

long	inbuf[ARB]		# array of FITS integers
pointer	outbuf			# pointer to IRAF image line
int	npix			# number of pixels
int	data_type		# IRAF pixel type

begin
	switch (data_type) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    call achtll (inbuf, Meml[outbuf], npix)
	case TY_REAL:
	    call achtlr (inbuf, Memr[outbuf], npix)
	case TY_DOUBLE:
	    call achtld (inbuf, Memd[outbuf], npix)
	case TY_COMPLEX:
	    call achtlx (inbuf, Memx[outbuf], npix)
	default:
	    call error (10, "RFT_CHANGE_LINE: Illegal IRAF image type")
	}
end


# RFT_PIX_LIMITS -- Procedure to determine to maxmimum and minimum values in a
# line.

procedure rft_pix_limits (buf, npix, pixtype, linemin, linemax)

pointer	buf				# pointer to IRAF image line
int	npix				# number of pixels
int	pixtype				# output data type
real	linemax, linemin		# min and max pixel values

long	lmax, lmin
real	rmax, rmin
double	dmax, dmin
complex	xmax, xmin

begin
	switch (pixtype) {
	case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	    call aliml (Meml[buf], npix, lmin, lmax)
	    linemax = lmax
	    linemin = lmin
	case TY_REAL:
	    call alimr (Memr[buf], npix, rmin, rmax)
	    linemax = rmax
	    linemin = rmin
	case TY_DOUBLE:
	    call alimd (Memd[buf], npix, dmin, dmax)
	    linemax = dmax
	    linemin = dmin
	case TY_COMPLEX:
	    call alimx (Memx[buf], npix, xmin, xmax)
	    linemax = xmax
	    linemin = xmin
	default:
	    call error (30, "RFT_PIX_LIMITS: Unknown IRAF type")
	}
end
