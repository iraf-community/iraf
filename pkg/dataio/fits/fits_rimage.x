# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include <mach.h>
include <fset.h>
include	"rfits.h"

# RFT_READ_IMAGE --  Convert FITS image pixels to IRAF image pixels.

procedure rft_read_image (fits_fd, fits, im)

int	fits_fd		# FITS file descriptor
pointer	fits		# FITS data structure
pointer	im		# IRAF image descriptor

int	i, npix, npix_record, blksize, ndummy
long	v[IM_MAXDIM], nlines, il
pointer	tempbuf, buf
real	linemax, linemin, lirafmin, lirafmax
double	dblank

long	clktime()
int	fstati(), rft_init_read_pixels(), rft_read_pixels()

errchk	malloc, mfree, rft_init_read_pixels, rft_read_pixels, rft_lscale_pix
errchk	rft_lchange_pix, rft_rchange_pix, rfit_dchange_pix, rft_put_image_line
errchk	rft_pix_limits, rft_rscale_pix, rft_dscale_pix

include	"rfits.com"

begin
	# No pixel file was created.
	if (NAXIS(im) == 0) {
	    call printf ("Warning: No pixel file created\n")
	    return
	}

	# Initialize the header.
	call rft_set_image_header (fits, im)

	# Compute the number of columns and lines in the image.
	npix = NAXISN(im, 1)
	nlines = 1
	do i = 2, NAXIS(im)
	    nlines = nlines * NAXISN(im, i)
	lirafmax = -MAX_REAL
	lirafmin = MAX_REAL

	# Compute the number of pixels per record and the number of records
	# per output block.

	npix_record = len_record * FITS_BYTE / abs (BITPIX(fits))
	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, FITS_RECORD) == 0)
	    blksize = blksize / FITS_RECORD
	else
	    blksize = 1

	# FITS data is converted to type  LONG, REAL or DOUBLE. If BITPIX is
	# not one of the MII types then rft_read_pixels returns an ERROR.

	call amovkl (long(1), v, IM_MAXDIM)
	switch (BITPIX(fits)) {
	case FITS_REAL:

	    # Allocate temporary space.
	    call malloc (tempbuf, npix, TY_REAL)

	    # Initialize the read.
	    i = rft_init_read_pixels (npix_record, BITPIX(fits), LSBF, TY_REAL)

	    # Turn on the ieee NaN mapping.
	    call ieesnanr (blank)
	    #call ieemapr (YES, NO)
	    #call ieezstatr ()
	    NBPIX(im) = 0

	    # Allocate the space for the output line, read in the image
	    # line, convert from the ieee to native format, and compute the
	    # minimum and maximum.

	    do il = 1, nlines {
	        call rft_put_image_line (im, buf, v, PIXTYPE(im))
	        if (rft_read_pixels (fits_fd, Memr[tempbuf], npix,
	            NRECORDS(fits), blksize) != npix)
		    call printf ("Error reading FITS data\n")
	        if (SCALE(fits) == YES)
	            call rft_rscale_pix (Memr[tempbuf], buf, npix,
		        FITS_BSCALE(fits), FITS_BZERO(fits), PIXTYPE(im))
		else
		    call rft_rchange_pix (Memr[tempbuf], buf, npix, PIXTYPE(im))
	        call rft_pix_limits (buf, npix, PIXTYPE(im), linemin, linemax)
	        lirafmax = max (lirafmax, linemax)
	        lirafmin = min (lirafmin, linemin)
	    }

	    # Set the number of bad pixels.
	    call ieestatr (NBPIX(im), ndummy)

	    # Free space.
	    call mfree (tempbuf, TY_REAL)

	case FITS_DOUBLE:

	    # Allocate temporary space.
	    call malloc (tempbuf, npix, TY_DOUBLE)

	    # Initialize the read.
	    i = rft_init_read_pixels (npix_record, BITPIX(fits), LSBF,
	        TY_DOUBLE)

	    # Turn on the ieee NaN mapping.
	    dblank = blank
	    call ieesnand (dblank)
	    #call ieemapd (YES, NO)
	    #call ieezstatd ()
	    NBPIX(im) = 0

	    # Allocate the space for the output line, read in the image
	    # line, convert from the ieee to native format, and compute the
	    # minimum and maximum.

	    do il = 1, nlines {
	        call rft_put_image_line (im, buf, v, PIXTYPE(im))
	        if (rft_read_pixels (fits_fd, Memd[tempbuf], npix,
	            NRECORDS(fits), blksize) != npix)
		    call printf ("Error reading FITS data\n")
	        if (SCALE(fits) == YES)
	            call rft_dscale_pix (Memd[tempbuf], buf, npix,
		        FITS_BSCALE(fits), FITS_BZERO(fits), PIXTYPE(im))
		else
		    call rft_dchange_pix (Memd[tempbuf], buf, npix, PIXTYPE(im))
	        call rft_pix_limits (buf, npix, PIXTYPE(im), linemin, linemax)
		if (IS_INDEFR(linemax))
		    lirafmax = INDEFR
		else
	            lirafmax = max (lirafmax, linemax)
		if (IS_INDEFR(linemin))
		    lirafmin = INDEFR
		else
	            lirafmin = min (lirafmin, linemin)
	    }

	    # Set the number of bad pixels.
	    call ieestatd (NBPIX(im), ndummy)

	    # Free space.
	    call mfree (tempbuf, TY_DOUBLE)

	default:

	    # Allocate the required space.
	    call malloc (tempbuf, npix, TY_LONG)

	    # Allocate the space for the output line, read in the image
	    # line, convert from the ieee to native format, and compute the
	    # minimum and maximum.

	    i = rft_init_read_pixels (npix_record, BITPIX(fits), LSBF, TY_LONG)
	    do il = 1, nlines {
	        call rft_put_image_line (im, buf, v, PIXTYPE(im))
	        if (rft_read_pixels (fits_fd, Meml[tempbuf], npix,
	            NRECORDS(fits), blksize) != npix)
		    call printf ("Error reading FITS data\n")
	        if (SCALE(fits) == YES)
	            call rft_lscale_pix (Meml[tempbuf], buf, npix,
		        FITS_BSCALE(fits), FITS_BZERO(fits), PIXTYPE(im))
	        else
		    call rft_lchange_pix (Meml[tempbuf], buf, npix, PIXTYPE(im))
	        if (BLANKS(fits) == YES)
	            call rft_map_blanks (Meml[tempbuf], buf, npix, PIXTYPE(im),
		        BLANK_VALUE(fits), blank, NBPIX(im))
	        call rft_pix_limits (buf, npix, PIXTYPE(im), linemin, linemax)
	        lirafmax = max (lirafmax, linemax)
	        lirafmin = min (lirafmin, linemin)
	    }

	    # Free space.
	    call mfree (tempbuf, TY_LONG)
	}

	IRAFMIN(im) = lirafmin
	IRAFMAX(im) = lirafmax
	LIMTIME(im) = clktime (long(0))

	if (NBPIX (im) != 0) {
	    call printf ("Warning: %d bad pixels replaced in image\n")
		call pargl (NBPIX (im))
	}
	if (IS_INDEFR(lirafmax) || lirafmax > MAX_REAL) {
	    call printf ("Warning: image contains pixel values > %g\n")
		call pargr (MAX_REAL)
	}
	if (IS_INDEFR(lirafmin) || lirafmin < -MAX_REAL) {
	    call printf ("Warning: image contains pixel values < %g\n")
		call pargr (-MAX_REAL)
	}
end


# RFT_SET_IMAGE_HEADER -- Set remaining header fields not set in
# rft_read_header.

procedure rft_set_image_header (fits, im)

pointer	fits		# FITS data structure
pointer	im		# IRAF image pointer

include	"rfits.com"

begin
	# Determine data type from BITPIX if user data type not specified.

	if (data_type == ERR) {
	    if (BITPIX(fits) < 0) {
		if (abs (BITPIX(fits)) <= (SZ_REAL * SZB_CHAR * NBITS_BYTE))
		    PIXTYPE(im) = TY_REAL
		else
		    PIXTYPE(im) = TY_DOUBLE
	    } else if (SCALE(fits) == YES) {
		PIXTYPE(im) = TY_REAL
	    } else {
	        if (BITPIX(fits) <= (SZ_SHORT * SZB_CHAR * NBITS_BYTE))
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


# RFT_RSCALE_PIX -- Procedure to convert an IRAF image line from type real
# to the requested output data type with optional scaling using the
# FITS parameters BSCALE and BZERO.

procedure rft_rscale_pix (inbuf, outbuf, npix, bscale, bzero, data_type)

real	inbuf[ARB]		# buffer of FITS integers
pointer	outbuf			# pointer to output image line
int	npix			# number of pixels
double	bscale, bzero		# FITS bscale and bzero
int	data_type		# IRAF image pixel type

errchk	altmdr, achtrl, amovr, achtrd, achtrx

begin
	switch (data_type) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    call altmdr (inbuf, inbuf, npix, bscale, bzero)
	    call achtrl (inbuf, Meml[outbuf], npix)
	case TY_REAL:
	    call altmdr (inbuf, inbuf, npix, bscale, bzero)
	    call amovr (inbuf, Memr[outbuf], npix)
	case TY_DOUBLE:
	    call altmdr (inbuf, inbuf, npix, bscale, bzero)
	    call achtrd (inbuf, Memd[outbuf], npix)
	case TY_COMPLEX:
	    call altmdr (inbuf, inbuf, npix, bscale, bzero)
	    call achtrx (inbuf, Memx[outbuf], npix)
	default:
	    call error (10, "RFT_SCALE_LINE: Illegal IRAF image type")
	}
end


# RFT_DSCALE_PIX -- Procedure to convert an IRAF image line from type double
# to the requested output data type with optional scaling using the
# FITS parameters BSCALE and BZERO.

procedure rft_dscale_pix (inbuf, outbuf, npix, bscale, bzero, data_type)

double	inbuf[ARB]		# buffer of FITS integers
pointer	outbuf			# pointer to output image line
int	npix			# number of pixels
double	bscale, bzero		# FITS bscale and bzero
int	data_type		# IRAF image pixel type

errchk	altmd, achtdl, amovd, achtdr, achtdx

begin
	switch (data_type) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    call altmd (inbuf, inbuf, npix, bscale, bzero)
	    call achtdl (inbuf, Meml[outbuf], npix)
	case TY_REAL:
	    call altmd (inbuf, inbuf, npix, bscale, bzero)
	    call achtdr (inbuf, Memr[outbuf], npix)
	case TY_DOUBLE:
	    call altmd (inbuf, inbuf, npix, bscale, bzero)
	    call amovd (inbuf, Memd[outbuf], npix)
	case TY_COMPLEX:
	    call altmd (inbuf, inbuf, npix, bscale, bzero)
	    call achtdx (inbuf, Memx[outbuf], npix)
	default:
	    call error (10, "RFT_SCALE_LINE: Illegal IRAF image type")
	}
end



# RFT_LSCALE_PIX -- Procedure to convert an IRAF image line from type long
# to the requested output data type with optional scaling using the
# FITS parameters BSCALE and BZERO.

procedure rft_lscale_pix (inbuf, outbuf, npix, bscale, bzero, data_type)

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
	    call altmlr (inbuf, Memr[outbuf], npix, bscale, bzero)
	    #call achtlr (inbuf, Memr[outbuf], npix)
	    #call altmdr (Memr[outbuf], Memr[outbuf], npix, bscale, bzero)
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


# RFT_RCHANGE_PIX -- Procedure to change a line of real numbers to the
# IRAF image type.

procedure rft_rchange_pix (inbuf, outbuf, npix, data_type)

real	inbuf[ARB]		# array of FITS integers
pointer	outbuf			# pointer to IRAF image line
int	npix			# number of pixels
int	data_type		# IRAF pixel type

errchk	achtrl, amovr, achtrd, achtrx

begin
	switch (data_type) {
	case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	    call achtrl (inbuf, Meml[outbuf], npix)
	case TY_REAL:
	    call amovr (inbuf, Memr[outbuf], npix)
	case TY_DOUBLE:
	    call achtrd (inbuf, Memd[outbuf], npix)
	case TY_COMPLEX:
	    call achtrx (inbuf, Memx[outbuf], npix)
	default:
	    call error (10, "RFT_RCHANGE_LINE: Illegal IRAF image type")
	}
end


# RFT_DCHANGE_PIX -- Procedure to change a line of double precision numbers
# to the IRAF image type.

procedure rft_dchange_pix (inbuf, outbuf, npix, data_type)

double  inbuf[ARB]		# array of FITS integers
pointer	outbuf			# pointer to IRAF image line
int	npix			# number of pixels
int	data_type		# IRAF pixel type

errchk	achtdl, achtdr, amovd, achtdx

begin
	switch (data_type) {
	case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	    call achtdl (inbuf, Meml[outbuf], npix)
	case TY_REAL:
	    call achtdr (inbuf, Memr[outbuf], npix)
	case TY_DOUBLE:
	    call amovd (inbuf, Memd[outbuf], npix)
	case TY_COMPLEX:
	    call achtdx (inbuf, Memx[outbuf], npix)
	default:
	    call error (10, "RFT_DCHANGE_LINE: Illegal IRAF image type")
	}
end



# RFT_LCHANGE_PIX -- Procedure to change a line of long integers to the
# IRAF image type.

procedure rft_lchange_pix (inbuf, outbuf, npix, data_type)

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


# RFT_PIX_LIMITS -- Procedure to determine to maxmimum and minimum values in a
# line. Note that double precision is somewhat of a special case because
# MAX_DOUBLE is a lot less than the maximum permitted ieee numbers for iraf.

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
	    if (dmax > MAX_REAL)
	        linemax = INDEFR
	    else
		linemax = dmax
	    if (dmin < -MAX_REAL)
		linemin = INDEFR
	    else
	        linemin = dmin
	case TY_COMPLEX:
	    call alimx (Memx[buf], npix, xmin, xmax)
	    linemax = xmax
	    linemin = xmin
	default:
	    call error (30, "RFT_PIX_LIMITS: Unknown IRAF type")
	}
end


# ALTMDR -- procedure to scale a long vector into a real vector using
# double precision constants to preserve accuracy

procedure altmlr (a, b, npix, bscale, bzero)

long	a[ARB]		# input array
real	b[ARB]		# output array
int	npix		# number of pixels
double	bscale, bzero	# scaling parameters

int	i

begin
	do i = 1, npix
	    b[i] = a[i] * bscale + bzero
end


# ALTMDR -- procedure to scale a real vector with double precision constants.

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
