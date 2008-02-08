include <imio.h>
include	<imhdr.h>
include <mach.h>
include <fset.h>
include	"rfits.h"

# RFT_READ_IMAGE --  Read FITS image pixels to IRAF image file

procedure rft_read_image (fits_fd, fits, im)

int	fits_fd		# FITS file descriptor
pointer	fits		# FITS data structure
pointer	im		# IRAF image descriptor

int	i, npix, npix_record, blksize, gn
long	v[IM_MAXDIM], nlines, il
pointer	tempbuf, buf, ext
real	linemax, linemin, rmax, rmin, datamin, datamax

long	clktime()
int	gi_gstfval()
int	fstati(), rft_init_read_pixels(), rft_read_pixels(), open()
int	rft_ieee_read(), tab_read_header(), noop
data	tempbuf /NULL/

errchk	malloc, mfree, rft_init_read_pixels, rft_read_pixels, rft_scale_pix
errchk	rft_change_pix, rft_put_image_line, rft_pix_limits, rft_ieee_read
errchk	gi_update

include	"rfits.com"

begin
	if (NAXIS(fits) == 0) {
#	    call printf ("Warning: No pixel file created\n")
	    return
	}

	# intialize
	call rft_set_image_header (fits, im)

	rmax = -MAX_REAL
	rmin = MAX_REAL

	# FITS data is converted to type  LONG.  If BITPIX is not one
	# of the MII types then rft_read_pixels returns an ERROR.

	npix_record = len_record * FITS_BYTE / BITPIX(fits)
	if (ieee == YES) {
	   if (PIXTYPE(im) == TY_REAL)
	     i = rft_init_read_pixels (npix_record, BITPIX(fits), LSBF, TY_REAL)
	   else
	     i=rft_init_read_pixels (npix_record, BITPIX(fits), LSBF, TY_DOUBLE)
	} else
	   i = rft_init_read_pixels (npix_record, BITPIX(fits), LSBF, TY_LONG)

	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, 2880) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1
	

	# If the user set xdimtogf we want to reset some of the stf
	# descriptor to allow for a new image to have a slot for the gpb
	# but not write in it until all the groups are in the pixel
	# file. 
	if (gkey == TO_MG) {
	   call gi_pstfval (im, "GCOUNT", GCOUNT(fits))
	   if (GCOUNT(fits) == 0) GCOUNT(fits) = 1
	   call gi_pstfval (im, "PSIZE", OPSIZE(fits))
	   call gi_reset (im)
	   # Reset the NAXIS(fits) value just for printing statistic on the
	   # user terminal.
	   if (GCOUNT(fits) > 1)
	      NAXIS(fits) = NAXIS(fits) - 1
	} else
	   GCOUNT(fits) = 1

	npix = IM_LEN(im, 1)
	nlines = 1
	do i = 2, IM_NDIM(im)
	    nlines = nlines * IM_LEN(im, i)

	if (ieee == NO) {
	   if (tempbuf != NULL)
	      call mfree (tempbuf, TY_LONG)
	   call malloc (tempbuf, npix, TY_LONG)
        }

        do gn = 1, GCOUNT(fits) {
	   call amovkl (long(1), v, IM_MAXDIM)

	   do il = 1, nlines {
	      # Write image line
	      call rft_put_image_line (im, buf, v, PIXTYPE(im))

	      # Read in image line
	      if (ieee == YES) {
	         if (rft_ieee_read(fits_fd, buf, npix, NRECORDS(fits), 
				 blksize) != npix)
	  	    call printf ("Error reading FITS data\n")
	      } else {
	         if (rft_read_pixels (fits_fd, Meml[tempbuf], npix,
	                              NRECORDS(fits), blksize) != npix)
	  	     call printf ("Error reading FITS data\n")

	         # Scale data
	         if (SCALE(fits) == YES)
	             call rft_scale_pix (Meml[tempbuf], buf, npix,
			  FITS_BSCALE(fits), FITS_BZERO(fits), PIXTYPE(im))
	         else
		     call rft_change_pix (Meml[tempbuf], buf, npix, PIXTYPE(im))
	         # Map blanks
	         if (BLANKS(fits) == YES)
	             call rft_map_blanks (Meml[tempbuf], buf, npix, PIXTYPE(im),
		         BLANK_VALUE(fits), blank, NBPIX(im))
	      }
	      # Calculate image maximum and minimum
	      call rft_pix_limits (buf, npix, PIXTYPE(im), linemin, linemax)
	      rmax = max (rmax, linemax)
	      rmin = min (rmin, linemin)

	      if (NBPIX (im) != 0) {
	          call printf ("Warning: %d bad pixels in image\n")
		    call pargl (NBPIX (im))
	      }

	   }
           if (gn < GCOUNT(fits)) {
	       # Tell stf not to write gpb values into the pixels
	       # file since they are not available yet.
	       call gi_pstfval (im, "PSIZE", 0)
	       call gi_newgrp (im, gn+1, datamin, datamax, 0)
           }
        }
        call imflush(im)

	# Now if the user has chosen the xdimtogf flag we want to read
	# the attached table and put the values straight into the gpb
	# part of the data file.
        if (gkey == TO_MG) {
	   # Due to problems with imunmap we need to do the 2 things below.
           call close(IM_PFD(im))
	   IM_PFD(im) = open (IM_PIXFILE(im), READ_WRITE, BINARY_FILE)

	   #Do not read a table if OPSIZE is zero (same as is PCOUNT =0)
	   if (OPSIZE(fits) != 0) {
	      call calloc (ext, LEN_EXTENSION, TY_STRUCT)
              if (tab_read_header (fits_fd, im, ext, noop, fits) == EOF)
		  call error (2, "RFT_READ_IMAGE: EOF while reading gpb table")
              # Reset value of PSIZE to the real one since rgi_get_table_val
	      # will use it to calculate the size of the gpb.
	      call gi_pstfval (im,"PSIZE", OPSIZE(fits))
              call rgi_read_tfits (fits_fd, im, GCOUNT(fits), ext)
	      call ext_free(ext)
	   }
	   call gi_update (im)
           EXTEND(fits) = NO
	   BITPIX(fits) = gi_gstfval(im, "BITPIX")
        }
	if (ieee == NO)
	   call mfree (tempbuf, TY_LONG)
	IRAFMAX(im) = rmax
	IRAFMIN(im) = rmin
	LIMTIME(im) = clktime(long(0))
end

define	SZ_KEYWORD	8
# RFT_SET_IMAGE_HEADER -- Set remaining header fields not set in
# rft_read_header.

procedure rft_set_image_header (fits, im)

pointer	fits		# FITS data structure
pointer	im		# IRAF image pointer

int	strcmp()

include	"rfits.com"


begin
	# Determine data type from BITPIX if user data type not specified.

	if (data_type == ERR) {
	    if (SCALE(fits) == YES) {
		PIXTYPE(im) = TY_REAL
		# If bitpix is 64 then is a ieee FITS datatype.
		if (BITPIX(fits) == SZ_DOUBLE * SZB_CHAR * NBITS_BYTE)
		    PIXTYPE(im) = TY_DOUBLE
	    } else {
	        if (BITPIX(fits) <= SZ_SHORT * SZB_CHAR * NBITS_BYTE)
		    PIXTYPE(im) = TY_SHORT
		else
		    PIXTYPE(im) = TY_LONG
		if (ieee == YES) {
	           if (BITPIX(fits) <= SZ_LONG * SZB_CHAR * NBITS_BYTE)
		       PIXTYPE(im) = TY_REAL
		   else
		       PIXTYPE(im) = TY_DOUBLE
		}
		# Get IRAFTYPE keyword value to check for unsigned input
		# fits file.
		if (strcmp (FITSTYPE(fits), "USHORT") == 0) {
		   PIXTYPE(im) = TY_USHORT
		}
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
long	v[IM_MAXDIM]			# imio pointer
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
errchk	altml, altmlr, altmr, altmd, altmx

begin
	switch (data_type) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    call achtll (inbuf, Meml[outbuf], npix)
	    call altml (Meml[outbuf], Meml[outbuf], npix, bscale, bzero)
	case TY_REAL:
	    call altmlr (inbuf, Memr[outbuf], npix, bscale, bzero)
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

# ALTMLR -- procedure to scale lines

procedure altmlr (a, b, npix, bscale, bzero)

int	a[ARB]		# input array
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
