include <imio.h>
include <imhdr.h>
include <mach.h>
include "wfits.h"
include "dfits.h"

define	FITS_REAL	32
define	FITS_DOUBLE	64
define	MIN_DOUBLE 	(10.0 / MAX_DOUBLE)

# WFT_SCALE_PAR -- Procedure to  establish the scale parameter BSCALE and BZERO
# according to the user input data and the parameter setup. Will scale data
# only if input is floating point. If ieee is chosen then the user supplied
# BSCALE and BZERO are disabled.

procedure wft_scale_par (im, fits)

pointer	im		# pointer to the IRAF image
pointer	fits		# pointer to the FITS structure

int	sizeof(), wft_set_bitpix()
errchk	wft_set_scale, wft_set_bitpix
errchk	wft_fits_set_scale

define ieee_ 99
include "wfits.com"

begin
	DATA_BITPIX(fits) = sizeof (PIXTYPE(im)) * SZB_CHAR * NBITS_BYTE

	FITS_BITPIX(fits) = wft_set_bitpix (bitpix, PIXTYPE(im),
	    DATA_BITPIX(fits))

	# If we have a header file only, do not calculate.
	if (IM_NDIM(im) == 0) return
ieee_
	if (ieee == YES) {
	    if (PIXTYPE(im) == TY_DOUBLE) {
		FITS_BITPIX(fits) = FITS_DOUBLE

	    } else  if (PIXTYPE(im) == TY_REAL) {
		FITS_BITPIX(fits) = FITS_REAL
	    
	    } else {
		# Convert to ieee if set and only if the image data type is
		# floating; otherwise reset the ieee flag.

		ieee = NO
		scale = NO	
		bzero = 0.0d0
		bscale = 1.0d0
	    }
	}

	# Calculate the FITS bscale and bzero parameters.

	if (ieee == NO) {
	   if (autoscale == YES) {
	      call wft_get_tape_limits (FITS_BITPIX(fits), TAPEMIN(fits),
	                                TAPEMAX(fits))
	      call wft_data_limits (im, IRAFMIN(fits), IRAFMAX(fits))
              if (abs(IRAFMIN(fits)) > INDEFR || abs(IRAFMAX(fits)) > INDEFR) {
		call flush(STDOUT)
		call eprintf("\n***** DATAMIN and DATAMAX are out of range,\n")
		call eprintf("***** force IEEE standard for this file.\n")
		# If there is a log file defined,  write this message.
		call put_in_log (
			"\n***** DATAMIN and DATAMAX are out of range,\n")
		call put_in_log (
			"***** force IEEE standard for this file.\n")
		ieee = YES
		autoscale = NO
		goto ieee_
	      }
	      call wft_fits_set_scale (im, DATA_BITPIX(fits), FITS_BITPIX(fits),
	             IRAFMIN(fits), IRAFMAX(fits), TAPEMIN(fits), TAPEMAX(fits),
		     SCALE(fits), BSCALE(fits), BZERO(fits))
	   } else {
	      IRAFMIN(fits) = IM_MIN(im)
	      IRAFMAX(fits) = IM_MAX(im)
	      SCALE(fits) = scale
	      BZERO(fits) = bzero
	      BSCALE(fits) = bscale
	   }
	} else {
	      IRAFMIN(fits) = IM_MIN(im)
	      IRAFMAX(fits) = IM_MAX(im)
	      SCALE(fits) = NO
	      BZERO(fits) = 0.0
	      BSCALE(fits) = 1.0
	}
end

# WFT_SET_BITPIX -- This procedure sets the FITS bitpix for each image based on
# either the user given value or the precision of the IRAF data.	

int procedure wft_set_bitpix (bitpix, datatype, data_bitpix)

int	bitpix		# the user defined FITS bits per pixel, ERR if
			# not legal FITS type
int	datatype	# the IRAF image data type
int	data_bitpix	# the bits per pixel in the data

begin
	if (bitpix == ERR) {
	    switch (datatype) {
	    case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	        if (data_bitpix <= FITS_BYTE)
		    return (FITS_BYTE)
	        else if (data_bitpix <= FITS_SHORT)
		    return (FITS_SHORT)
	        else
		    return (FITS_LONG)
	    case TY_REAL, TY_COMPLEX:
		if (NDIGITS_RP <= BYTE_PREC)
		    return (FITS_BYTE)
		else if (NDIGITS_RP <= SHORT_PREC)
		    return (FITS_SHORT)
		else
		    return (FITS_LONG)
	    case TY_DOUBLE:
		if (NDIGITS_DP <= BYTE_PREC)
		    return (FITS_BYTE)
		else if (NDIGITS_DP <= SHORT_PREC)
		    return (FITS_SHORT)
		else
		    return (FITS_LONG)
	    default:
		call flush (STDOUT)
		call error (2, "wft_set_bitpix: Unknown IRAF data type.")
	    }
	} else
	    return (bitpix)
end

# WFT_FITS_SET_SCALE -- Procedure to set the FITS scaling parameters if
# autoscaling is enabled.

procedure wft_fits_set_scale (im, data_bitpix, fits_bitpix, irafmin, irafmax,
	tapemin, tapemax, scale, bscale, bzero )

pointer	im		# pointer to IRAF image
int	data_bitpix	# bits per pixel of data
int	fits_bitpix	# fits bits per pixel
real	irafmin		# minimum picture value
real	irafmax		# maximum picture value
double	tapemin		# minimum tape value
double	tapemax		# maximum tape value
int	scale		# scale data ?
double	bscale		# FITS bscale
double	bzero		# FITS bzero

errchk	wft_set_scale

begin
	switch (PIXTYPE(im)) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    if (data_bitpix > fits_bitpix) {
		scale = YES
		call wft_set_scale (fits_bitpix, irafmin, irafmax, tapemin, 
		    tapemax, bscale, bzero)
	    } else {
		scale = NO
		bscale = 1.0d0
		bzero = 0.0d0
	    }
	case TY_REAL, TY_DOUBLE, TY_COMPLEX:
	    scale = YES
	    call wft_set_scale (fits_bitpix, irafmin, irafmax, tapemin, tapemax,
	        bscale, bzero)
	default:
	    call flush(STDOUT)
	    call error (1, "wft_fits_set_scale: Unknown IRAF image type.")
	}

end


# WFT_SET_SCALE -- This procedure calculates bscale and bzero for each frame
# from the FITS bitpix and the maximum and minimum data values.

procedure wft_set_scale (fits_bitpix, datamin, datamax, mintape, maxtape,
	bscale, bzero)

int	fits_bitpix		# the FITS integer bits per pixels
real	datamax, datamin	# the IRAF image data minimum and maximum
double	mintape, maxtape	# min and max FITS tape values
double	bscale, bzero		# the calculated bscale and bzero values

double	maxdata, mindata, num, denom

bool	rft_equald()

begin
	# calculate the maximum and minimum values in the data
	maxdata = datamax + abs ((datamax / (10.0 ** (NDIGITS_RP - 1))))
	mindata = datamin - abs ((datamin / (10.0 ** (NDIGITS_RP - 1))))
	denom =  maxtape - mintape
	num = maxdata - mindata
	#denom = denom - denom / (1.0d1 ** (NDIGITS_RP - 1))
	#num = num + num / (1.0d1 ** (NDIGITS_RP - 1))

	# check for constant image case
	if (rft_equald (num, 0.0d0)) {
	    bscale = 1.0d0
	    bzero = maxdata
	} else {
	    bscale = num / denom
	    if (bscale == 0.0) {
		bscale = MIN_DOUBLE
		denom = num/bscale
		call printf ("\7\7 Data range too small, loss of ")
		call printf ("precision might occur\n")
		call flush (STDOUT)
	    }
    	    bzero = (maxtape / denom) * mindata - (mintape / denom) * maxdata
	}
end


# WFT_GET_TAPE_LIMITS -- Procedure for calculating the maximum and minimum FITS
# integer values from the FITS bitpix.

procedure wft_get_tape_limits (fits_bitpix, mintape, maxtape)

int	fits_bitpix		# the bits per pixel of a FITS integer
double	maxtape, mintape	# the maximun and minimum FITS tape integers

begin
	switch (fits_bitpix) {
	case FITS_BYTE:
	    maxtape = BYTE_MAX
	    mintape = BYTE_MIN
	case FITS_SHORT:
	    maxtape = SHORT_MAX #- 1.0d0
	    mintape = SHORT_MIN #+ 1.0d0
	case FITS_LONG:
	    maxtape = LONG_MAX #- 2.147d3
	    mintape = LONG_MIN #+ 2.147d3
	default:
	    call flush (STDOUT)
	    call error (4, "TAPE_LIMITS: Unknown FITS type.")
	}
end




# WFT_GET_BITPIX -- This procedure fetches the user determined bitpix or ERR if
# the bitpix is not one of the permitted FITS types.

int procedure wft_get_bitpix (bitpix)

int	bitpix

begin
	switch (bitpix) {
	case FITS_BYTE, FITS_SHORT, FITS_LONG:
	    return (bitpix)
	default:
	    return (ERR)
	}
end

# WFT_DATA_LIMITS -- Procedure to calculate the maximum and minimum data values
# in an IRAF image. Values are only calculated if the max and min are unknown
# or the image has been modified since the last values were calculated.

procedure wft_data_limits (im, irafmin, irafmax)

pointer	im		# image pointer
real	irafmin		# minimum picture value
real	irafmax		# maximum picture value

pointer	buf
int	npix
long	v[IM_MAXDIM]
real	maxval, minval
int	imgnlr(), compress, blklen, gn, ngroups, junk
int	gi_gstfval(), force_minmax
real	datamin, datamax
errchk	imgnlr

include "wfits.com"

begin
	compress = YES
	blklen = 1
	# We want to recalculate min and max all the time when
	# we are not using ieee as in this case.
	force_minmax = NO
	if (PIXTYPE(im) == TY_REAL || PIXTYPE(im) == TY_DOUBLE)
	   force_minmax = YES

	if (sdasmgcv > 0) {
	   ngroups = gi_gstfval (im, "GCOUNT")

	   if (LIMTIME(im) < MTIME(im) && NAXIS(im) > 0 ||
	       force_minmax == YES) {
	      npix = NAXISN(im,1)

	      call amovkl (long(1), v, IM_MAXDIM)
	      # Read the first line of the 1st group and
	      # calculate (min,max).
	      junk = imgnlr (im, buf, v)
	      call alimr (Memr[buf], npix, irafmin, irafmax)
	      do gn = 1, ngroups {
	         call gi_opengr (im, gn, datamin, datamax, 0)
	         call amovkl (long(1), v, IM_MAXDIM)
	         while (imgnlr (im, buf, v) != EOF) {
	            call alimr (Memr[buf], npix, minval, maxval)
	            irafmin = min (irafmin, minval)
	            irafmax = max (irafmax, maxval)
	         }
	      }
	   } else {
	      irafmax = IM_MAX(im)
	      irafmin = IM_MIN(im)
	      do gn = 1, ngroups {
	         call gi_opengr (im, gn, datamin, datamax, 0)
	         irafmin = min (irafmin, datamin)
	         irafmax = max (irafmax, datamax)
	      }
           }
	   #reset to group one
	   gn = 1
	   call gi_opengr (im, gn, datamin, datamax, 0)
	} else {
	   if (LIMTIME(im) < MTIME(im) && NAXIS(im) > 0
	       || force_minmax == YES) {

	      irafmax = -MAX_REAL
	      irafmin = MAX_REAL
	      npix = NAXISN(im,1)

	      call amovkl (long(1), v, IM_MAXDIM)
	      while (imgnlr (im, buf, v) != EOF) {
	         call alimr (Memr[buf], npix, minval, maxval)
	         irafmin = min (irafmin, minval)
	         irafmax = max (irafmax, maxval)
	      }
	   } else {
	      irafmax = IM_MAX(im)
	      irafmin = IM_MIN(im)
	   }
	}
	# extend the range for doubles since irafmin and max are single
	# float and precision loss can occur when calculating the
	# scale and offset factors.
	if (PIXTYPE(im) == TY_DOUBLE) {
	   irafmax = irafmax + (irafmax / (1.0d1 ** (NDIGITS_RP)))
	   irafmin = irafmin - (irafmin / (1.0d1 ** (NDIGITS_RP)))
	}
end
