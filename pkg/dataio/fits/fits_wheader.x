# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <mach.h>
include <fset.h>
include "wfits.h"

# WFT_WRITE_HEADER -- Write the FITS headers. The FITS header
# parameters are encoded one by one until the FITS END keyword is detected.
# If the long_header switch is set the full FITS header is printed on the
# standard output. If the short header parameter is specified only the image
# title and dimensions are printed.

procedure wft_write_header (im, fits, fits_fd)

pointer	im		# pointer to the IRAF image
pointer	fits		# pointer to the FITS structure
int	fits_fd		# the FITS file descriptor

char	card[LEN_CARD+1], trim_card[LEN_CARD+1]
int	nrecords, recntr, cardptr, cardcnt, stat, cards_per_rec, i
int	wft_card_encode(), wft_set_bitpix(), sizeof(), strncmp()
int	wft_init_card_encode(), fstati()

errchk	wft_set_bitpix, wft_get_iraf_typestring, wft_set_scale, wft_set_blank
errchk	wft_fits_set_scale, wft_init_card_encode, wft_card_encode
errchk	wft_init_write_pixels, wft_write_pixels, wft_write_last_record

include "wfits.com"

begin
	# SET the data and FITS bits per pixel.

	DATA_BITPIX(fits) = sizeof (PIXTYPE(im)) * SZB_CHAR * NBITS_BYTE
	FITS_BITPIX(fits) = wft_set_bitpix (bitpix, PIXTYPE(im),
	    DATA_BITPIX(fits))

	# Calculate the FITS bscale and bzero parameters. Notice for the
	# time being that scaling is turned off if IEEE floating point
	# output is selected. May decide to change this later after
	# checking the specifications.

	if (FITS_BITPIX(fits) < 0) {

	    IRAFMIN(fits) = IM_MIN(im)
	    IRAFMAX(fits) = IM_MAX(im)
	    SCALE(fits) = NO
	    BZERO(fits) = 0.0d0
	    BSCALE(fits) = 1.0d0

	} else if (autoscale == YES) {

	    call wft_get_tape_limits (FITS_BITPIX(fits), TAPEMIN(fits),
	        TAPEMAX(fits))
	    call wft_data_limits (im, IRAFMIN(fits), IRAFMAX(fits))
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

	# If blanks in the image set the blank parameter. Currently information
	# on blanks is not written out so this is effectively a null operation
	# in IRAF.

	if (NBPIX(im) > 0)
	    call wft_set_blank (FITS_BITPIX(fits), BLANK(fits),
	        BLANK_STRING(fits))

	# Set the IRAF datatype parameter.
	call wft_get_iraf_typestring (PIXTYPE(im), TYPE_STRING(fits))

	# Initialize the card counters. These counters are used only for
	# information printed to the standard output.

	recntr = 1
	cardptr = 1
	cardcnt = 1
	cards_per_rec = len_record / LEN_CARD

	# Get set up to write the FITS header. Initialize for an ASCII write.
	stat = wft_init_card_encode (im, fits)
	if (make_image == YES)
	    call wft_init_wrt_pixels (len_record, TY_CHAR, FITS_BYTE, blkfac)

	# Print short header.
	if (short_header == YES && long_header == NO) {

	    call printf ("%-20.20s  ")
		call pargstr (OBJECT(im))
	    do i = 1, NAXIS(im) {
		if (i == 1) {
		    call printf ("Size = %d")
			call pargl (NAXISN(im,i))
		} else {
		    call printf (" x %d")
			call pargl (NAXISN(im,i))
		}
	    }
	    call printf ("\n")

	    call strlwr (TYPE_STRING(fits))
	    call printf ("\tpixtype=%s bitpix=%d")
		call pargstr (TYPE_STRING(fits))
		call pargi (FITS_BITPIX(fits))

	    if (fstati (fits_fd, F_BLKSIZE) == 0) {
		call printf (" blkfac=%d")
		    call pargi (blkfac)
	    } else
		call printf (" blkfac=fixed")

	    if (SCALE(fits) == YES) {
	        call printf (" bscale=%.7g bzero=%.7g\n")
		    call pargd (BSCALE(fits))
		    call pargd (BZERO(fits))
	    } else
		call printf (" scaling=none\n")
	    call strupr (TYPE_STRING(fits))
	}

	# Write the cards to the FITS header.
	repeat {

	    # Encode the card.
	    stat = wft_card_encode (im, fits, card)
	    if (stat == NO)
		next

	    # Write the card to the output file if make_image is yes.
	    if (make_image == YES)
	        call wft_write_pixels (fits_fd, card, LEN_CARD)

	    # Trim the card and write is to the standard output if 
	    # long_header is yes.

	    if (long_header == YES) {
		call wft_trimstr (card, trim_card, LEN_CARD)
	        call printf ("%2d/%2d:--  %s\n")
		    call pargi (recntr)
		    call pargi (cardptr)
		    call pargstr (trim_card)
	    }

	    if (mod (cardcnt, cards_per_rec) == 0) {
	        recntr = recntr + 1
	        cardptr = 1
	    } else
		cardptr = cardptr + 1
	    cardcnt = cardcnt + 1

	} until (strncmp (card, "END     ", LEN_KEYWORD) == 0)

	# Issue warning about possible precision loss. Comment this out
	# for time being, since the short header was modified.
	#if (SCALE(fits) == YES && bitpix != ERR) {
	    #call printf (
	    #"\tDefault bitpix overridden: maximum precision loss ~%.7g\n")
		#call pargd (BSCALE(fits))
	#}

	# Write the last header records.
	if (make_image == YES) {
	    call wft_write_last_record (fits_fd, nrecords)
	    if (short_header == YES || long_header == YES) {
	        call printf ("\t%d Header  ")
		    call pargi (nrecords)
	    }
	}
end


# WFT_SET_BITPIX -- This procedure sets the FITS bitpix for each image based on
# either the user given value or the precision of the IRAF data. Notice that
# the user must explicitly set the bitpix parameter to -16 or -32 to select
# the IEEE output format.

int procedure wft_set_bitpix (bitpix, datatype, data_bitpix)

int	bitpix		# the user set bits per pixel, ERR or legal bitpix
int	datatype	# the IRAF image data type
int	data_bitpix	# the bits per pixel in the data

begin
	if (bitpix == ERR) {
	    switch (datatype) {
	    case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	        if (data_bitpix <= FITS_BYTE)
		    return (FITS_BYTE)
	        else if (data_bitpix <= FITS_SHORT) {
		    if (datatype == TY_USHORT)
			return (FITS_LONG)
		    else
		        return (FITS_SHORT)
	        } else
		    return (FITS_LONG)
	    case TY_REAL, TY_COMPLEX:
		return (FITS_REAL)
	    case TY_DOUBLE:
		 return (FITS_DOUBLE)
	    default:
		call error (2, "SET_BITPIX: Unknown IRAF data type.")
	    }
	} else
	    return (bitpix)
end


# WFT_GET_IRAF_TYPESTRING -- Procedure to set the iraf datatype keyword.
# Permitted strings are INTEGER, FLOATING or COMPLEX.

procedure wft_get_iraf_typestring (datatype, type_str)

int	datatype	# the IRAF data type
char	type_str[ARB]	# the output IRAF type string

begin
	switch (datatype) {
	case TY_SHORT:
	    call strcpy ("SHORT", type_str, LEN_STRING) 
	case TY_USHORT:
	    call strcpy ("USHORT", type_str, LEN_STRING) 
	case TY_INT: 
	    call strcpy ("INTEGER", type_str, LEN_STRING) 
	case TY_LONG: 
	    call strcpy ("LONG", type_str, LEN_STRING) 
	case TY_REAL:
	    call strcpy ("REAL", type_str, LEN_STRING)
	case TY_DOUBLE:
	    call strcpy ("DOUBLE", type_str, LEN_STRING)
	case TY_COMPLEX:
	    call strcpy ("COMPLEX", type_str, LEN_STRING)
	default:
	    call error (3, "IRAF_TYPE: Unknown IRAF image type.")
	}
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
	case TY_SHORT, TY_INT, TY_LONG:
	    if (data_bitpix > fits_bitpix) {
		scale = YES
		call wft_set_scale (fits_bitpix, irafmin, irafmax, tapemin, 
		    tapemax, bscale, bzero)
	    } else {
		scale = NO
		bscale = 1.0d0
		bzero = 0.0d0
	    }
	case TY_USHORT:
	    if (data_bitpix > fits_bitpix) {
		scale = YES
		call wft_set_scale (fits_bitpix, irafmin, irafmax, tapemin, 
		    tapemax, bscale, bzero)
	    } else if (data_bitpix == fits_bitpix) {
		scale = YES
		bscale = 1.0d0
		bzero = 3.2768d4
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
	    call error (1, "WRT_HEADER: Unknown IRAF image type.")
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
	# Calculate the maximum and minimum values in the data.
	maxdata = datamax + abs ((datamax / (10.0 ** (NDIGITS_RP - 1))))
	mindata = datamin - abs ((datamin / (10.0 ** (NDIGITS_RP - 1))))
	denom =  maxtape - mintape
	num = maxdata - mindata
	#denom = denom - denom / (1.0d1 ** (NDIGITS_RP - 1))
	#num = num + num / (1.0d1 ** (NDIGITS_RP - 1))

	# Check for constant image case.
	mindata = datamin
	maxdata = datamax
	if (rft_equald (num, 0.0d0)) {
	    bscale = 1.0d0
	    bzero = maxdata
	} else {
	    bscale = num / denom
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
	    maxtape = SHORT_MAX
	    mintape = SHORT_MIN
	case FITS_LONG:
	    maxtape = LONG_MAX
	    mintape = LONG_MIN
	default:
	    call error (4, "TAPE_LIMITS: Unknown FITS type.")
	}
end


# WFT_SET_BLANK -- Determine the FITS integer value for a blank pixel from the
# FITS bitpix. Notice that these are null ops for IEEE floating point format.

procedure wft_set_blank (fits_bitpix, blank, blank_str)

int	fits_bitpix	# the requested FITS bits per pixel
long	blank		# the FITS integer value of a blank pixel
char	blank_str[ARB]	# the encoded FITS integer value of a blank pixel

begin
	switch (fits_bitpix) {
	case FITS_BYTE:
	    blank = long (BYTE_BLANK)
	    call strcpy ("0", blank_str, LEN_BLANK)
	case FITS_SHORT:
	    blank = long (SHORT_BLANK)
	    call strcpy ("-32768", blank_str, LEN_BLANK)
	case FITS_LONG:
	    blank = long (LONG_BLANK)
	    call strcpy ("-2147483648", blank_str, LEN_BLANK)
	case FITS_REAL:
	    blank = INDEFL
	    call strcpy ("", blank_str, LEN_BLANK)
	case FITS_DOUBLE:
	    blank = INDEFL
	    call strcpy ("", blank_str, LEN_BLANK)
	default:
	    call error (5, "SET_BLANK: Unknown FITS type.")
	}
end


# WFT_INIT_CARD_ENCODE -- This procedure initializes the card encoding
# procedure.  The cards counters are initialized and the number of history cards
# calculated.

int procedure wft_init_card_encode (im, fits)

# both entry points
pointer	im	# pointer to the IRAF image
pointer	fits	# pointer to the WFITS structure

# entry wft_card_encode
int	wft_card_encode		# entry point
char	card[LEN_CARD+1]	# string containing the card image

int	cardno, axisno, optiono, hist_ptr, unknown_ptr
int	nstandard, noptions, stat
int	wft_standard_card(), wft_option_card(), wft_last_card()
int	wft_history_card(), wft_unknown_card()
errchk	wft_standard_card, wft_option_card, wft_history_card
errchk	wft_unknown_card, wft_last_card

begin
	# Initialize the card pointers.
	cardno = 1
	axisno = 1
	optiono = 1
	unknown_ptr = 1
	hist_ptr = 1

	# Initilaize the card counters.
	nstandard = 3 + NAXIS(im)
	noptions = NOPTIONS + nstandard

	return (YES)


# WFT_CARD_ENCODE -- Procedure to encode the FITS header parameters into
# FITS card images.

entry	wft_card_encode (im, fits, card)

	# Fetch the appropriate FITS header card image.
	if (cardno <= nstandard) {
	    stat = wft_standard_card (cardno, im, fits, axisno, card)
	} else if (cardno <= noptions) {
	    stat = wft_option_card (im, fits, optiono, card)
	} else if (wft_unknown_card (im, unknown_ptr, card) == YES) {
	    stat = YES
	} else if (wft_history_card (im, hist_ptr, card) == YES) {
	    stat = YES
	} else {
	    stat = wft_last_card (card)
	}

	cardno = cardno + 1

	return (stat)
end


# WFT_TRIMSTR -- Procedure to trim trailing blanks from a fixed size string.

procedure wft_trimstr (instr, outstr, nchars)

char	instr[ARB]	# input string
char	outstr[ARB]	# output string
int	nchars		# last character of instr

int	ip

begin
	call strcpy (instr, outstr, nchars)
	ip = nchars
	while (outstr[ip] == ' ')
	    ip = ip - 1
	outstr[ip+1] = EOS
end
