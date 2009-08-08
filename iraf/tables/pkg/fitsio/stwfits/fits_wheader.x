include <imio.h>
include <imhdr.h>
include <mach.h>
include "wfits.h"

define	FITS_DOUBLE	64
# WFT_WRITE_HEADER -- Procedure to write FITS headers. The FITS header
# parameters are encoded one by one until the FITS END keyword is detected.
# If the long_header switch is set the full FITS header is printed on the
# standard output. If the short header parameter is specified only the image
# title and dimensions are printed.

procedure wft_write_header (im, fits, fits_fd)

pointer	im		# pointer to the IRAF image
pointer	fits		# pointer to the FITS structure
int	fits_fd		# the FITS file descriptor

size_t	sz_val
char	card[LEN_CARD+1], trim_card[LEN_CARD+1]
int	recntr, cardptr, cardcnt, stat, cards_per_rec, ndim, ngroups
size_t	nrecords

int	wft_card_encode(), strncmp()
int	wft_init_card_encode(), gi_gstfval(), imod()

errchk	wft_get_iraf_typestring, wft_set_blank
errchk	wft_init_card_encode, wft_card_encode, wft_scale_par
errchk	wft_init_write_pixels, wft_write_pixels, wft_write_last_record

include "wfits.com"

begin
	# Setup BSCALE and BZERO
	call wft_scale_par (im,fits)

	# Change dimensionality if we are converting to an extra
	# dimension FITS file.
	if (sdasmgcv > 0) {
	   ngroups = gi_gstfval(im, "GCOUNT")
	   if (ngroups > 1) {
	      IM_NDIM(im) = IM_NDIM(im) + 1
 	      ndim = IM_NDIM(im)
	      IM_LEN(im,ndim) =  ngroups
	   }
#	   extensions = YES
	}

	# If blanks in image set the blank parameter
	if (NBPIX(im) > 0)
	    call wft_set_blank (FITS_BITPIX(fits), BLANK(fits),
	        BLANK_STRING(fits))

	call wft_get_iraf_typestring (PIXTYPE(im), TYPE_STRING(fits))

	# initialize card counters, these counters are used only for
	# information printed to the standard output
	recntr = 1
	cardptr = 1
	cardcnt = 1
	cards_per_rec = len_record / LEN_CARD

	# Get set up to write header
	stat = wft_init_card_encode (im, fits)
	call wft_init_write_pixels (len_record, TY_CHAR, FITS_BYTE)

	# Write the cards to the FITS header
	repeat {
	    stat = wft_card_encode (im, fits, card)
	    if (stat == NO)
		next

	    sz_val = LEN_CARD
	    call wft_write_pixels (fits_fd, card, sz_val)

	    if (long_header == YES) {
		call wft_trimstr (card, trim_card, LEN_CARD)
	        call printf ("%2d/%2d:--  %s\n")
		    call pargi (recntr)
		    call pargi (cardptr)
		    call pargstr (trim_card)
	    }

	    if (imod (cardcnt, cards_per_rec) == 0) {
	        recntr = recntr + 1
	        cardptr = 1
	    } else
		cardptr = cardptr + 1
	    cardcnt = cardcnt + 1

	} until (strncmp (card, "END     ", LEN_KEYWORD) == 0)

	# Write last header records.
	call wft_write_last_record (fits_fd, nrecords)
	if (long_header == YES) {
	   call printf ("%d Header  ")
		 call pargz (nrecords)
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

int	cardno, axisno, optiono, hist_ptr, unknown_ptr, xdimno
int	nstandard, noptions, stat
int	wft_standard_card(), wft_option_card(), wft_xdim_card()
int	wft_history_card(), wft_unknown_card(), wft_last_card()
errchk	wft_standard_card, wft_option_card, wft_history_card
errchk	wft_unknown_card, wft_last_card

include	"wfits.com"

begin
	# Initialize the card pointers.
	cardno = 1
	axisno = 1
	optiono = 1
	unknown_ptr = 1
	hist_ptr = 1
	xdimno = 1

	# Initialize the card counters.
	nstandard = 3 + NAXIS(im)
	if (extensions == YES || sdasmgcv > 0)	# add EXTEND card if necessary
	   nstandard = nstandard + 1
	if (ext_type == IMAGE)
	   nstandard = nstandard + 1    # Add PCOUNT and GCOUNT but not EXTEND
	# add SDASMGNU standard card if necessary
	# also added when PSIZE=0 and gcount=1. This is to allow
	# strfits to recreate the geis file with no gpb.
	noptions = NOPTIONS + nstandard
	
	if (sdasmgcv >= 0)
	   noptions = noptions + 1

	return (YES)

# WFT_CARD_ENCODE -- Procedure to encode the FITS header parameters into
# FITS card images.

entry	wft_card_encode (im, fits, card)

	# fetch the appropriate FITS header card image
	if (cardno <= nstandard) {
	    stat = wft_standard_card (cardno, im, fits, axisno, card)
	} else if (cardno <= noptions) {
	    stat = wft_option_card (im, fits, optiono, card)
	} else if (wft_unknown_card (fits, im, unknown_ptr, card) == YES) {
	    stat = YES
	} else if (wft_history_card (im, hist_ptr, card) == YES) {
	    stat = YES
	} else if (wft_xdim_card (im, xdimno, card) == YES) {
	    stat = YES
	} else {
	    stat = wft_last_card (card)
	}

	cardno = cardno + 1

	return (stat)
end

# WFT_SET_BLANK -- Determine the FITS integer value for a blank pixel from the
# FITS bitpix.

procedure wft_set_blank (fits_bitpix, blank, blank_str)

int	fits_bitpix	# the requested FITS bits per pixel
long	blank		# the FITS integer value of a blank pixel
char	blank_str[ARB]	# the encoded FITS integer value of a blank pixel

double	tmp

begin
	switch (fits_bitpix) {
	case FITS_BYTE:
	    blank = BYTE_BLANK
	    call strcpy ("0", blank_str, LEN_BLANK)
	case FITS_SHORT:
	    blank = SHORT_BLANK
	    call strcpy ("-32768", blank_str, LEN_BLANK)
	case FITS_LONG:
	    blank = LONG_BLANK
	    call strcpy ("-2147483648", blank_str, LEN_BLANK)
	case FITS_LONGLONG:
	    if ( SZ_LONG == 2 ) {
		call error (0, "WFT_SET_BLANK: cannot handle 64-bit integer.")
	    } else {
		tmp = LONGLONG_BLANK
		blank = tmp
                call strcpy ("-9223372036854775808", blank_str, LEN_BLANK)
	    }
	default:
	    call flush (STDOUT)
	    call error (5, "SET_BLANK: Unknown FITS type.")
	}
end


