# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include "wfits.h"

# WFT_STANDARD_CARD -- Procedure for fetching the minimum header
# parameters required by fits. The end card is encoded separately.

int procedure wft_standard_card (cardno, im, fits, axisno, card)

int	cardno		# number of FITS standard card
pointer	im		# pointer to the IRAF image
pointer	fits		# pointer to the FITS structure
int	axisno		# axis number
char	card[ARB]	# FITS card image

char	keyword[LEN_KEYWORD]
int	len_object
int	strlen()
errchk	wft_encodeb, wft_encodei, wft_encodel, wft_encode_axis

begin
	# Get mandatory keywords.
	switch (cardno) {
	case FIRST_CARD:
	    if (XTENSION(fits) == EXT_PRIMARY) {
	        call wft_encodeb ("SIMPLE", YES, card, "FITS STANDARD")
	    } else {
		len_object = max (min (LEN_OBJECT, strlen ("IMAGE")),
		    LEN_STRING)
		call wft_encodec ("XTENSION", "IMAGE", len_object, card,
		    "IMAGE EXTENSION")
	    }
	case SECOND_CARD:
	    call wft_encodei ("BITPIX", FITS_BITPIX(fits), card,
	    "FITS BITS/PIXEL")
	case THIRD_CARD:
	    call wft_encodei ("NAXIS", NAXIS(im), card, "NUMBER OF AXES")
	default:
	    call wft_encode_axis ("NAXIS", keyword, axisno)
	    call wft_encodel (keyword, NAXISN(im, axisno), card, "")
	    axisno = axisno + 1
	}

	return (YES)
end


# WFT_OPTION_CARD -- Procedure for fetching  optional FITS header parameters.
# At present these are bscale, bzero, bunit, blank, object, origin, date,
# irafmax, irafmin, iraf type and iraf bits per pixel. Blank is only encoded
# if there are a nonzero number of blanks in the IRAF image. Bunit and object
# are only encoded if the appropriate IRAF strings are defined. Bzero, bscale,
# irafmax, irafmin, iraf type and iraf bits per pixel are only encoded if
# there is a pixel file.

int procedure wft_option_card (im, fits, optiono, card)

pointer	im		# pointer to the IRAF image
pointer	fits		# pointer to FITS structure
int	optiono		# number of the option card
char	card[ARB]	# FITS card image

char	datestr[LEN_DATE]
int	len_object, stat
int	strlen()
errchk	wft_encoded, wft_encodec, wft_encode_blank, wft_encoder, wft_encodei
errchk	wft_encode_date 
include "wfits.com"

begin
	 stat = YES

	 # get optional keywords
	 switch (optiono) {
	 case KEY_EXTEND:
	    if (XTENSION(fits) == EXT_IMAGE || wextensions == NO)
		stat = NO
	    else
		call wft_encodeb ("EXTEND", YES, card,
		    "STANDARD EXTENSIONS MAY BE PRESENT")
	 case KEY_PCOUNT:
	    if (XTENSION(fits) == EXT_PRIMARY)
		stat = NO
	    else
		call wft_encodei ("PCOUNT", 0, card, "NO RANDOM PARAMETERS")
	 case KEY_GCOUNT:
	    if (XTENSION(fits) == EXT_PRIMARY)
		stat = NO
	    else
		call wft_encodei ("GCOUNT", 1, card, "ONLY ONE GROUP")
	 case KEY_BSCALE:
	    if ((NAXIS(im) <= 0) || (FITS_BITPIX(fits) < 0))
		stat = NO
	    else {
	        call wft_encoded ("BSCALE", BSCALE(fits), card,
	        "REAL = TAPE*BSCALE + BZERO", NDEC_DOUBLE)
	    }
	 case KEY_BZERO:
	    if ((NAXIS(im) <= 0) || (FITS_BITPIX(fits) < 0))
		stat = NO
	    else
	        call wft_encoded ("BZERO", BZERO(fits), card, "", NDEC_DOUBLE)
	 case KEY_BUNIT:
	    stat = NO
	 case KEY_BLANK:
	    stat = NO
	    #if (NBPIX(im) == 0)
		#stat = NO
	    #else
		#call wft_encode_blank ("BLANK", BLANK_STRING(fits), card,
		#"TAPE VALUE OF BLANK PIXEL")
	case KEY_OBJECT:
	    if (OBJECT(im) == EOS)
		stat = NO
	    else {
		len_object = max (min (LEN_OBJECT, strlen (OBJECT(im))),
		    LEN_STRING)
		call wft_encodec ("OBJECT", OBJECT(im), len_object, card, "")
	    }
	case KEY_ORIGIN:
	    call wft_encodec ("ORIGIN", "KPNO-IRAF", LEN_ORIGIN, card, "")
	case KEY_DATE:
	    call wft_encode_date (datestr, LEN_DATE)
	    len_object = max (min (LEN_OBJECT, strlen (datestr)), LEN_STRING)
	    call wft_encodec ("DATE", datestr, len_object, card, "")
	case KEY_IRAFNAME:
	    len_object = max (min (LEN_OBJECT, strlen (IRAFNAME(fits))),
		LEN_STRING)
	    call wft_encodec ("IRAFNAME", IRAFNAME(fits), len_object, card, 
		"NAME OF IRAF IMAGE FILE")
	case KEY_IRAFMAX:
	    if (NAXIS(im) <= 0)
		stat = NO
	    else
	        call wft_encoder ("IRAF-MAX", IRAFMAX(fits), card, "DATA MAX",
				NDEC_REAL)
	case KEY_IRAFMIN:
	    if (NAXIS(im) <= 0)
		stat = NO
	    else
	        call wft_encoder ("IRAF-MIN", IRAFMIN(fits), card, "DATA MIN",
		NDEC_REAL)
	case KEY_IRAFBP:
	    if (NAXIS(im) <= 0)
		stat = NO
	    else
	        call wft_encodei ("IRAF-BPX", DATA_BITPIX(fits), card,
	    	    "DATA BITS/PIXEL")
	case KEY_IRAFTYPE:
	    if (NAXIS(im) <= 0)
		stat = NO
	    else
	        call wft_encodec ("IRAFTYPE", TYPE_STRING(fits), LEN_STRING,
	    		    card, "PIXEL TYPE")
	default:
	    stat = NO
	}

	optiono = optiono + 1

	return (stat)
end


# WFT_HISTORY_CARD -- Procedure to fetch a single history line, trim newlines
# and pad with blanks to size LEN_CARD in order to create a FITS HISTORY card.

int procedure wft_history_card (im, hp, card)

pointer	im		# pointer to the IRAF image
int	hp		# pointer to first character to extract from string
char	card[ARB]	# FITS card image

char	cval
char	chfetch()

begin
	if (chfetch (HISTORY(im), hp, cval) == EOS)
	    return (NO)
	else {
	    hp = hp - 1
	    call strcpy ("HISTORY ", card, LEN_KEYWORD)
	    call wft_fits_card (HISTORY(im), hp, card, COL_VALUE - 2, LEN_CARD,
		'\n')
	    return (YES)
	}
end


# WFT_UNKNOWN_CARD  -- Procedure to fetch a single unknown
# "line", trim newlines and pad blanks to size LEN_CARD in order to
# create an unknown keyword card. At present user area information is
# assumed to be in the form of FITS card images, less then or equal to
# 80 characters and delimited by a newline.

int procedure wft_unknown_card (im, up, card)

pointer	im		# pointer to the IRAF image
int	up		# pointer to next character in the unknown string
char	card[ARB]	# FITS card image

char	cval
int	stat, axis, index
char	chfetch()
int	strmatch(), ctoi()

begin
	if (chfetch (UNKNOWN(im), up, cval) == EOS)
	    return (NO)
	else {
	    up = up - 1
	    stat = NO
	    while (stat == NO)  {
	        call wft_fits_card (UNKNOWN(im), up, card, 1, LEN_CARD, '\n')
		if (card[1] == EOS)
		    break
	        if (strmatch (card, "^GROUPS  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^SIMPLE  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^BITPIX  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^NAXIS   ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^NAXIS") != 0) {
		    index = LEN_NAXIS_KYWRD + 1
		    if (ctoi (card, index, axis) > 0)
		        stat = NO
		    else
			stat = YES
	        } else if (strmatch (card, "^GCOUNT  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^PCOUNT  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^PSIZE   ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^BSCALE  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^BZERO   ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^BLANK   ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^IRAF-MAX") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^IRAF-MIN") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^IRAFTYPE") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^IRAF-B/P") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^IRAF-BPX") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^FILENAME") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^IRAFNAME") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^EXTEND  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^EXTNAME ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^EXTVER  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^INHERIT ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^IRAF-TLM") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^OBJECT  ") != 0) {
		    stat = NO
	        } else if (strmatch (card, "^END     ") != 0) {
		    stat = NO
	        } else
                    stat = YES
	    }

	    return (stat)
	}
end


# WFT_LAST_CARD -- Procedure to encode the FITS end card.

int procedure wft_last_card (card)

char	card[ARB]	# FITS card image

begin
	call sprintf (card, LEN_CARD, "%-8.8s  %70w")
	    call pargstr ("END")

	return (YES)
end
