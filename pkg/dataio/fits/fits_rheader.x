# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include <imhdr.h>
include <imio.h>
include <mach.h>
include	"rfits.h"

define	NEPSILON 10.0d0    # number of machine epsilon

# RFT_READ_HEADER -- Read a FITS header.
# If BSCALE and BZERO are different from 1.0 and 0.0  scale is set to true
# otherwise scale is false.
# EOT is detected by an EOF on the first read and EOF is returned to the calling
# routine.  Any error is passed to the calling routine.

int procedure rft_read_header (fits_fd, fits, im, gim)

int	fits_fd			# FITS file descriptor
pointer	fits			# FITS data structure
pointer	im			# IRAF image descriptor
pointer	gim			# IRAF global header image descriptor

int	i, stat, nread, max_lenuser, fd_usr, ndiscard
char	card[LEN_CARD+1], type_str[LEN_TYPESTR]
int	rft_decode_card(), rft_init_read_pixels(), rft_read_pixels(), strmatch()
int	stropen()
errchk	rft_decode_card, rft_init_read_pixels, rft_read_pixels
errchk	stropen, close

include "rfits.com"

begin
	# Initialization.
	XTENSION(fits) = EXT_PRIMARY
	BITPIX(fits) = INDEFI
	NAXIS(im) = 0
	do i = 1, IM_MAXDIM
	    IM_LEN(im,i) = 0
	PCOUNT(fits) = 0
	GCOUNT(fits) = 1
	SCALE(fits) = NO
	FITS_BSCALE(fits) = 1.0d0
	FITS_BZERO(fits) = 0.0d0
	BLANKS(fits) = NO
	BLANK_VALUE(fits) = INDEFL
	NRECORDS(fits) = 0
	IRAFNAME(fits) = EOS
	INHERIT(fits) = NO
	ndiscard = 0
	OBJECT(im) = EOS
	UNKNOWN(im) = EOS
	max_lenuser = (LEN_IMDES + IM_LENHDRMEM(im) - IMU) * SZ_STRUCT - 1

	# The FITS header is character data in FITS_BYTE form. Open the
	# header for reading. Open the user area which is a character
	# string as a file.

	i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)
	fd_usr = stropen (UNKNOWN(im), max_lenuser, NEW_FILE)

	# Loop until the END card is encountered.
	nread = 0
	repeat {

	    # Read the card.
	    i = rft_read_pixels (fits_fd, card, LEN_CARD, NRECORDS(fits), 1)
	    card[LEN_CARD + 1] = '\n'
	    card[LEN_CARD + 2] = EOS

	    # Decode the card images.
	    if ((i == EOF) && (nread == 0)) {
		call close (fd_usr)
		return (EOF)
	    } else if ((nread == 0) && SIMPLE(fits) == NO &&
	        strmatch (card, "^SIMPLE  ") == 0) {
		call flush (STDOUT)
		call close (fd_usr)
		call error (30,
		    "RFT_READ_HEADER: Not a FITS file (no SIMPLE keyword)")
	    } else if ((nread == 0) && EXTEND(fits) == YES &&
	        strmatch (card, "^XTENSION") == 0) {
		XTENSION(fits) = EXT_SPECIAL
		call flush (STDOUT)
		call close (fd_usr)
		call error (30,
		"RFT_READ_HEADER: Not a FITS extension (no XTENSION keyword)")
	    } else if (i != LEN_CARD) {
		call close (fd_usr)
	        call error (2, "RFT_READ_HEADER: Error reading FITS header")
	    } else
	        nread = nread + 1

	    # Remove contaminating control characters and replace with blanks.
	    call rft_control_to_blank (card, card, LEN_CARD)

	    # Print FITS card images if long_header option specified.
	    if (long_header == YES) {
		call printf ("%-80.80s\n")
		    call pargstr (card)
	    }

	    # Stat = YES if FITS END card is encountered.
	    stat = rft_decode_card (fits, im, fd_usr, card, ndiscard)

	} until (stat == YES)

	# Check for the possibility of a global header.
	if (NAXIS(im) == 0 && XTENSION(fits) == EXT_PRIMARY)
	    GLOBALHDR(fits) = YES

	# Set the output image pixel type.
	call rft_set_image_pixtype (fits, im, FITS_BSCALE(fits),
	    FITS_BZERO(fits))

	# Copy the global header title and user area into the output image.
	if (GLOBALHDR(fits) == YES) {
	    if (XTENSION(fits) == EXT_IMAGE && INHERIT(fits) == YES &&
	        gim != NULL) {
		if (OBJECT(im) == EOS)
		    call strcpy (OBJECT(gim), OBJECT(im), SZ_OBJECT)
		call close (fd_usr)
		fd_usr = stropen (UNKNOWN(im), max_lenuser, APPEND)
	        call rft_gheader (im, gim, fd_usr, card, LEN_CARD, ndiscard,
		    long_header)
	    }
	}

	# Print optional short header.
	if (short_header == YES && long_header == NO) {
	    call printf ("%s  ")
	    switch (XTENSION(fits)) {
	    case EXT_PRIMARY:
		call pargstr ("")
	    case EXT_IMAGE:
		call pargstr ("IMAGE")
	    case EXT_TABLE:
		call pargstr ("TABLE")
	    case EXT_BINTABLE:
		call pargstr ("BINTABLE")
	    case EXT_UNKNOWN:
		call pargstr ("UNKNOWN")
	    default:
		call pargstr ("UNDEFINED")
	    }
	    if (make_image == NO) {
		if (old_name == YES) {
	            call printf ("-> %s  ")
		        call pargstr (IRAFNAME(fits))
		}
	    } else {
		call printf ("-> %s  ")
		    call pargstr (IM_HDRFILE(im))
	    }
	    call printf ("%-20.20s  ")
	        call pargstr (OBJECT(im))
	    call printf ("size=")
	    if (NAXIS(im) == 0)
		call printf ("0")
	    else {
	        do i = 1, NAXIS(im) {
		    if (i == 1) {
		        call printf ("%d")
		            call pargl (NAXISN(im,i))
		    } else {
		        call printf ("x%d")
		            call pargl (NAXISN(im,i))
		    }
	        }
	    }
	    call printf ("\n")
	    if (XTENSION(fits) == EXT_PRIMARY || XTENSION(fits) == EXT_IMAGE) {
	        call printf ("    bitpix=%d")
		    call pargi (BITPIX(fits))
	        if (SCALE(fits) == NO) {
		    call printf ("  scaling=none")
	        } else {
	            call printf ("  bscale=%.7g  bzero=%.7g")
		        call pargd (FITS_BSCALE(fits))
			call pargd (FITS_BZERO(fits))
	        }
		call rft_typestring (PIXTYPE(im), type_str, LEN_TYPESTR)
		call strlwr (type_str)
	        call printf ("  pixtype=%s")
		    call pargstr (type_str)
	        call printf ("\n")
	    }
	}

	# Let the user know if there is not enough space in the user area.
	if (ndiscard > 0) {
	    if (short_header == YES || long_header == YES) {
		if (long_header == NO)
		    call printf ("    ")
	        call printf (
	            "Warning: User area too small %d card images discarded\n")
	        call pargi (ndiscard)
	    }
	    call rft_last_user (UNKNOWN(im), max_lenuser)
	}

	call close (fd_usr)
	return (OK)
end


# RFT_CONTROL_TO_BLANK -- Replace an ACSII control characters in the
# FITS card image with blanks.

procedure rft_control_to_blank (incard, outcard, len_card)

char	incard[ARB]		# the input FITS card image
char	outcard[ARB]		# the output FITS card image
int	len_card		# the length of the FITS card image

int	i

begin
	for (i = 1; i <= len_card; i = i + 1) {
	    if (IS_PRINT(incard[i]))
	        outcard[i] = incard[i]
	    else
		outcard[i] = ' '
	}
end


# RFT_DECODE_CARD -- Decode a FITS card and return YES when the END
# card is encountered.  The keywords understood are given in rfits.h.

int procedure rft_decode_card (fits, im, fd_usr, card, ndiscard)

pointer	fits		# FITS data structure
pointer	im		# IRAF image descriptor
int	fd_usr		# file descriptor of user area
char	card[ARB]	# FITS card
int	ndiscard	# Number of cards for which no space available

char	cval
double	dval
int	nchar, i, j, k, len
pointer	sp, str, comment

bool	rft_equald()
int	strmatch(), ctoi(), ctol(), ctod(), cctoc(), rft_hms()
errchk	putline

include	"rfits.com"

begin
	call smark (sp)
	call salloc (str, LEN_CARD, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	i = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	    call sfree (sp)
	    return(YES)
	} else if (strmatch (card, "^SIMPLE  ") != 0) {
	    if (SIMPLE(fits) == YES) {
		if (short_header == YES || long_header == YES) {
		    if (long_header == NO)
			call printf ("    ")
		    call printf ("Warning: Duplicate SIMPLE keyword ignored\n")
		}
	    } else {
	        nchar = cctoc (card, i, cval)
	        if (cval != 'T')
		    call error (13, "RFT_DECODE_CARD: Non-standard FITS format")
		else
		    SIMPLE(fits) = YES
	    }
	} else if (strmatch (card, "^XTENSION") != 0) {
	    call rft_get_fits_string (card, Memc[str], LEN_CARD)
	    if (strmatch (Memc[str], "^IMAGE") != 0)
		XTENSION(fits) = EXT_IMAGE
	    else if (strmatch (Memc[str], "^TABLE") != 0)
		XTENSION(fits) = EXT_TABLE
	    else if (strmatch (Memc[str], "^BINTABLE") != 0)
		XTENSION(fits) = EXT_BINTABLE
	    else
		XTENSION(fits) = EXT_UNKNOWN
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	    if (! IS_INDEFI(BITPIX(fits))) {
		if (short_header == YES || long_header == YES) {
		    if (long_header == NO)
			call printf ("    ")
		    call printf ("Warning: Duplicate BITPIX keyword ignored\n")
		}
	    } else
	        nchar = ctoi (card, i, BITPIX(fits))
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	    if (NAXIS(im) != 0) {
		if (short_header == YES || long_header == YES) {
		    if (long_header == NO)
			call printf ("    ")
		    call printf ("Warning: Duplicate NAXIS keyword ignored\n")
		}
	    } else
	        nchar = ctoi (card, i, NAXIS(im))
	    if (NAXIS(im) > IM_MAXDIM)
		call error (5, "RFT_DECODE_CARD: FITS NAXIS too large")
	} else if (strmatch (card, "^NAXIS") != 0) {
	    k = strmatch (card, "^NAXIS")
	    nchar = ctoi (card, k, j)
	    if (NAXISN(im,j) != 0) {
		if (short_header == YES || long_header == YES) {
		    if (long_header == NO)
			call printf ("    ")
		    call printf ("Warning: Duplicate NAXIS%d keyword ignored\n")
		        call pargi (j)
		}
	    } else
	        nchar = ctol (card, i, NAXISN(im, j))
	} else if (strmatch (card, "^GROUPS  ") != 0) {
	    nchar = cctoc (card, i, cval)
	    if (cval == 'T') {
		NAXIS(im) = 0
		call error (6, "RFT_DECODE_CARD: Group data not implemented")
	    }
	} else if (strmatch (card, "^EXTEND  ") != 0) {
	    if (EXTEND(fits) == YES) {
		if (short_header == YES || long_header == YES) {
		    if (long_header == NO)
			call printf ("    ")
		    call printf ("Warning: Duplicate EXTEND keyword ignored\n")
		}
	    } else {
	        nchar = cctoc (card, i, cval)
	        if (cval == 'T')
		    EXTEND(fits) = YES
	    }
	} else if (strmatch (card, "^INHERIT ") != 0) {
	    if (INHERIT(fits) == YES) {
		if (short_header == YES || long_header == YES) {
		    if (long_header == NO)
			call printf ("    ")
		    call printf ("Warning: Duplicate INHERIT keyword ignored\n")
		}
	    } else {
	        nchar = cctoc (card, i, cval)
	        if (cval == 'T')
		    INHERIT(fits) = YES
	    }
	} else if (strmatch (card, "^PCOUNT  ") != 0) {
	    nchar = ctoi (card, i, PCOUNT(fits))
	    if (nchar <= 0)
		PCOUNT(fits) = 0
	} else if (strmatch (card, "^GCOUNT  ") != 0) {
	    nchar = ctoi (card, i, GCOUNT(fits))
	    if (nchar <= 0)
		GCOUNT(fits) = 1
	#} else if (strmatch (card, "^TABLES  ") != 0) {
	    #nchar = ctoi (card, i, ival)
	    #if (ival > 0)
		#call printf ("Warning: FITS special records not decoded\n")
	} else if (strmatch (card, "^BSCALE  ") != 0) {
	    nchar = ctod (card, i, dval)
	    if (nchar > 0)
	        FITS_BSCALE(fits) = dval
	    else if (short_header == YES || long_header == YES) {
		if (long_header == NO)
		    call printf ("    ")
		call printf ("Warning: Error decoding BSCALE, BSCALE=1.0\n")
	    }
	    if (! rft_equald (dval, 1.0d0) && (scale == YES))
		SCALE(fits) = YES
	} else if (strmatch (card, "^BZERO   ") != 0) {
	    nchar = ctod (card, i, dval)
	    if (nchar > 0)
	        FITS_BZERO(fits) = dval
	    else if (short_header == YES || long_header == YES) {
		if (long_header == NO)
		    call printf ("    ")
		call printf ("Warning: Error decoding BZERO, BZERO=0.0\n")
	    }
	    if (! rft_equald (dval, 0.0d0) && (scale == YES))
		SCALE(fits) = YES
	} else if (strmatch (card, "^BLANK   ") != 0) {
	    BLANKS(fits) = YES
	    nchar = ctol (card, i, BLANK_VALUE(fits))
	} else if (strmatch (card, "^OBJECT  ") != 0) {
	    call rft_get_fits_string (card, OBJECT(im), SZ_OBJECT)
	} else if (strmatch (card, "^IRAFNAME") != 0) {
	    call rft_get_fits_string (card, IRAFNAME(fits), SZ_FNAME)
	} else if (strmatch (card, "^FILENAME") != 0) {
	    if (IRAFNAME(fits) == EOS)
	        call rft_get_fits_string (card, IRAFNAME(fits), SZ_FNAME)
	} else if (strmatch (card, "^EXTNAME ") != 0) {
	    if (XTENSION(fits) != EXT_PRIMARY && XTENSION(fits) != EXT_IMAGE)
	        call rft_get_fits_string (card, IRAFNAME(fits), SZ_FNAME)
	} else if (strmatch (card, "^EXTVER  ") != 0) {
	    # Filter this quantitity out and ignore it for now.
	    ;
	} else if (strmatch (card, "^ORIGIN  ") != 0) {
	    call rft_trim_card (card, card, LEN_CARD)
	    call strcat (card[i], HISTORY(im), SZ_HISTORY)
	} else if (strmatch (card, "^DATE    ") !=  0) {
	    call rft_trim_card (card, card, LEN_CARD)
	    call strcat (card[i], HISTORY(im), SZ_HISTORY)
	} else if (strmatch (card, "^IRAF-TLM") !=  0) {
	    call rft_trim_card (card, card, LEN_CARD)
	    call strcat (card[i], HISTORY(im), SZ_HISTORY)
	#} else if (strmatch (card, "^HISTORY ") != 0) {
	    #call rft_trim_card (card, card, LEN_CARD)
	    #call strcat (card[i - 2], HISTORY(im), SZ_HISTORY)
	} else if (strmatch (card, "^UT      ") != 0) {
	    len = rft_hms (card, Memc[str], Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("UT", Memc[str], len, card, Memc[comment])
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^ZD      ") != 0) {
	    len = rft_hms (card, Memc[str], Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("ZD", Memc[str], len, card, Memc[comment])
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^ST      ") != 0) {
	    len = rft_hms (card, Memc[str], Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("ST", Memc[str], len, card, Memc[comment])
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^RA      ") != 0) {
	    len = rft_hms (card, Memc[str], Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("RA", Memc[str], len, card, Memc[comment])
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^DEC     ") != 0) {
	    len = rft_hms (card, Memc[str], Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("DEC", Memc[str], len, card, Memc[comment])
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else {
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	}

	call sfree (sp)

	return (NO)

end


# RFT_HMS -- Procedure to decode a FITS HMS card from the mountain.

int procedure rft_hms (card, str, comment, maxch)

char	card[ARB]		# FITS card
char	str[ARB]		# string
char	comment[ARB]		# comment string
int	maxch			# maximum number of characters

char	colon, minus
int	ip, nchar, fst, lst, deg, min
real	sec
int	stridx(), strldx(), strlen(), ctoi(), ctor()

begin
	# Return if not a FITS string parameter.
	if (card[COL_VALUE] != '\'')
	    return (0)

	# Set up key characters.
	colon = ':'
	minus = '-'

	# Get the FITS string.
	call rft_get_fits_string (card, str, maxch)

	# Get the comment string.
	call rft_get_comment (card, comment, maxch)

	# Test for blank string and for 2 colon delimiters.
	if (str[1] == EOS)
	    return (0)
	fst = stridx (colon, str)
	if (fst == 0)
	    return (0)
	lst = strldx (colon, str)
	if (lst == 0)
	    return (0)
	if (fst == lst)
	    return (0)

	# Decode the degrees field.
	ip = 1
	while (IS_WHITE(str[ip]))
	    ip = ip + 1
	if (str[ip] == '+' || str[ip] == '-')
	    ip = ip + 1
	nchar = ctoi (str, ip, deg)
	if (nchar == 0)
	    deg = 0

	# Decode the minutes field.
	ip = fst + 1
	while (IS_WHITE(str[ip]))
	    ip = ip + 1
	if (str[ip] == '+' || str[ip] == '-')
	    ip = ip + 1
	nchar = ctoi (str, ip, min)
	if (nchar == 0)
	    min = 0

	# Decode the seconds field.
	ip = lst + 1
	while (IS_WHITE(str[ip]))
	    ip = ip + 1
	if (str[ip] == '+' || str[ip] == '-')
	    ip = ip + 1
	nchar = ctor (str, ip, sec)
	if (nchar == 0)
	    sec = 0.0

	# Reformat the HMS card.
	if (stridx (minus, str) > 0 || deg < 0 || min < 0 || sec < 0.0) {
	    call sprintf (str, maxch, "%c%d:%02d:%05.2f")
		call pargc (minus)
	        call pargi (abs (deg))
	        call pargi (abs (min))
	        call pargr (abs (sec))
	} else {
	    call sprintf (str, maxch, "%2d:%02d:%05.2f")
	        call pargi (deg)
	        call pargi (abs (min))
	        call pargr (abs (sec))
	}

	return (strlen (str))
end


# RFT_GET_COMMENT -- Extract the comment field from a FITS card.

procedure rft_get_comment (card, comment, maxch)

char	card[ARB]		# FITS card
char	comment[ARB]		# comment string
int	maxch			# maximum number of characters

int	istart, j

begin
	istart = 0
	for (j = LEN_CARD; (j >= 1) && (card[j] != '\''); j = j - 1) {
	    if (card[j] == '/') {
		for (istart = j + 1; IS_WHITE(card[istart]) && istart <=
		    LEN_CARD; istart = istart + 1)
		    ;
		break
	    }
	}

	if (istart == 0)
	    comment[1] = EOS
	else
	    call strcpy (card[istart], comment, LEN_CARD - istart + 1 )
end


# RFT_GET_FITS_STRING -- Extract a string from a FITS card and trim trailing
# blanks. The EOS is marked by either ', /, or the end of the card.
# There may be an optional opening ' (FITS standard).

procedure rft_get_fits_string (card, str, maxchar)

char	card[ARB]		# FITS card
char	str[ARB]		# FITS string
int	maxchar			# maximum number of characters

int	j, istart, nchar

begin
	# Check for opening quote
	for (istart = COL_VALUE; istart <= LEN_CARD && card[istart] != '\'';
	    istart = istart + 1)
	    ;
	istart = istart + 1

	# Check for closing quote.
	for (j = istart; (j<LEN_CARD)&&(card[j]!='\''); j = j + 1)
	    ;
	for (j = j - 1; (j >= istart) && (card[j] == ' '); j = j - 1)
	    ;
	nchar = min (maxchar, j - istart + 1)

	# Copy the string.
	if (nchar <= 0)
	    str[1] = EOS
	else
	    call strcpy (card[istart], str, nchar)
end


# RFT_EQUALD -- Procedure to compare two double precision numbers for equality
# to within the machine precision for doubles.

bool procedure rft_equald (x, y)

double	x, y		# the two numbers to be compared for equality

int	ex, ey
double	x1, x2, normed_x, normed_y

begin
	if (x == y)
	    return (true)

	call rft_normd (x, normed_x, ex)
	call rft_normd (y, normed_y, ey)

	if (ex != ey)
	    return (false)
	else {
	    x1 = 1.0d0 + abs (normed_x - normed_y)
	    x2 = 1.0d0 + NEPSILON * EPSILOND
	    return (x1 <= x2)
	}
end


# RFT_NORMED -- Normalize a double precision number x to the value normed_x,
# in the range [1-10]. Expon is returned such that x = normed_x *
# (10.0d0 ** expon).

procedure rft_normd (x, normed_x, expon)

double	x			# number to be normailized
double	normed_x		# normalized number
int	expon			# exponent

double	ax

begin
	ax = abs (x)
	expon = 0

	if (ax > 0) {
	    while (ax < (1.0d0 - NEPSILON * EPSILOND)) {
		ax = ax * 10.0d0
		expon = expon - 1
	    }

	    while (ax >= (10.0d0 - NEPSILON * EPSILOND)) {
		ax = ax / 10.0d0
		expon = expon + 1
	    }
	}

	if (x < 0)
	    normed_x = -ax
	else
	    normed_x = ax
end


# RFT_TRIM_CARD -- Procedure to trim trailing whitespace from the card

procedure rft_trim_card (incard, outcard, maxch)

char	incard[ARB]		# input FITS card image 
char	outcard[ARB]		# output FITS card
int	maxch			# maximum size of card

int	ip

begin
	ip = maxch
	while (incard[ip] == ' ' || incard[ip] == '\t' || incard[ip] == '\0')
	    ip = ip - 1
	call amovc (incard, outcard, ip)
	outcard[ip+1] = '\n'
	outcard[ip+2] = EOS
end


# RFT_LAST_CARD -- Remove a partially written card from the data base

procedure rft_last_user (user, maxch) 

char	user[ARB]	# user area
int	maxch		# maximum number of characters

int	ip

begin
	ip = maxch
	while (user[ip] != '\n')
	    ip = ip - 1
	user[ip+1] = EOS
end


# RFT_SET_IMAGE_PIXTYPE -- Set remaining header fields not set in
# rft_read_header.

procedure rft_set_image_pixtype (fits, im, bscale, bzero)

pointer fits            # FITS data structure
pointer im              # IRAF image pointer
double	bscale		# FITS scaling parameter
double	bzero		# FITS offset parameter

bool	rft_equald()
include "rfits.com"

begin
        # Determine data type from BITPIX if user data type not specified.

        if (data_type == ERR) {
            if (BITPIX(fits) < 0) {
                if (abs (BITPIX(fits)) <= (SZ_REAL * SZB_CHAR * NBITS_BYTE))
                    PIXTYPE(im) = TY_REAL
                else
                    PIXTYPE(im) = TY_DOUBLE
            } else if (SCALE(fits) == YES) {
		if (rft_equald (bscale, 1.0d0)) {
		    if (rft_equald (bzero / 32768.0d0, 1.0d0))
                        PIXTYPE(im) = TY_USHORT
		    else
                        PIXTYPE(im) = TY_REAL
		} else
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


# Copy the global header into the output image header.

procedure rft_gheader (im, gim, fd_usr, card, len_card, ndiscard, long_header)

pointer	im		# IRAF image header descriptor
pointer	gim		# IRAF global image header descriptor
int	fd_usr		# IRAF image header user area
char	card[ARB]	# FITS card
int	len_card	# length of FITS card
int	ndiscard	# number of cards discarded
int	long_header	# print the long header

int	ngcards, gim_lenuser, ninherit, count
pointer	sp, indices, idb_gim, grp, irp
bool	streq()
int	strlen(), idb_nextcard(), idb_find()
pointer	idb_open()
errchk	putline()

begin
	# Initialize.
	call smark (sp)
	ngcards = strlen (UNKNOWN(gim)) / (len_card + 1)
	call salloc (indices, ngcards, TY_INT)

	# Mark the global header cards which are to be inherited. These
	# include all COMMENT, HISTORY, and BLANK cards, plus all those
	# cards which do not already have values in the extension header.
	count = 0
	idb_gim = idb_open (gim, gim_lenuser)
	while (idb_nextcard (idb_gim, grp) != EOF) {
	    if (count >= ngcards)
		break
	    call strcpy (Memc[grp], card, 8)
	    if (streq (card, "COMMENT "))
		Memi[indices+count] = YES
	    else if (streq (card, "HISTORY "))
		Memi[indices+count] = YES
	    else if (streq (card, "        "))
		Memi[indices+count] = YES
	    else if (idb_find (im, card, irp) > 0)
		Memi[indices+count] = NO
	    else 
		Memi[indices+count] = YES
	    count = count + 1
	}
	call idb_close (idb_gim)

	# Open the global header image user area and loop through the cards.
	ninherit = 0
	count = 0
	idb_gim = idb_open (gim, gim_lenuser)
	while (idb_nextcard (idb_gim, grp) != EOF) {
	    if (Memi[indices+count] == YES) {
	        call strcpy (Memc[grp], card, len_card)
	        card[len_card+1] = '\n'
	        card[len_card+2] = EOS
	        if (ndiscard > 1)
		    ndiscard = ndiscard + 1
	        else {
	            iferr (call putline (fd_usr, card))
		        ndiscard = ndiscard + 1
		    else
		        ninherit = ninherit + 1
	        }
	    }
	    count = count + 1
	}
	call idb_close (idb_gim)

	if (long_header == YES) {
	    call printf ("%d global header keywords were inherited\n")
		call pargi (ninherit)
	}

	call sfree (sp)
end


# RFT_TYPESTRING -- Procedure to set the iraf datatype keyword.

procedure rft_typestring (data_type, type_str, maxch)

int     data_type	# the IRAF data type
char    type_str[ARB]	# the output IRAF type string
int	maxch		# maximum size of the type string

begin
        switch (data_type) {
        case TY_SHORT:
            call strcpy ("SHORT", type_str, maxch)
        case TY_USHORT:
            call strcpy ("USHORT", type_str, maxch)
        case TY_INT:
            call strcpy ("INTEGER", type_str, maxch)
        case TY_LONG:
            call strcpy ("LONG", type_str, maxch)
        case TY_REAL:
            call strcpy ("REAL", type_str, maxch)
        case TY_DOUBLE:
            call strcpy ("DOUBLE", type_str, maxch)
        case TY_COMPLEX:
            call strcpy ("COMPLEX", type_str, maxch)
        default:
            call strcpy ("UNKNOWN", type_str, maxch)
        }
end


