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

int procedure rft_read_header (fits_fd, fits, im)

int	fits_fd			# FITS file descriptor
pointer	fits			# FITS data structure
pointer	im			# IRAF image descriptor

int	i, stat, nread, max_lenuser, fd_usr, ndiscard
char	card[LEN_CARD+1]
int	rft_decode_card(), rft_init_read_pixels(), rft_read_pixels(), strmatch()
int	stropen()
errchk	rft_decode_card, rft_init_read_pixels, rft_read_pixels
errchk	stropen, close

include "rfits.com"

begin
	# Initialization.
	SIMPLE(fits) = NO
	BITPIX(fits) = INDEFI
	NAXIS(im) = INDEFI
	do i = 1, IM_MAXDIM
	    IM_LEN(im,i) = INDEFL
	SCALE(fits) = NO
	FITS_BSCALE(fits) = 1.0d0
	FITS_BZERO(fits) = 0.0d0
	BLANKS(fits) = NO
	BLANK_VALUE(fits) = INDEFL
	NRECORDS(fits) = 0
	ndiscard = 0
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
		return (EOF)
	    } else if ((nread == 0) && strmatch (card, "^SIMPLE  ") == 0) {
		call flush (STDOUT)
		call error (30,
		    "RFT_READ_HEADER: Not a FITS file (no SIMPLE keyword)")
	    } else if (i != LEN_CARD) {
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

	# Print optional short header.
	if (short_header == YES && long_header == NO) {
	    if (make_image == NO && old_name == YES) {
	        call printf ("(%s)  %-20.20s  ")
		    call pargstr (IRAFNAME(fits))
		    call pargstr (OBJECT(im))
	    } else {
	        call printf ("%-20.20s  ")
		    call pargstr (OBJECT(im))
	    }
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
	}

	# Let the user know if there is not enough space in the user area.
	if (ndiscard > 0) {
	    call printf (
	        "Warning: User area too small %d card images discarded\n")
	    call pargi (ndiscard)
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

char	cval, str[LEN_CARD]
double	dval
int	nchar, ival, i, j, k, len
pointer	sp, comment

bool	rft_equald()
int	strmatch(), ctoi(), ctol(), ctod(), cctoc(), rft_hms()
errchk	putline

include	"rfits.com"

begin
	call smark (sp)
	call salloc (comment, SZ_LINE, TY_CHAR)

	i = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	    call sfree (sp)
	    return(YES)
	} else if (strmatch (card, "^SIMPLE  ") != 0) {
	    if (SIMPLE(fits) == YES)
		call printf ("Warning: Duplicate SIMPLE keyword ignored\n")
	    else {
	        nchar = cctoc (card, i, cval)
	        if (cval != 'T')
		    call error (13, "RFT_DECODE_CARD: Non-standard FITS format")
		SIMPLE(fits) = YES
	    }
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	    if (! IS_INDEFI(BITPIX(fits)))
		call printf ("Warning: Duplicate BITPIX keyword ignored\n")
	    else
	        nchar = ctoi (card, i, BITPIX(fits))
	} else if (strmatch (card, "^BLANK   ") != 0) {
	    BLANKS(fits) = YES
	    nchar = ctol (card, i, BLANK_VALUE(fits))
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	    if (! IS_INDEFI(NAXIS(im)))
		call printf ("Warning: Duplicate NAXIS keyword ignored\n")
	    else
	        nchar = ctoi (card, i, NAXIS(im))
	    if (NAXIS(im) > IM_MAXDIM)
		call error (5, "RFT_DECODE_CARD: FITS NAXIS too large")
	} else if (strmatch (card, "^NAXIS") != 0) {
	    k = strmatch (card, "^NAXIS")
	    nchar = ctoi (card, k, j)
	    if (! IS_INDEFL(NAXISN(im,j))) {
		call printf ("Warning: Duplicate NAXIS%d keyword ignored\n")
		    call pargi (j)
	    } else
	        nchar = ctol (card, i, NAXISN(im, j))
	} else if (strmatch (card, "^GROUPS  ") != 0) {
	    nchar = cctoc (card, i, cval)
	    if (cval == 'T') {
		NAXIS(im) = 0
		call error (6, "RFT_DECODE_CARD: Group data not implemented")
	    }
	} else if (strmatch (card, "^TABLES  ") != 0) {
	    nchar = ctoi (card, i, ival)
	    if (ival > 0)
		call printf ("Warning: FITS special records not decoded\n")
	} else if (strmatch (card, "^BSCALE  ") != 0) {
	    nchar = ctod (card, i, dval)
	    if (nchar > 0)
	        FITS_BSCALE(fits) = dval
	    else
		call printf ("Warning: Error decoding BSCALE, BSCALE=1.0\n")
	    if (! rft_equald (dval, 1.0d0) && (scale == YES))
		SCALE(fits) = YES
	} else if (strmatch (card, "^BZERO   ") != 0) {
	    nchar = ctod (card, i, dval)
	    if (nchar > 0)
	        FITS_BZERO(fits) = dval
	    else
		call printf ("Warning: Error decoding BZERO, BZERO=0.0\n")
	    if (! rft_equald (dval, 0.0d0) && (scale == YES))
		SCALE(fits) = YES
	} else if (strmatch (card, "^OBJECT  ") != 0) {
	    call rft_get_fits_string (card, OBJECT(im), SZ_OBJECT)
	} else if (strmatch (card, "^IRAFNAME") != 0) {
	    call rft_get_fits_string (card, IRAFNAME(fits), SZ_FNAME)
	} else if (strmatch (card, "^ORIGIN  ") != 0) {
	    call rft_trim_card (card, card, LEN_CARD)
	    call strcat (card[i], HISTORY(im), SZ_HISTORY)
	} else if (strmatch (card, "^DATE    ") !=  0) {
	    call rft_trim_card (card, card, LEN_CARD)
	    call strcat (card[i], HISTORY(im), SZ_HISTORY)
	#} else if (strmatch (card, "^HISTORY ") != 0) {
	    #call rft_trim_card (card, card, LEN_CARD)
	    #call strcat (card[i - 2], HISTORY(im), SZ_HISTORY)
	} else if (strmatch (card, "^UT      ") != 0) {
	    len = rft_hms (card, str, Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("UT", str, len, card, Memc[comment])
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
	    len = rft_hms (card, str, Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("ZD", str, len, card, Memc[comment])
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
	    len = rft_hms (card, str, Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("ST", str, len, card, Memc[comment])
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
	    len = rft_hms (card, str, Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("RA", str, len, card, Memc[comment])
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
	    len = rft_hms (card, str, Memc[comment], LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("DEC", str, len, card, Memc[comment])
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
