# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include	<imio.h>
include	"imtext.h"

# RT_RHEADER -- read FITS header, saving the image dimension information in
# the image header.  The format (integer/floating point) is returned.

procedure rt_rheader (tf, im, format)

int	tf		# File descriptor for input text file
pointer	im		# Pointer to image header
int	format		# Format of text file pixels (integer/floating point)

pointer	sp, wt, card
bool	streq()
int	ncard, fd_user, max_lenuser
int	getline(), rt_decode_card(), stridxs(), strlen(), stropen()
errchk	getline, rt_decode_card

begin
	call smark (sp)
	call salloc (wt, LEN_WT, TY_STRUCT)
	call salloc (card, LEN_CARD+1, TY_CHAR)

	Memc[card+LEN_CARD] = '\n'
	Memc[card+LEN_CARD+1] = EOS

	# Prepare user area string to be written
	max_lenuser = (LEN_IMDES + IM_LENHDRMEM(im) - IMU) * SZ_STRUCT - 1
	fd_user = stropen (Memc[IM_USERAREA(im)], max_lenuser, NEW_FILE)

	ncard = 1
	repeat {
	    if (getline (tf, Memc[card]) == EOF) 
	        call error (2, "RT_RHEADER: EOF encountered before END card")

	    ncard = ncard + 1
	    if (rt_decode_card (wt, im, fd_user, Memc[card]) == YES) 
		break
	}

	# Encountered END card; examine a few header keyword values.  From
	# the FORMAT keyword, determine if the pixel values are written as
	# integers, floating point numbers or complex numbers.

	if (strlen (FORM(wt)) > 0) {
            if (stridxs ("I", FORM(wt)) > 0)
	        format = INT_FORM
	    else if (stridxs ("(", FORM(wt)) > 0)
		format = CPX_FORM
	    else 
	        format = FP_FORM
	} else
	    format = UNSET

	# The image pixel type is set by the IRAFTYPE keyword value.

	if (streq (IRAFTYPE(wt), "SHORT INTEGER"))
	    IM_PIXTYPE (im) = TY_SHORT
	else if (streq (IRAFTYPE(wt), "UNSIGNED SHORT INT"))
	    IM_PIXTYPE (im) = TY_USHORT
	else if (streq (IRAFTYPE(wt), "INTEGER"))
	    IM_PIXTYPE (im) = TY_INT
	else if (streq (IRAFTYPE(wt), "LONG INTEGER"))
	    IM_PIXTYPE (im) = TY_LONG
	else if (streq (IRAFTYPE(wt), "REAL FLOATING"))
	    IM_PIXTYPE (im) = TY_REAL
	else if (streq (IRAFTYPE(wt), "DOUBLE FLOATING"))
	    IM_PIXTYPE (im) = TY_DOUBLE
	else if (streq (IRAFTYPE(wt), "COMPLEX"))
	    IM_PIXTYPE (im) = TY_COMPLEX

	call close (fd_user)
	call sfree (sp)
end


# RT_DECODE_CARD -- Decode a FITS format card and return YES when the END
# card is encountered.   The decoded value is stored in the image header,
# or in the user area if there is no other place for it.  The END card is
# tested only to the first three characters; strictly speaking the END
# card begins with the 8 characters "END     ".

int procedure rt_decode_card (wt, im, fd, card)

pointer	wt			# Pointer to wtextimage keyword structure
pointer	im			# Pointer to image header being written
int	fd			# File descriptor of user area
char	card[ARB]		# Card image read from FITS header

int	nchar, ival, i, j, k, ndim

int	strmatch(), ctoi()
errchk	rt_get_fits_string, putline, putline

begin

	i = COL_VALUE
	if (strmatch (card, "^END") > 0)
	    return (YES)

	else if (strmatch (card, "^NAXIS   ") > 0) {
	    nchar = ctoi (card, i, ndim)
	    if (ndim > 0)
		IM_NDIM(im) = ndim

	} else if (strmatch (card, "^NAXIS") > 0) {
	    k = strmatch (card, "^NAXIS")
	    nchar = ctoi (card, k, j)
	    nchar = ctoi (card, i, IM_LEN(im,j))

 	} else if (strmatch (card, "^NDIM    ") > 0)
	    nchar = ctoi (card, i, IM_NDIM(im))

 	else if (strmatch (card, "^LEN") > 0) {
 	    k = strmatch (card, "^LEN")
	    nchar = ctoi (card, k, j)
	    nchar = ctoi (card, i, IM_LEN(im,j))

	} else if (strmatch (card, "^BITPIX  ") > 0) {
	    nchar = ctoi (card, i, ival)
	    if (ival != 8) 
		call error (6, "Not 8-bit ASCII characters")

	} else if (strmatch (card, "^FORMAT  ") > 0) {
	    call rt_get_fits_string (card, FORM(wt), SZ_STRING)
	} else if (strmatch (card, "^IRAFTYPE") > 0) {
	    call rt_get_fits_string (card, IRAFTYPE(wt), SZ_STRING)
	} else if (strmatch (card, "^OBJECT  ") > 0) {
	    call rt_get_fits_string (card, IM_TITLE(im), SZ_IMTITLE)
	} else {
	    # Putline returns an error if there is no room in the user area
	    iferr (call putline (fd, card)) {
		call eprintf ("Space in user area has been exceeded\n")
		return (YES)
	    }
	}

	return (NO)
end


# RT_GET_FITS_STRING -- Extract a string from a FITS card and trim trailing
# blanks. The EOS is marked by either ', /, or the end of the card.
# There may be an optional opening ' (FITS standard).

procedure rt_get_fits_string (card, str, maxchar)

char	card[ARB]		# Input card image containing keyword and value
char	str[maxchar]		# Output string 
int	maxchar			# Maximum number of characters output
int	j, istart, nchar

begin
	# Check for opening quote
	if (card[COL_VALUE] == '\'')
	    istart = COL_VALUE + 1
	else
	    istart = COL_VALUE

	for (j=istart;  (j < LEN_CARD) && (card[j] != '\'');  j=j+1)
	    ;
	for (j=j-1;     (j >= istart) && (card[j] == ' ');  j=j-1)
	    ;

	nchar = min (maxchar, j - istart + 1)
	call strcpy (card[istart], str, nchar)
end
