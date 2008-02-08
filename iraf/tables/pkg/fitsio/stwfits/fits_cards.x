include <imhdr.h>
include "wfits.h"

# WFT_STANDARD_CARD -- Procedure for fetching the minimum header
# parameters required by fits. The end card is encoded separately.

# Nelson Zarate	??		original
# Phil Hodge	28-Sept-2005	In procedure chgfmt_3d, sppfmt was
#			incorrectly declared to be an array of int,
#			but it's an array of char.  (It isn't used
#			in this function anyway.)

int procedure wft_standard_card (cardno, im, fits, axisno, card)

int	cardno		# number of FITS standard card
pointer	im		# pointer to the IRAF image
pointer	fits		# pointer to the FITS structure
int	axisno		# axis number
char	card[ARB]	# FITS card image

int	pixval
char	keyword[LEN_KEYWORD]
errchk	wft_encodeb, wft_encodei, wft_encodel, wft_axis_encode
include "wfits.com"

begin
	# Get mandatory keywords.
	switch (cardno) {
	case FIRST_CARD:
	    if (ext_type == IMAGE)
		call wft_encodec ("XTENSION", "IMAGE", card, 
				  "IMAGE extension")
	    else
		call wft_encodeb ("SIMPLE", YES, card, "FITS STANDARD")
	case SECOND_CARD:
	    if (ieee == YES)
		pixval = -FITS_BITPIX(fits)
	    else
		pixval = FITS_BITPIX(fits)
	    call wft_encodei ("BITPIX", pixval, card, "FITS BITS/PIXEL")
	case THIRD_CARD:
	    call wft_encodei ("NAXIS", NAXIS(im), card, "NUMBER OF AXES")
	default:
	    if (NAXIS(im) >= (cardno - 3)) {
		call wft_axis_encode ("NAXIS", keyword, axisno)
		call wft_encodel (keyword, NAXISN(im, axisno), card, "")
		axisno = axisno + 1
	    } else if (cardno >= NAXIS(im) + 4) {
		if (ext_type == IMAGE) { 
		    if (cardno == NAXIS(im) + 4) 
			call wft_encodei ("PCOUNT", 0, card, 
					  "No 'random' parameters")
		    else 
			call wft_encodei ("GCOUNT", 1, card, "Only one group")
		    #	          return (NO)
		} else {
		    call wft_encodeb ("EXTEND", YES, card,
				      "There maybe standard extensions")
		}
	    }
	}

	return (YES)
end


include <imio.h>
# WFT_OPTION_CARD -- Procedure for fetching  optional FITS header parameters.
# At present these are bscale, bzero, bunit, blank, object, origin, date,
# irafmax, irafmin, iraf type and iraf bits per pixel. Blank is only encoded
# if there are a nonzero number of blanks in the IRAF image. Bunit and object
# are only encoded if the appropriate IRAF strings are defined. Bzero, bscale,
# irafmax, irafmin, iraf type and iraf bits per pixel are only encoded if
# there is a pixel file.
#
# Revision history  
#
# Jan 21 1991	Change constant names (see wfits.h)
#	  	Change keyword names: IRAFNAME to FILENAME, IRAF-MAX to 
#	 	ALLG-MAX, IRAF_MIN to ALLG-MIN, IRAFTYPE to ODATTYPE.
#		Drop IRAFBP.
# Aug 5 1997	Change code to write ODATTYPE for template images

int procedure wft_option_card (im, fits, optiono, card)

pointer	im		# pointer to the IRAF image
pointer	fits		# pointer to FITS structure
int	optiono		# number of the option card
char	card[ARB]	# FITS card image

char	datestr[LEN_DATE], comment[LEN_CARD]
int	gcount, stat, prec, len_object
pointer rp, sp
bool	gi_geis()
int	gi_gstfval(), imaccf(), idb_findrecord(), iki_access()
errchk	wft_encoded, wft_encodec, wft_encode_blank, wft_encoder, wft_encodei
errchk	wft_encode_date 

include "wfits.com"


begin
	stat = YES

	# get optional keywords
	switch (optiono) {
	case KEY_BSCALE:
	    if (NAXIS(im) <= 0)
		stat = NO
	    else {
		prec = NDEC_DOUBLE
		if (BSCALE(fits) == 1.0d0 && BZERO(fits) == 0.0d0)
		    prec = 2
	        call wft_encoded ("BSCALE", BSCALE(fits), card,
				  "REAL = TAPE*BSCALE + BZERO", prec)
	    }
	case KEY_BZERO:
	    if (NAXIS(im) <= 0)
		stat = NO
	    else {
		prec = NDEC_DOUBLE
		if (BSCALE(fits) == 1.0d0 && BZERO(fits) == 0.0d0)
		    prec = 2
	        call wft_encoded ("BZERO", BZERO(fits), card, "", prec)
	    }
	case KEY_BUNIT:
	    stat = NO
	case KEY_BLANK:
	    stat = NO
	case KEY_OPSIZE:
	    if (! gi_geis (im) || (sdasmgcv < 0))
		stat = NO
	    else {
		if (gi_gstfval (im, "PCOUNT") == 0) {
		    len_object = 0
		} else {
		    len_object = gi_gstfval(im, "PSIZE")
		}
		call wft_encodei ("OPSIZE", len_object, card,
				  "PSIZE of original image")
	    }
	case KEY_OBJECT:
	    # Create OBJECT keyword.
	    # First pick is TARGNAME; if is not in the user area, then
	    # look for OBJECT.
	    if (idb_findrecord (im, "TARGNAME", rp) != 0) {
		# Do not create OBJECT keyword if TARGNAME is in the user area.
		stat = NO
	    } else {
		if (imaccf(im, "OBJECT") == YES)
		    stat = NO
		else {
		    # Create Object keyword for input IMH only.
		    # This is called assume that OIF file index is 1.
		    call smark(sp)
		    call salloc(rp, SZ_PATHNAME, TY_CHAR)
		    if (iki_access(IM_NAME(im), Memc[rp], comment,0) == 1) {
			call wft_encodec ("OBJECT", OBJECT(im), card, "")
		    }
		    call sfree(sp)
		}
	    }
	case KEY_ORIGIN:
	    call wft_encodec ("ORIGIN", "STScI-STSDAS", card,
			      "Fitsio version 21-Feb-1996")
	case KEY_DATE:
	    call wft_encode_date (datestr, LEN_DATE)
	    call wft_encodec ("FITSDATE", datestr, card, 
			      "Date FITS file was created")
	case KEY_FILENAME:
	    call wft_encodec ("FILENAME", IRAFNAME(fits), card, 
			      "Original filename")
	case KEY_ALLGMAX:
	    if (NAXIS(im) <= 0 || (sdasmgcv < 0))
		stat = NO
	    else
	        call wft_encoder ("ALLG-MAX", IRAFMAX(fits), card, 
				  "Data max in all groups", NDEC_REAL)
	case KEY_ALLGMIN:
	    if (NAXIS(im) <= 0 || (sdasmgcv < 0))
		stat = NO
	    else
	        call wft_encoder ("ALLG-MIN", IRAFMIN(fits), card,
				  "Data min in all groups", NDEC_REAL)
	case KEY_IRAFTYPE:
	    switch (TYPE_STRING(fits)) {
	    case 'S':
		call strcpy ("Original datatype: 16 bits integer", comment,
			     LEN_CARD)
	    case 'U':
		call strcpy ("Original datatype: Unsigned 16 bits int",
			     comment, LEN_CARD)
	    case 'I':
		call strcpy ("Original datatype: 32 bits integer", comment,
			     LEN_CARD)
	    case 'F':
		call strcpy ("Original datatype: Single precision real",
			     comment, LEN_CARD)
	    case 'D':
		call strcpy ("Original datatype: Double precision real",
			     comment, LEN_CARD)
	    default:
		call strcpy ("Original datatype", comment, LEN_CARD)
	    }
	    call wft_encodec ("ODATTYPE", TYPE_STRING(fits), 
			      card, comment)
	case KEY_SDASMGNU:
	    if (! gi_geis (im) || (sdasmgcv < 0))
		stat = NO
	    else {
		gcount = gi_gstfval (im, "GCOUNT")
		call wft_encodei ("SDASMGNU", gcount, card, 
				  "Number of groups in original image")
	    }
	default:
	    stat = NO
	}

	optiono = optiono + 1

	return (stat)
end


define SZ_PTYPE		8
# WFT_XDIM_CARD -- procedure to reset image structure to reflect a change
# in dimensionality if the number of group in the is > 1. This is valid
# only if the opened image is SDAS type.

int procedure wft_xdim_card (im, xdimno, card)

pointer	im		# image descriptor
int	xdimno  	# counter
char	card[ARB]

int	ndim
char	pname[SZ_PTYPE]
int	gi_gstfval(), stat, ic

include "wfits.com"

begin

	stat = YES
	if (sdasmgcv < 0)   # is not a request for xtra dimension
	    return(NO)
	else {
	    ndim = IM_NDIM(im)
	    switch (xdimno) {
	    case FIRST_CARD:
		# Set extension flag on
		#	      extensions = YES
		# Change dimensionality only if ngroups > 1
		if (gi_gstfval(im, "GCOUNT") <= 1)
		    stat = NO
		else {
		    call sprintf (pname, SZ_PTYPE, "CTYPE%d")
		    call pargi (ndim)
		    call wft_encodec (pname, "GROUP_NUMBER", 
				      card, "Extra dimension axis name")
		    #Initialize counter for xtra rows and columns
		    ic = 2
		}
	    case SECOND_CARD:
		call sprintf (pname, SZ_PTYPE, "CD%d_%d")
		call pargi (ndim)
		call pargi (ndim)
		call wft_encodei (pname, 1, card, "")
	    case THIRD_CARD:
		call sprintf (pname, SZ_PTYPE, "CD%d_%d")
		call pargi (ndim)
		call pargi (xdimno-2)
		call wft_encodei (pname, 0, card, "")
	    case FOURTH_CARD:
		call sprintf (pname, SZ_PTYPE, "CD%d_%d")
		call pargi (xdimno-3)
		call pargi (ndim)
		call wft_encodei (pname, 0, card, "")
	    default:
		if (ndim <= 2)
		    return(NO) 
		else if (mod(ic,2) == 0) {
		    call sprintf (pname, SZ_PTYPE, "CD%d_%d")
		    call pargi (ic)
		    call pargi (ndim)
		    call wft_encodei (pname, 0.0, card, "")
		} else if (mod(ic,2) == 1) {
		    call sprintf (pname, SZ_PTYPE, "CD%d_%d")
		    call pargi (ndim)
		    call pargi (ic-1)
		    call wft_encodei (pname, 0.0, card, "")
		}
		ic = ic + 1
		if (ndim*2+1 == xdimno)
		    return (NO)
	    }
	} 
	xdimno = xdimno + 1
	return(stat)
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

int procedure wft_unknown_card (fits, im, up, card)

pointer fits		# points to the fits descriptor
pointer	im		# pointer to the IRAF image
int	up		# pointer to next character in the unknown string
char	card[ARB]	# FITS card image

char	cval
int	stat
char	chfetch()
int	strmatch()

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
	        if (strmatch (card, "^GROUPS  ") != 0)
		    stat = NO
	        else if (strmatch (card, "^GCOUNT  ") != 0)
		    stat = NO
	        else if (strmatch (card, "^PCOUNT  ") != 0)
		    stat = NO
	        else if (strmatch (card, "^PSIZE   ") != 0)
		    stat = NO
	        else if (strmatch (card, "^BSCALE  ") != 0)
		    stat = NO
	        else if (strmatch (card, "^BZERO   ") != 0)
		    stat = NO
	        else if (strmatch (card, "^BLANK   ") != 0)
		    stat = NO
		# For consistancy set the values below the same as
		# the IRAFMAX and IRAFMIN ones.
	        else if (strmatch (card, "^DATAMIN ") != 0) {
		    call wft_encoder ("DATAMIN", IRAFMIN(fits), card,
				      "DATA MIN", NDEC_REAL)
		    stat = YES
	        } else if (strmatch (card, "^DATAMAX ") != 0) {
		    call wft_encoder ("DATAMAX", IRAFMAX(fits), card,
				      "DATA MAX", NDEC_REAL)
		    stat = YES
	        } else
                    stat = YES
	    }

	    return (stat)
	}
end

include <imhdr.h>
include <tbset.h>

# TAB_STANDARD_CARD -- Procedure for fetching the minimum header
# parameters required by fits. The end card is encoded separately.

int procedure tab_standard_card (cardno, tp, ext, card)

pointer	tp		# pointer to the table file
pointer	ext		# pointer to the extension structure
char	card[ARB]	# FITS card image
int	cardno		# number of FITS standard card

int	tbpsta()
int	nrows, ncols
errchk	wft_encodeb, wft_encodei, wft_encodel, wft_axis_encode
include "wfits.com"

begin
	# Get mandatory keywords.
	switch (cardno) {
	case FIRST_CARD:
	    if (ext_type == BINTABLE)
		call wft_encodec ("XTENSION", "BINTABLE", card, 
				  "Binary table extension")
	    else
		call wft_encodec ("XTENSION", "TABLE", card,
				  "Ascii table extension")
	case SECOND_CARD:
	    call wft_encodei ("BITPIX", EXT_BITPIX(ext), card,
			      "8-bits per 'pixels'")
	case THIRD_CARD:
	    call wft_encodei ("NAXIS", 2, card, "Simple 2-D matrix")
	case FOURTH_CARD:
	    call wft_encodel ("NAXIS1", EXT_LENAXIS(ext,1), card,
			      "Number of characters per row")	
	case FIFTH_CARD:
	    nrows = tbpsta (tp, TBL_NROWS)	    
	    call wft_encodel ("NAXIS2", nrows, card, "The number of rows")
	case SIXTH_CARD:
	    call wft_encodei ("PCOUNT", 0, card, "No 'random' parameters")
	case SEVENTH_CARD:
	    call wft_encodei ("GCOUNT", 1, card, "Only one group")
	case EIGHTH_CARD:
	    ncols = tbpsta (tp, TBL_NCOLS)
	    call wft_encodei ("TFIELDS", ncols, card,"Number of fields per row")
	case NINTH_CARD:
	    call wft_encodec ("EXTNAME", EXTNAME(ext), 
			      card, "Name of table")
	case 10:

	    call wft_blank_card (card)
	}

	return (YES)
end


# TAB_COLUMN_CARD -- Procedure for fetching FITS table column information.

int procedure tab_column_card (tp, colp, ext, colno, descno, card)

pointer tp		# pointer to the table descriptor
pointer	colp		# pointer to a column descriptor
pointer	ext		# pointer to extension structure
int	colno		# number of the table column
int	descno
char	card[ARB]	# FITS card image

pointer colpp
char	nullstr[SZ_LINE], keyword[LEN_KEYWORD]
int	i
char	colname[SZ_COLNAME], colunits[SZ_COLUNITS], colfmt[SZ_COLFMT]
char	forfmt[SZ_COLFMT]
int	type, lenfmt, coloff, nelem

int	tbcigi()
pointer	tbcnum()
errchk	wft_encoded, wft_encodec, wft_encode_blank, wft_encoder, wft_encodei
errchk	wft_encode_date, tcbnum, tbcinf

include "wfits.com"

begin

	switch (descno) {
	case FIRST_CARD:
	    call tbcigt (colp, TBL_COL_NAME, colname, SZ_COLNAME)
	    call wft_axis_encode ("TTYPE", keyword, colno)
	    call wft_encodec (keyword, colname, card, "")
	    descno = descno + 1
	    if (ext_type == BINTABLE)      # Skip TBCOL card
		descno = descno + 1
	case SECOND_CARD:
	    call wft_axis_encode ("TBCOL", keyword, colno)
	    if (colno == 1)
		coloff = 1
	    else {
		colpp = tbcnum (tp, colno-1)
		call tbcigt (colpp, TBL_COL_FMT, colfmt, SZ_COLFMT)
		# change format H to F or G to E or D if any in colfmt.
		type = tbcigi (colpp, TBL_COL_DATATYPE)
		call chgfmt (colfmt, type, forfmt, lenfmt)
		coloff = coloff + lenfmt
	    }
	    call wft_encodei (keyword, coloff, card, "")
	    descno = descno + 1
	case THIRD_CARD:	
	    call tbcigt (colp, TBL_COL_FMT, colfmt, SZ_COLFMT)
	    # change format H to F or G to E or D if any in colfmt.
	    type = tbcigi (colp, TBL_COL_DATATYPE)
	    if (ext_type == BINTABLE) {
		nelem = tbcigi (colp, TBL_COL_LENDATA)
		call chgfmt_3d (colfmt, type, nelem, forfmt)
	    } else
		call chgfmt (colfmt, type, forfmt, lenfmt)
	    call wft_axis_encode ("TFORM", keyword, colno)
	    call wft_encodec (keyword, forfmt, card, "")
	    descno = descno + 1
	case FOURTH_CARD:
	    # If the column datatype is Boolean preappend the string
	    # 'LOGICAL-' to the value in the TUNIT field. The fits reader
	    # should be able to look at this value and change the
	    # column datatype from character to boolean.
	    i = 1
	    if (ext_type != BINTABLE &&
		tbcigi (colp, TBL_COL_DATATYPE) == TY_BOOL) {
		call strcpy ("LOGICAL-", colunits, SZ_COLUNITS)
		i = 9
	    }
	    call tbcigt (colp, TBL_COL_UNITS, colunits[i], SZ_COLUNITS)
	    call wft_axis_encode ("TUNIT", keyword, colno)
	    call wft_encodec (keyword, colunits, card, "")
	    descno = descno + 1
	case FIFTH_CARD:
	    descno = descno + 1
	    call wft_blank_card (card)
	    if (Memb[EXT_PCUNDEF(ext)+colno-1] ) {
		type = tbcigi (colp, TBL_COL_DATATYPE)
		if (ext_type == BINTABLE) {
		    call tab_null_col (type, colno, card)
		    if (card[1] == ' ')
			return (NO)
		} else {
		    nullstr[1] = '*'
		    nullstr[2] = EOS
		    call wft_axis_encode ("TNULL", keyword, colno)
		    call wft_encodec (keyword, nullstr, card, "")
		}
	    } else
		return (NO)
	case SIXTH_CARD:
	    call tbcigt (colp, TBL_COL_FMT, colfmt, SZ_COLFMT)
	    call spp2fort (colfmt, forfmt)
	    call wft_axis_encode ("TDISP", keyword, colno)
	    call wft_encodec (keyword, forfmt, card, colfmt)
	    descno = descno + 1
	case SEVENTH_CARD:
	    call wft_blank_card (card)
	    descno = 1	
	    colno = colno + 1
	}
	return (YES)
end

include <mach.h>
include <ctype.h>

procedure tab_null_col (type, colno, card)
int	type		#i: column datatype
int     colno           #i: column number
char	card[ARB]	#o: FITS card

char	keyword[LEN_STRING]
char    buf[20]        
int	top,num,temp

begin
	# In binary tables, the TNULL keyword should only be added
	# for integer types. (BPS 02.28.97)

	if (type == TY_SHORT || type == TY_INT || type == TY_LONG) {
	    call wft_axis_encode ("TNULL", keyword, colno)

	    # Convert the numeric value of INDEF to string; we cannot use
	    # sprintf since it will be 'INDEFI'.

	    call amovkc (" ", buf, 20)
	    num = INDEFI
	    if (type == TY_SHORT) num = INDEFS
		if (num < 0) {
		    num = -num
		    buf[1] = '-'
		}

	    top = 20
	    repeat {
		temp = num / 10
		buf[top] = TO_DIGIT (num - temp * 10)
		num = temp
		top = top - 1
	    } until (num == 0)
	    if (buf[1] == '-') {
		buf[top] = '-'
		buf[1] = ' '
	    }

	    call wft_nqencode (keyword, buf, card, "")

	} else {
	    call wft_blank_card (card)
	}

end

# CHGFMT_3D --
#
procedure chgfmt_3d (sppfmt, datatype, nelem, ftnfmt)

char	sppfmt[ARB]		# i: Print format in SPP style
int	datatype		# i: Column datatype
int	nelem			# i: Number of elements per column
char	ftnfmt[ARB]		# o: The corresponding Fortran format

char	format_letter

begin
	switch(datatype) {
	case TY_REAL:
	    format_letter = 'E'
	case TY_DOUBLE:
	    format_letter = 'D'
	case TY_BOOL:
	    format_letter = 'L'
	case TY_INT,TY_LONG:
	    format_letter = 'J'
	    #	   case TY_BITARR:
	    #	       format_letter = 'X'
	case TY_SHORT:
	    format_letter = 'I'
	default:
	    # Datatype is  a negative quantity
	    # indicating the number of character.
	    nelem = abs(datatype)
	    format_letter = 'A'
	}
	call sprintf(ftnfmt, SZ_COLFMT,"%d%c")
	call pargi(nelem)
	call pargc(format_letter)

end


# CHGFMT -- convert spp format to FITS table Fortran format
#           The only formats allows by the FITS standards to be put
# 	    in the TFORMn keyword are I, E, D, A. But to actually
#           write the columns we can put more visually atractive ones
#	    like, F, or G's. (This is done with chgtyp routine)
#
#	sppfmt	ftnfmt   	comments
#	%12.5f	E12.5    	floating-point value
#	%12.5e	E12.5    	floating-point value
#	%12.5g	E12.5    	general floating-point value
#	%12d	I12      	integer
#	%012d	I12.12   	integer padded with '0' on the left
#	%12b	L12      	logical (Boolean)
#	%17a	A17      	character string
#	%12.2h	E15.7    	hh:mm:ss.dd written as hh.dddddd
#	%12.2m	E15.7    	mm:ss.dd written as mm.dddd
#	%12x	I12      	hexadecimal integer written as decimal
#

define	LOG10_16	1.204	# base 10 log of 16

procedure chgfmt (sppfmt, datatype, ftnfmt, lenfmt)

char	sppfmt[ARB]		# i: Print format in SPP style
int	datatype		# i: Column datatype
char	ftnfmt[ARB]		# o: The corresponding Fortran format
int	lenfmt			# o: Lenght of display format ('w' field)
#--
char	p_ftnfmt[SZ_COLFMT]	# pseudo-Fortran format (incl SPP extensions)
char	dot			# '.'
int	w_num, d_num		# field width and num of decimals (as in w.d)
int	nchar, ip		# for reading w and d using ctoi, and for itoc
int	dot_loc			# location of "." in format
int	stridx(), ctoi(), itoc()
int    strncmp()
include "wfits.com"

begin
	# Convert to pseudo-Fortran print format, which may not be a valid
	# Fortran format.
	call strlwr(sppfmt)     # To take care of %12.5H for example.
	call tbbptf (sppfmt, p_ftnfmt)
	if (strncmp(sppfmt, "%s", 2) ==0 ){
	    ip = 2    # NZ March 9 1995
	    nchar = itoc (abs(datatype), p_ftnfmt[ip], SZ_COLFMT-ip+1)
	}
	if (p_ftnfmt[2] == '-') {			# get rid of it
		do ip = 3, SZ_COLFMT
		    p_ftnfmt[ip-1] = p_ftnfmt[ip]
	    p_ftnfmt[SZ_COLFMT] = EOS
	}

	d_num = 0
	# We may not need this stuff; see below.
	dot = '.'
	dot_loc = stridx (dot, p_ftnfmt)
	ip = 2
	if (ctoi (p_ftnfmt, ip, w_num) > 0) {		# field width
		if (dot_loc > 0) {
		    ip = dot_loc + 1
		    if (ctoi (p_ftnfmt, ip, d_num) <= 0) # number of decimals
			d_num = 0
		} else {
		    d_num = 0
		}
	} else {
	    w_num = 0
	}
	if ((w_num < 0) || (d_num < 0))
	    call error (1, "utbptf:  invalid format")
	if (w_num == 0)
	    w_num = 6

	ip = 2
	if (def_fmt == YES) {
	    switch(datatype) {
	    case TY_REAL:
		ftnfmt[1] = 'E'
		w_num = 15
		d_num = 7
	    case TY_DOUBLE:
		ftnfmt[1] = 'D'
		w_num = 25
		d_num = 17
	    case TY_INT,TY_LONG:
		ftnfmt[1] = 'I'
		w_num = 12
		d_num = -1
	    case TY_SHORT:
		ftnfmt[1] = 'I'
		w_num = 6
		d_num = -1
	    default:
		# Datatype is  a negative quantity, use for boolean too.
		ftnfmt[1] = 'A'
		d_num = -1
		if (datatype < 0) {
		    nchar = itoc (abs(datatype), ftnfmt[ip], SZ_COLFMT-ip+1)
		    lenfmt = abs(datatype) + 1 
		    return
		}
	    }
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    if (d_num != -1) {
		ftnfmt[ip+nchar] = dot
		ip = ip + nchar + 1
		nchar = itoc (d_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    }
        } else {
	    if (p_ftnfmt[1] == 'H') {			# hours:min:sec format
		   if (datatype == TY_DOUBLE) {
		       ftnfmt[1] = 'D'
		       w_num = 21
		       d_num = 16
		   } else {	# assume single float
		       ftnfmt[1] = 'E'
		       d_num = 5
		       w_num = 9
		   }
	    } else if (p_ftnfmt[1] == 'M') 	{	# min:sec format
		   if (datatype == TY_DOUBLE) {
		       ftnfmt[1] = 'D'
		       w_num = 20
		       d_num = 16
		   } else {	# assume single float
		       ftnfmt[1] = 'E'
		       d_num = 5
		       w_num = 9
		   }
	    } else if (p_ftnfmt[1] == 'Z') {		# hexadecimal
		ftnfmt[1] = 'I'
		w_num = w_num * LOG10_16 + 1		# need more room
	    } else if (p_ftnfmt[1] == 'L') {		# Logical
		#	       ftnfmt[1] = 'I'		# change to I (1 or 0)
		ftnfmt[1] = 'A'	               # July 1991 NZ	
	    } else if (p_ftnfmt[1] == 'G') {		# hexadecimal
		ftnfmt[1] = 'E'
		if (datatype == TY_DOUBLE)
		    ftnfmt[1] = 'D'
	    } else if (p_ftnfmt[1] == 'I' && (datatype != TY_INT) &&
		       (datatype != TY_SHORT)) {
		if (datatype == TY_DOUBLE) {
		    ftnfmt[1] = 'D'
		    w_num = 25
		    d_num = 17
		} else {
		    ftnfmt[1] = 'E'
		    w_num = 15
		    d_num = 7
		}
	    } else if (p_ftnfmt[1] == 'F') {
		if (datatype == TY_DOUBLE)
		    ftnfmt[1] = 'D'
		else
		    ftnfmt[1] = 'E'
	    } else if (p_ftnfmt[1] == 'E' && datatype != TY_REAL) {
		if (datatype == TY_DOUBLE)
		    ftnfmt[1] = 'D'
	    } else {
		# No change except possibly removing a '-' sign.
		call strcpy (p_ftnfmt, ftnfmt, SZ_COLFMT)
	    }
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    if (ftnfmt[1] != 'I' && ftnfmt[1] != 'A') {
		ftnfmt[ip+nchar] = dot
		ip = ip + nchar + 1
		nchar = itoc (d_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    }
	}

	lenfmt = w_num + 1		# Separate each field bye a blank
end


# spp2fort -- convert spp format to Fortran format
# This procedure converts an SPP-stype format for display to a Fortran
# format.  The input is assumed to be in lower case, and the output will
# be in upper case.  The input and output may be the same string.
# This routine calls tbbptf to convert from SPP and then checks to make
# sure it is a legal Fortran format; if not then it will be modified.
# The following table shows examples of SPP formats and the legal Fortran
# formats returned by this routine:
#	sppfmt	ftnfmt   	comments
#	%12.5f	F12.5    	floating-point value
#	%12.5e	E12.5    	floating-point value
#	%12.5g	G12.5    	general floating-point value
#	%12d	I12      	integer
#	%012d	I12.12   	integer padded with '0' on the left
#	%12b	L12      	logical (Boolean)
#	%17a	A17      	character string
#	%12.2h	F12.6    	hh:mm:ss.dd written as hh.dddddd
#	%12.2m	F12.4    	mm:ss.dd written as mm.dddd
#	%12x	I12      	hexadecimal integer written as decimal
#
# P.E. Hodge, 7-Aug-87  Subroutine created

define	LOG10_16	1.204	# base 10 log of 16

procedure spp2fort(sppfmt, ftnfmt)

char	sppfmt[ARB]		# i: Print format in SPP style
char	ftnfmt[ARB]		# o: The corresponding Fortran format
#--
char	p_ftnfmt[SZ_COLFMT]	# pseudo-Fortran format (incl SPP extensions)
char	dot			# '.'
int	w_num, d_num		# field width and num of decimals (as in w.d)
int	nchar, ip		# for reading w and d using ctoi, and for itoc
int	dot_loc			# location of "." in format
int	stridx(), ctoi(), itoc()

begin
	# Convert to pseudo-Fortran print format, which may not be a valid
	# Fortran format.
	call strlwr(sppfmt)     # To take care of %12.5H for example.
	call tbbptf (sppfmt, p_ftnfmt)
	if (p_ftnfmt[2] == '-') {			# get rid of it
		do ip = 3, SZ_COLFMT
		    p_ftnfmt[ip-1] = p_ftnfmt[ip]
	    p_ftnfmt[SZ_COLFMT] = EOS
	}

	# We may not need this stuff; see below.
	dot = '.'
	dot_loc = stridx (dot, p_ftnfmt)
	ip = 2
	if (ctoi (p_ftnfmt, ip, w_num) > 0) {		# field width
		if (dot_loc > 0) {
		    ip = dot_loc + 1
		    if (ctoi (p_ftnfmt, ip, d_num) <= 0) # number of decimals
			d_num = 0
		} else {
		    d_num = 0
		}
	} else {
	    w_num = 0
	}
	if ((w_num < 0) || (d_num < 0))
	    call error (1, "spp2fort:  invalid format")
	if (w_num == 0)
	    w_num = 6

	# There are only a few formats that need fixing.
	ip = 2
	if (p_ftnfmt[1] == 'H') {			# hours:min:sec format
	    ftnfmt[1] = 'F'
	    d_num = d_num + 4
	    w_num = max (w_num, d_num+3)
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    ftnfmt[ip+nchar] = dot
	    ip = ip + nchar + 1
	    nchar = itoc (d_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	} else if (p_ftnfmt[1] == 'M') 	{		# min:sec format
	    ftnfmt[1] = 'F'
	    d_num = d_num + 2
	    w_num = max (w_num, d_num+3)
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    ftnfmt[ip+nchar] = dot
	    ip = ip + nchar + 1
	    nchar = itoc (d_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    #	} else if (p_ftnfmt[1] == 'Z') {	# hexadecimal
	    #	    ftnfmt[1] = 'I'
	    #	    w_num = w_num * LOG10_16 + 1	# need more room
	    #	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	} else {
	    # No change except possibly removing a '-' sign.
	    call strcpy (p_ftnfmt, ftnfmt, SZ_COLFMT)
	}
end

# WFT_BLANK_CARD -- Procedure to encode the FITS blank card.

procedure wft_blank_card (card)

char	card[ARB]	# FITS card image

begin
	call amovkc (" ", card, LEN_CARD)
	card[LEN_CARD+1] = EOS
end

# WFT_LAST_CARD -- Procedure to encode the FITS end card.

int procedure wft_last_card (card)

char	card[ARB]	# FITS card image

begin
	call sprintf (card, LEN_CARD, "%-8.8s  %70w")
	call pargstr ("END")

	return (YES)
end
