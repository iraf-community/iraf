# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <time.h>
include "fxf.h"

# FXFENCODE.X -- Routines to encode a keyword, its value and a comment into
# a FITS card.
#
#	fxf_encode_axis (root, keyword, axisno)
#	fxf_encode_date (ctime, datestr, szdate, format, cutover)
#
#	    fxf_encodeb (keyword, param, card, comment)
#	    fxf_encodei (keyword, param, card, comment)
#	    fxf_encodel (keyword, param, card, comment)
#	    fxf_encoder (keyword, param, card, comment, precision)
#	    fxf_encoded (keyword, param, card, comment, precision)
#	    fxf_encodec (keyword, param, maxch, card, comment)
#
#	       fxf_akwc (keyword, value, len, comment, pn)
#	       fxf_akwb (keyword, value, comment, pn)
#	       fxf_akwi (keyword, value, comment, pn)
#	       fxf_akwr (keyword, value, comment, precision, pn)
#	       fxf_akwd (keyword, value, comment, precision, pn)
#
# Encode_axis adds an axis number to a keyword ("rootXXX").  Encode_date
# encodes the current date as a string in the form "dd/mm/yy".


# FXF_ENCODEB -- Encode a boolean parameter into a FITS card.

procedure fxf_encodeb (keyword, param, card, comment)

char	keyword[ARB]	# FITS keyword
int	param		# integer parameter equal to YES/NO
char	card[ARB]	# FITS card image
char	comment[ARB]	# FITS comment string

char	truth

begin
	if (param == YES)
	    truth = 'T'
	else
	    truth = 'F'

	call sprintf (card, LEN_CARD, "%-8.8s= %20c / %-47.47s")
	    call pargstr (keyword)
	    call pargc (truth)
	    call pargstr (comment)
end


# FXF_ENCODEI -- Encode an integer parameter into a FITS card.

procedure fxf_encodei (keyword, param, card, comment)

char	keyword[ARB]	# FITS keyword
int	param		# integer parameter
char	card[ARB]	# FITS card image
char	comment[ARB]	# FITS comment string

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20d / %-47.47s")
	    call pargstr (keyword)
	    call pargi (param)
	    call pargstr (comment)
end


# FXF_ENCODEL -- Encode a long parameter into a FITS card.

procedure fxf_encodel (keyword, param, card, comment)

char	keyword[ARB]		# FITS keyword
long	param			# long integer parameter
char	card[ARB]		# FITS card image
char	comment[ARB]		# FITS comment string

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20d / %-47.47s")
	    call pargstr (keyword)
	    call pargl (param)
	    call pargstr (comment)
end


# FXF_ENCODER -- Encode a real parameter into a FITS card.

procedure fxf_encoder (keyword, param, card, comment, precision)

char	keyword[ARB]		# FITS keyword
real	param			# real parameter
char	card[ARB]		# FITS card image
char	comment[ARB]		# FITS comment card
int	precision		# precision of real

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20.*e / %-47.47s")
	    call pargstr (keyword)
	    call pargi (precision)
	    call pargr (param)
	    call pargstr (comment)
end


# FXF_ENCODED -- Encode a double parameter into a FITS card.

procedure fxf_encoded (keyword, param, card, comment, precision)

char	keyword[ARB]		# FITS keyword
double	param			# double parameter
char	card[ARB]		# FITS card image
char	comment[ARB]		# FITS comment string
int	precision		# FITS precision

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20.*e / %-47.47s")
	    call pargstr (keyword)
	    call pargi (precision)
	    call pargd (param)
	    call pargstr (comment)
end


# FXF_ENCODE_AXIS -- Add the axis number to axis dependent keywords.

procedure fxf_encode_axis (root, keyword, axisno)

char	root[ARB]		# FITS root keyword
char	keyword[ARB]		# FITS keyword
int	axisno			# FITS axis number

int	len, strlen()

begin
	call strcpy (root, keyword, SZ_KEYWORD)
	len = strlen (keyword)
	call sprintf (keyword, SZ_KEYWORD, "%*.*s%d")
	    call pargi (-len)
	    call pargi (len)
	    call pargstr (root)
	    call pargi (axisno)
end


# FXF_ENCODEC -- Procedure to encode an IRAF string parameter into a FITS card.

procedure fxf_encodec (keyword, param, maxch, card, comment)

char	keyword[LEN_CARD]	# FITS keyword
char	param[LEN_CARD]		# FITS string parameter
int	maxch			# maximum chars in value string
char	card[LEN_CARD+1]	# FITS card image
char	comment[LEN_CARD]	# comment string

int	nblanks, maxchar, slashp

begin
	maxchar = max(8, min (maxch, LEN_OBJECT))
	slashp = 32 
	nblanks = LEN_CARD - (slashp + 1)
	if (maxchar >= 19) {
	   slashp = 1
	   nblanks = max (LEN_OBJECT - maxchar - slashp+3, 1)
	}

	call sprintf (card, LEN_CARD, "%-8.8s= '%*.*s' %*t/ %*.*s")
	    call pargstr (keyword)
	    call pargi (-maxchar)
	    call pargi (maxchar)
	    call pargstr (param)
	    call pargi (slashp)
	    call pargi (-nblanks)
	    call pargi (nblanks)
	    call pargstr (comment)
end


# FXF_ENCODE_DATE -- Encode the current date as a string value.
#
# New Y2K format:	yyyy-mm-ddThh:mm:sec
# Old FITS format:	dd/mm/yy
# Old TLM format:	hh:mm:ss (dd/mm/yyyy)
#
# We still write the old format for dates 1999 or less.

procedure fxf_encode_date (ctime, datestr, maxch, format, cutover)

long	ctime			#I time value to be encoded
char	datestr[ARB]		#O string containing the date
int	maxch			#I number of chars in the date string
char	format[ARB]		#I desired date format for old dates
int	cutover			#I write new format for years >= cutover

int	tm[LEN_TMSTRUCT], nchars
int	dtm_encode_hms()
long	lsttogmt()
bool	streq()

begin
	# Find out what year it is.
	call brktime (ctime, tm)

	# Encode in ISO format for years >= cutover year.

	if (TM_YEAR(tm) >= cutover) {
	    # ISO format is used for all new date stamps.
	    call brktime (lsttogmt(ctime), tm)
	    nchars = dtm_encode_hms (datestr, maxch,
		TM_YEAR(tm), TM_MONTH(tm), TM_MDAY(tm),
		TM_HOUR(tm), TM_MIN(tm), double(TM_SEC(tm)), 0, 0)

	} else if (streq (format, "TLM")) {
	    # TLM format is for old-format DATE-TLM keywords.
	    call sprintf (datestr, maxch, "%02d:%02d:%02d (%02d/%02d/%d)")
		call pargi (TM_HOUR(tm))
		call pargi (TM_MIN(tm))
		call pargi (TM_SEC(tm))
		call pargi (TM_MDAY(tm))
		call pargi (TM_MONTH(tm))
		call pargi (TM_YEAR(tm))

	} else {
	    # The default otherwise is the old FITS format.
	    call sprintf (datestr, maxch, "%02d/%02d/%02d")
		call pargi (TM_MDAY(tm))
		call pargi (TM_MONTH(tm))
		call pargi (mod(TM_YEAR(tm),100))

	}
end


# FXF_AKWC -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure fxf_akwc (keyword, value, len, comment, pn)

char	keyword[SZ_KEYWORD]	# keyword name
char	value[ARB]		# keyword value
int	len			# length of value
char	comment[ARB]		# comment
pointer pn			# pointer to a char area
char	card[LEN_CARD]

begin
	call fxf_encodec (keyword, value, len, card, comment)
	call amovc (card, Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD
end


# FXF_AKWB -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure fxf_akwb (keyword, value, comment, pn)

char	keyword[SZ_KEYWORD]	# I keyword name
int	value			# I Keyword value (YES, NO)
char	comment[ARB]		# I Comment
pointer pn			# I/O Pointer to a char area

pointer sp, pc

begin
	call smark (sp)
	call salloc (pc, LEN_CARD, TY_CHAR)

	call fxf_encodeb (keyword, value, Memc[pc], comment)
	call amovc (Memc[pc], Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD

	call sfree (sp)
end


# FXF_AKWI -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure fxf_akwi (keyword, value, comment, pn)

char	keyword[SZ_KEYWORD]	# I keyword name
int	value			# I Keyword value 
char	comment[ARB]		# I Comment
pointer pn			# I/O Pointer to a char area

pointer sp, pc

begin
	call smark (sp)
	call salloc (pc, LEN_CARD, TY_CHAR)

	call fxf_encodei (keyword, value, Memc[pc], comment)
	call amovc (Memc[pc], Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD

	call sfree (sp)
end


# FXF_AKWR -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure fxf_akwr (keyword, value, comment, precision, pn)

char	keyword[SZ_KEYWORD]	# I keyword name
real	value			# I Keyword value 
char	comment[ARB]		# I Comment
int	precision
pointer pn			# I/O Pointer to a char area

pointer sp, pc

begin
	call smark (sp)
	call salloc (pc, LEN_CARD, TY_CHAR)

	call fxf_encoder (keyword, value, Memc[pc], comment, precision)
	call amovc (Memc[pc], Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD

	call sfree (sp)
end


# FXF_AKWD -- Encode keyword, value and comment into a FITS card and
# append it to a buffer pointed by pn.
 
procedure fxf_akwd (keyword, value, comment, precision, pn)

char	keyword[SZ_KEYWORD]	# I keyword name
double	value			# I Keyword value 
char	comment[ARB]		# I Comment
int	precision
pointer pn			# I/O Pointer to a char area

pointer sp, pc

begin
	call smark (sp)
	call salloc (pc, LEN_CARD, TY_CHAR)

	call fxf_encoded (keyword, value, Memc[pc], comment, precision)
	call amovc (Memc[pc], Memc[pn], LEN_CARD)
	pn = pn + LEN_CARD

	call sfree (sp)
end
