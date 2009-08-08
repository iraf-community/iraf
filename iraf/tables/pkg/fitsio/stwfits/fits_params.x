include <time.h>
include "wfits.h"

define	MAX_STRFIELD	(LEN_CARD-11)

# WFT_ENCODEB -- Procedure to encode a boolean parameter into a FITS card.

procedure wft_encodeb (keyword, param, card, comment)

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


# WFT_ENCODEI -- Procedure to encode an integer parameter into a FITS card.

procedure wft_encodei (keyword, param, card, comment)

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

# WFT_ENCODES -- Procedure to encode a short integer parameter into a FITS card.

procedure wft_encodes (keyword, param, card, comment)

char	keyword[ARB]	# FITS keyword
short	param		# short integer parameter
char	card[ARB]	# FITS card image
char	comment[ARB]	# FITS comment string

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20d / %-47.47s")
	call pargstr (keyword)
	call pargs (param)
	call pargstr (comment)
end


# WFT_ENCODEL -- Procedure to encode a long parameter into a FITS card.

procedure wft_encodel (keyword, param, card, comment)

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


# WFT_ENCODER -- Procedure to encode a real parameter into a FITS card.

procedure wft_encoder (keyword, param, card, comment, precision)

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


# WFT_ENCODED -- Procedure to encode a double parameter into a FITS card.

procedure wft_encoded (keyword, param, card, comment, precision)

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


# WFT_AXIS_ENCODE -- Procedure to add the axis number to axis dependent
# keywords.

procedure wft_axis_encode (root, keyword, axisno)

char	root[ARB]		# FITS root keyword
char	keyword[ARB]		# FITS keyword
int	axisno			# FITS axis number

begin
	call strcpy (root, keyword, LEN_KEYWORD)
	call sprintf (keyword, LEN_KEYWORD, "%-5.5s%-3.3s")
	call pargstr (root)
	call pargi (axisno)
end

# WFT_ENCODEC -- Procedure to encode an IRAF string parameter into a FITS card.

procedure wft_encodec (keyword, param, card, comment)

char	keyword[ARB]	# FITS keyword
char	param[ARB]	# FITS string parameter
char	card[ARB]	# FITS card image
char	comment[ARB]	# comment string

size_t	sz_val
pointer	sp, qparam

begin
	call smark (sp)
	sz_val = MAX_STRFIELD
	call salloc (qparam, sz_val, TY_CHAR)

	call wft_putquote (param, Memc[qparam])
	call wft_nqencode (keyword, Memc[qparam], card, comment)

	call sfree (sp)
end

# WFT_NQENCODE -- Procedure to encode an IRAF string parameter
#		 with NO quotes on the string into a FITS card.

procedure wft_nqencode (keyword, param, card, comment)

char	keyword[ARB]	# FITS keyword
char	param[ARB]	# FITS string parameter
char	card[ARB]	# FITS card image
char	comment[ARB]	# comment string

int	maxchar, nblanks, slashp
int	strlen()

begin
	maxchar = min (strlen (param), MAX_STRFIELD)

	if (maxchar < 21) {
	    slashp = 32 
	    nblanks = LEN_CARD - (slashp + 1)
	} else {
	    slashp = 1
	    nblanks = LEN_CARD - (maxchar + 13)
	} 

	# Check to see if there is room for the comment

	if (nblanks <= 0){
	    call wft_cqencode (keyword, param, card)

	} else {
	    call sprintf (card, LEN_CARD, "%-8.8s= %*.*s %*t/ %*.*s")
	    call pargstr (keyword)
	    call pargi (-maxchar)
	    call pargi (maxchar)
	    call pargstr (param)
	    call pargi (slashp)
	    call pargi (-nblanks)
	    call pargi (nblanks)
	    call pargstr (comment)
	}
end

# WFT_NCENCODE -- Procedure to encode an IRAF string parameter into a FITS card
#                 NOTICE that it does not put a comment string.

procedure wft_ncencode (keyword, param, card)

char	keyword[ARB]	# FITS keyword
char	param[ARB]	# FITS string parameter
char	card[ARB]	# FITS card image

size_t	sz_val
pointer	sp, qparam

begin
	call smark (sp)
	sz_val = MAX_STRFIELD
	call salloc (qparam, sz_val, TY_CHAR)

	call wft_putquote (param, Memc[qparam])
	call wft_cqencode (keyword, Memc[qparam], card)

	call sfree (sp)
end

# WFT_CQENCODE -- Procedure to encode an IRAF string parameter into a FITS card
#                 with no comment and no quotes surrounding the string

procedure wft_cqencode (keyword, param, card)

char	keyword[ARB]	# FITS keyword
char	param[ARB]	# FITS string parameter
char	card[ARB]	# FITS card image

size_t	sz_val
pointer	sp, str

begin
	call smark (sp)
	sz_val = MAX_STRFIELD
	call salloc (str, sz_val, TY_CHAR)

	call strcpy (param, Memc[str], MAX_STRFIELD)

        call sprintf (card, LEN_CARD, "%-8.8s= %-70.70s")
	call pargstr (keyword)
	call pargstr (Memc[str])

	call sfree (sp)
end

# WFT_ENCODE_BLANK -- Procedure to encode the FITS blank parameter. Necessary
# because the 32 bit blank value equals INDEFI.

procedure wft_encode_blank (keyword, blank_str, card, comment)

char	keyword[ARB]		# FITS keyword
char	blank_str[ARB]		# string containing values of FITS blank integer
char	card[ARB]		# FITS card image
char	comment[ARB]		# FITS comment string

begin
	call sprintf (card, LEN_CARD, "%-8.8s= %20.20s / %-47.47s")
	call pargstr (keyword)
	call pargstr (blank_str)
	call pargstr (comment)
end


# WFT_ENCODE_DATE -- Procedure to encode the date in the form dd-mm-yy.

procedure wft_encode_date (datestr, szdate)

char	datestr[ARB]	# string containing the date
int	szdate		# number of chars in the date string

long	ctime, l_val
int	time[LEN_TMSTRUCT]
long	clktime()
int	imod()

begin
	l_val = 0
	ctime = clktime (l_val)
	call brktime (ctime, time)

	if (TM_YEAR(time) < 1999) {
	    call sprintf (datestr, szdate, "%02s/%02s/%02s")
	    call pargi (TM_MDAY(time))
	    call pargi (TM_MONTH(time))
	    call pargi (imod (TM_YEAR(time), CENTURY))
	} else {
	    call sprintf (datestr, szdate, "%04s-%02s-%02s")
	    call pargi (TM_YEAR(time))
	    call pargi (TM_MONTH(time))
	    call pargi (TM_MDAY(time))
	}
end


# WFT_FITS_CARD --  Procedure to fetch a single line from a string parameter
# padding it to a maximum of maxcols characters and trimmimg the delim
# character.

procedure wft_fits_card (instr, ip, card, col_out, maxcols, delim)

char	instr[ARB]	# input string
int	ip		# input string pointer, updated at each call
char	card[ARB]	# FITS card image
int	col_out		# pointer to column in card
int	maxcols		# maximum columns in card
int	delim		# 1 character string delimiter

int	op

begin
	op = col_out

	# Copy string
	while (op <= maxcols && instr[ip] != EOS && instr[ip] != delim) {
	    card[op] = instr[ip]
	    ip = ip + 1
	    op = op + 1
	}

	# Fill remainder of card with blanks
	while (op <= maxcols ) {
	    card[op] = ' '
	    op = op + 1
	}

	if (instr[ip] == delim)
	    ip = ip + 1

end


# WFT_GET_IRAF_TYPESTRING -- Procedure to set the iraf datatype keyword.
# Permitted strings are INTEGER, FLOATING or COMPLEX.
# Nelson Zarate: Sep-26-89. Allows for all types to be put in the IRAFTYPE
# keyword for later use by the reader (strfits). This will allows for
# automatic image type by the reader to match the one of the original
# file.

procedure wft_get_iraf_typestring (datatype, type_str)

int	datatype	# the IRAF data type
char	type_str[ARB]	# the output IRAF type string

include "wfits.com"

begin
	switch (datatype) {
	case TY_SHORT:
	    call strcpy ("SHORT", type_str, LEN_STRING) 
	case TY_USHORT:
	    call strcpy ("USHORT", type_str, LEN_STRING) 
	case TY_INT:
	    call strcpy ("INTEGER", type_str, LEN_STRING) 
	case TY_LONG:
	    if ( SZ_LONG == 2 ) {
		call strcpy ("INTEGER", type_str, LEN_STRING) 
	    } else {
		call strcpy ("LONG", type_str, LEN_STRING) 
	    }
	case TY_REAL:
	    call strcpy ("FLOATING", type_str, LEN_STRING)
	case TY_DOUBLE:
	    call strcpy ("DOUBLE", type_str, LEN_STRING)
	case TY_COMPLEX:
	    call strcpy ("COMPLEX", type_str, LEN_STRING)
	default:
	    call flush (STDOUT)
	    call error (3, "IRAF_TYPE: Unknown IRAF image type.")
	}
end




# WFT_TRIMSTR -- Procedure to trim trailing blanks from a string

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

# WFT_PUTQUOTE -- Enclose string within quote marks

procedure wft_putquote (str, qstr)

char	str[ARB]	# input string
char	qstr[ARB]	# output (quoted) string

int	ic, nc
int	strlen()

begin
	nc = min (strlen (str), MAX_STRFIELD-2)

	qstr[1] = '\''
	for (ic = 1; ic <= nc; ic = ic + 1)
	    qstr[ic+1] = str[ic]

	for ( ; ic <= 8; ic = ic + 1)
	    qstr[ic+1] = ' '

	qstr[ic+1] = '\''
	qstr[ic+2] = EOS

end
