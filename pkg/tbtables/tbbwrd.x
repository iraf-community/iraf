include <ctype.h>		# for IS_DIGIT

define	LEFT_MARGIN	0	# extra space at left of first column

# tbbwrd -- read a word from the input buffer
# This routine extracts the next word from the input buffer, returning
# the word itself and the number of characters that it contains.
# The function value is the number of char converted, as returned by
# ctowrd.  If the value is a string (i.e. not numeric), then trailing
# blanks will be truncated.  If the value is a single blank, however,
# that blank will not be deleted.
#
# If the word is numeric, PREC is the number of digits of precision
# in the word; for HH:MM:SS.d or HH:MM.d format PREC is the number of
# digits after the decimal point.  The idea is that the values of WIDTH
# and PREC returned by this routine can go directly into a format code.
#
# The format code FCODE and precision PREC are really only used if the
# data type is double.

# Phil Hodge,  3-Mar-1992  Subroutine created.
# Phil Hodge,  7-Aug-1992  Include checks on string beginning with number;
#			add fcode to the calling sequence.
# Phil Hodge, 10-Sep-1992  Set datatype to double if word is INDEF.
# Phil Hodge,  7-Jun-1994  Set type to char if more than one decimal point.
# Phil Hodge, 27-Jul-1994  Change LEFT_MARGIN from 3 to 0 (no longer needed).

int procedure tbbwrd (buf, ip, word, maxch, width, prec, datatype, fcode)

char	buf[ARB]		# i: buffer containing line from file
int	ip			# io: starting location in buffer
char	word[ARB]		# o: word extracted from buffer
int	maxch			# i: max size of word
int	width			# o: width of column
int	prec			# o: digits of precision in this word
int	datatype		# o: TY_DOUBLE, TY_INT, or TY_CHAR
int	fcode			# o: format code for print format
#--
char	cval			# one character in the word
int	ip_start		# ip before calling ctowrd
int	word_width		# width of extracted word (value of ctowrd)
int	i			# loop index
int	num_colon		# number of ':' found in word
bool	quote			# true if value begins with ' or "
bool	exponent		# true if there's an exponent in the word
bool	dec_point		# true if there's a decimal point in the word
int	ctowrd(), strlen(), strncmp()
bool	streq()

define	chartype_ 91
define	finished_ 93

begin
	ip_start = ip
	datatype = 0
	fcode = 's'

	word_width = ctowrd (buf, ip, word, maxch)
	if (word_width < 1)
	    return (0)

	# These may be updated later.
	width = word_width
	prec = width

	# Check whether column begins with a quote, indicating that it's
	# a string, even if the word itself is numeric, e.g. "3.14159".
	quote = false
	do i = ip_start, ip {
	    if (buf[i] != ' ') {
		if (buf[i] == '"' || buf[i] == '\'')
		    quote = true
		break
	    }
	}

	# Get a first estimate of the data type.  We may change this later.
	if (quote) {
	    datatype = TY_CHAR

	} else if (IS_DIGIT(word[1])) {
	    datatype = TY_DOUBLE

	} else if (word[1] == '-' || word[1] == '+' || word[1] == '.') {
	    if (IS_DIGIT(word[2]))
		datatype = TY_DOUBLE
	    else
		datatype = TY_CHAR

	} else if (word[1] == 'I') {
	    if (streq (word, "INDEFI")) {
		datatype = TY_INT
		width = 6
		fcode = 'd'
		goto finished_
	    } else if (strncmp (word, "INDEF", 5) == 0) {
		datatype = TY_DOUBLE
		width = 5
		prec = 5
		fcode = 'g'
		goto finished_
	    } else {
		datatype = TY_CHAR
	    }

	} else {
	    datatype = TY_CHAR
	}

	if (quote) {
	    # The value is enclosed in quotes; don't include them in the width.
	    width = word_width - 2

	    # Trim trailing blanks.  width is unchanged.
	    do i = strlen (word), 1, -1 {
		if (word[i] != ' ') {
		    word[i+1] = EOS
		    break
		}
	    }
	}

	if (datatype != TY_CHAR) {
	    # So far, the word appears to be a number.  Check each character,
	    # and change the type if we find that it's not numeric.

	    num_colon = 0			# initial values
	    exponent = false
	    dec_point = false
	    prec = 0				# incremented in loop
	    do i = 1, maxch {
		cval = word[i]
		if (IS_DIGIT(cval)) {
		    if (!exponent)
			prec = prec + 1		# count it
		} else if (cval == '.') {
		    # There can't be two decimal points, or even one in
		    # an exponent.
		    if (dec_point || exponent) {
			datatype = TY_CHAR
			goto chartype_
		    }
		    dec_point = true
		    if (num_colon > 0)
			prec = 0
		} else if (cval == '+' || cval == '-') {
		    # A sign must be the first character or in an exponent.
		    if (i > 1 && !exponent) {
			datatype = TY_CHAR
			goto chartype_
		    }
		} else if (cval == ':') {
		    num_colon = num_colon + 1
		    if (exponent || num_colon > 2) {
			datatype = TY_CHAR
			goto chartype_
		    }
		} else if (cval == 'E' || cval == 'e' ||
			   cval == 'D' || cval == 'd') {
		    # There can't be more than one exponent in a number.
		    if (exponent) {
			datatype = TY_CHAR
			goto chartype_
		    } exponent = true		# it looks like an exponent
		} else if (cval == EOS) {
		    break
		} else {
		    datatype = TY_CHAR		# not numeric
		    goto chartype_
		}
	    }
	    prec = max (prec, 1)

	    # We need this test for HMS format because there might have been
	    # no decimal point in the value (e.g. 3:17:42), so the digits
	    # would have been counted in the precision and not reset to zero
	    # by a decimal point.  We could in principle have prec=0, but
	    # that prints incorrect values due to truncation.
	    if (num_colon > 0 && !dec_point)
		prec = 1			# should be zero for HH:MM:SS

	    # Now make sure the field width is sufficient, and set format code.
	    if (num_colon == 2) {		# HH:MM:SS.d
		width = prec + 10
		fcode = 'h'
	    } else if (num_colon == 1) {	# HH:MM.d
		width = prec + 7
		fcode = 'm'
	    } else if (exponent) {
		width = prec + 6
		fcode = 'g'
	    } else if (dec_point) {
		width = prec + 2
		fcode = 'g'
	    } else {			# no decimal point, colon, or exponent
		width = prec + 1
		datatype = TY_INT		# reset datatype to int
		fcode = 'd'
	    }

	}

chartype_
	if (datatype == TY_CHAR && !quote) {
	    # It's a string, but we don't need to check for trailing blanks
	    # because the string is not enclosed in quotes.
	    width = word_width
	}

finished_
	# If this is the first column and the value is left-justified,
	# add a little extra space.
	if (ip_start == 1) {
	    if (buf[1] != ' ' && buf[1] != '\t')
		width = width + LEFT_MARGIN
	}

	return (word_width)
end
