# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<printf.h>

# DTOC -- Format and output a floating point number, in any of the formats
# F,E,G,H, or M (H and M are hours-minutes-seconds and minutes-seconds formats,
# respectively).

int procedure dtoc (dval, outstr, maxch, decpl, a_fmt, width)

double	dval			# number to be output
char	outstr[ARB]		# output string
int	maxch			# size of the output string
int	decpl			# number of decimal places or precision
int	a_fmt, fmt		# format type (feghm)
int	width			# field width

bool	neg
double	val
int	op, round, h, m, s, f, v, i
int	dtoc3(), ltoc()

define	output {outstr[op]=$1;op=op+1;if(op>maxch)goto retry_}
define	retry_ 91

begin
	# If HMS format is not desired, simply call DTOC3.  Control also
	# returns to this point in the event of overflow.

	fmt = a_fmt
	if (IS_UPPER (fmt))
	    fmt = TO_LOWER (fmt)

	if (fmt == FMT_FIXED || fmt == FMT_EXPON || fmt == FMT_GENERAL ||
	    IS_INDEFD(dval)) {
retry_	
	    return (dtoc3 (dval, outstr, maxch, decpl, fmt, width))
	}

	# HMS format is implemented using calls to DTOC3, LTOC.  Use zero
	# fill to get two chars for the second and third fields, if necessary.
	# The second field is omitted for "m" format.  No whitespace is
	# permitted in an HMS (or other) number.  If the format is %H or %M
	# (instead of the usual %h or %m) scale the number by 15 before output
	# (converting degrees to hours).

	if (IS_UPPER (a_fmt))
	    val = dval / 15.0
	else
	    val = dval

	# Working with a positive number simplifies things.
	neg = (val < 0.0)
	if (neg)
	    val = -val

	# Decompose number into HMS or MS.
	h = 0
	if (fmt == FMT_HMS) {
	    h = int(val);  val = (val - h) * 60.0
	}
	m = int(val);  val = (val - m) * 60.0
	s = int(val);  val = (val - s)

	# Round the fractional seconds field and carry if the rounded value
	# is greater than 60.  This has to be done explicitly due to the
	# "base 60" sexagesimal arithmetic.

	round = (10.0 ** decpl)
	f = int (val * round + 0.5)
	while (f >= round) {
	    f = f - round
	    s = s + 1
	}
	while (s >= 60) {
	    s = s - 60
	    m = m + 1
	}
	while (m >= 60) {
	    m = m - 60
	    h = h + 1
	}

	# Format the output string.
	op = 1
	if (neg)
	    output ('-')

	# Output the first field, which is the hours field for HMS format,
	# or the minutes field for MS format.

	if (fmt == FMT_HMS)
	    v = h
	else
	    v = h * 60 + m
	op = op + ltoc (v, outstr[op], maxch-op+1)
	output (':')

	# Output the minutes field in HMS format.
	if (fmt == FMT_HMS) {
	    output (TO_DIGIT (m / 10))
	    output (TO_DIGIT (mod (m, 10)))
	    output (':')
	}

	# Output the seconds field.
	output (TO_DIGIT (s / 10))
	output (TO_DIGIT (mod (s, 10)))

	# Output the fraction, if any.
	if (decpl > 0) {
	    output ('.')
	    do i = 1, decpl {
		round = round / 10
		output (TO_DIGIT (f / round))
		f = mod (f, round)
	    }
	}

	# If the HMS format does not fit, go try a more compact format.
	if (op-1 > abs(width) || op > maxch) {
	    fmt = FMT_GENERAL
	    goto retry_
	}

	outstr[op] = EOS
	return (op-1)
end
