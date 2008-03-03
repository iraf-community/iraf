# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>

define	OCTAL		8
define	DECIMAL		10
define	HEX		16
define	MAX_RADIX	'Z' - 'A' + 11

# GLTOC -- Convert long integer to any radix string.  Returns the
# number of characters generated.

int procedure gltoc (a_lval, outstr, maxch, base)

long	a_lval			# long integer to be encoded
char	outstr[maxch]		# output buffer
int	maxch, base		# numeric base (2..16)

long	lval, carry, d, n, radix
int	op, size, nchars
int	gstrcpy(), absi()
long	andl(), orl(), absl(), modl()
bool	unsigned

begin
	if (IS_INDEFL(a_lval) && base > 0)
	    return (gstrcpy ("INDEF", outstr, maxch))
	size = maxch

	# Digit string is generated backwards, then reversed.  Unsigned
	# conversion used if radix negative.

	radix = max(2, min(MAX_RADIX, absi(base)))

	unsigned = (base < 0)			# get raw number
	if (unsigned) {
	    lval = MAX_LONG
	    n = andl (a_lval, lval) / 2
	    if (a_lval < 0) {
		lval = (MAX_LONG / 2 + 1)
		n = orl (n, lval)
	    }
	    lval = 1
	    carry = andl (a_lval, lval)		# get initial carry
	} else
	    n = a_lval

	op = 0
	repeat {
	    d = absl (modl (n, radix))		# generate next digit
	    if (unsigned) {
		d = 2 * d + carry		# get actual digit value
		if (d >= radix) {		# check for generated carry
		    d = d - radix
		    carry = 1
	        } else
		    carry = 0
	    }
	    op = op + 1
	    if (d < 10)				# convert to char and store
		outstr[op] = TO_DIGIT (d)
	    else
		outstr[op] = d - 10 + 'A'
	    n = n / radix
	} until (n == 0 || op >= size)

	if (unsigned) {
	    if (carry != 0 && op < size) {	# check for final carry
		op = op + 1
		outstr[op] = '1'
	    }
	} else if (a_lval < 0 && op < size) {	# add sign if needed
	    op = op + 1
	    outstr[op] = '-'
	}
	nchars = op				# return length of string

	for (d=1;  d < op;  d=d+1) {		# reverse digits
	    carry = outstr[d]
	    outstr[d] = outstr[op]
	    outstr[op] = carry
	    op = op - 1
	}

	outstr[nchars+1] = EOS
	return (nchars)
end
