# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<printf.h>

.help parg
.nf ___________________________________________________________________________
PARG[CSILRDX] -- Pass a numeric argument to printf.  Get the format spec and
format the number on the output file.  Try to provide reasonable automatic
type conversions.  Avoid any type coercion of indefinites.
We try to make the operand fit in the specified field width, decreasing the
precision if necessary, but if it cannot be made to fit we increase the field
width until it does.  We feel that it is more important to output a readable
number than to keep the output columns justified.
.endhelp ______________________________________________________________________


# PARGD -- Pass a double.

procedure pargd (dval)

double	dval

begin
	call pargg (dval, TY_DOUBLE)
end


# PARGC -- Pass a char.

procedure pargc (cval)

char	cval
double	dval

begin
	dval = cval
	call pargg (dval, TY_CHAR)
end


# PARGS -- Pass a short.

procedure pargs (sval)

short	sval
double	dval

begin
	dval = sval
	if (IS_INDEFS (sval))
	    dval = INDEFD

	call pargg (dval, TY_SHORT)
end


# PARGI -- Pass an int.

procedure pargi (ival)

int	ival
double	dval

begin
	dval = ival
	if (IS_INDEFI (ival))
	    dval = INDEFD

	call pargg (dval, TY_INT)
end


# PARGL -- Pass a long.

procedure pargl (lval)

long	lval
double	dval

begin
	dval = lval
	if (IS_INDEFL (lval))
	    dval = INDEFD

	call pargg (dval, TY_LONG)
end


# PARGR -- Pass a real.

procedure pargr (rval)

real	rval
double	dval

begin
	dval = rval
	if (IS_INDEFR (rval))
	    dval = INDEFD

	call pargg (dval, TY_REAL)
end


# PARGG -- Generic put argument.  Encode a value of a specific datatype passed
# as a double precision value.

procedure pargg (value, dtype)

double	value
int	dtype

char	ch
long	lnum
complex	xnum
int	n, precision, i, junk, ival, nchars, nbits, fmt
int	ctocc(), gltoc(), dtoc(), xtoc(), fprfmt()
errchk	putci, fmtstr, fpradv
include "fmt.com"

begin
	# Read format.  If format spec contains "*" fields, VALUE is a part of
	# the format, rather than a true operand.  In that case we return,
	# and the next call again checks to see if the format spec is complete.
	# Note that if VALUE is not part of the format but is instead a floating
	# point value to be printed, it may have an exponent large enough to
	# cause integer overflow in an INT(VALUE) operation, hence we must
	# guard against this.  This is easy since only PARGI will be used to
	# pass format information.

	if (dtype == TY_REAL || dtype == TY_DOUBLE)
	    ival = 0
	else if (IS_INDEFD (value))
	    ival = INDEFI
	else
	    ival = nint (value)

	if (fprfmt (ival) == NOT_DONE_YET)
	    return

 	if (format_char == USE_DEFAULT || format_char == FMT_STRING)
	    switch (dtype) {
	    case TY_CHAR:
		format_char = FMT_CHAR
	    case TY_INT:
		format_char = FMT_DECIMAL
	    default:
		format_char = FMT_GENERAL
	    }

	if (dtype == TY_DOUBLE)				# supply def. precision
	    precision = NDIGITS_DP
	else
	    precision = NDIGITS_RP

	if (width == USE_DEFAULT)			# make as big as needed
	    width = SZ_OBUF

	# Convert number from binary into character form in OBUF, applying
	# defaults as needed.

	# Ignore case in testing format type.
	fmt = format_char
	if (IS_UPPER (fmt))
	    fmt = TO_LOWER(fmt)

	switch (fmt) {
	case FMT_BOOL:
	    if (IS_INDEFD (value))
		call strcpy ("INDEF", obuf, SZ_OBUF)
	    else if (int (value) == 0)
		call strcpy ("NO", obuf, SZ_OBUF)
	    else
		call strcpy ("YES", obuf, SZ_OBUF)

	case FMT_CHAR:
	    if (IS_INDEFD (value))
		call strcpy ("INDEF", obuf, SZ_OBUF)
	    else {
		ch = nint (value)
		junk = ctocc (ch, obuf, SZ_OBUF)
	    }

	case FMT_DECIMAL, FMT_OCTAL, FMT_HEX, FMT_RADIX, FMT_UNSIGNED:
	    switch (fmt) {
	    case FMT_DECIMAL:
		radix = DECIMAL				# signed decimal
	    case FMT_OCTAL:
		radix = -OCTAL				# unsigned octal
	    case FMT_HEX:
		radix = -HEX				# unsigned hex
	    case FMT_UNSIGNED:
		radix = -DECIMAL			# unsigned decimal
	    default:
		radix = -abs(radix)			# unsigned radix
	    }

	    if (IS_INDEFD (value)) {
		lnum = INDEFL
		nchars = gltoc (lnum, obuf, SZ_OBUF, radix)

	    } else {
		lnum = long (value)
		nchars = gltoc (lnum, obuf, SZ_OBUF, radix)

		# Limit sign extension if negative number, hex or octal.
		if (lnum < 0 && (dtype == TY_SHORT || dtype == TY_CHAR)) {
		    nbits = SZB_CHAR * NBITS_BYTE
		    if (dtype == TY_SHORT)
			nbits = nbits * SZ_SHORT
		    if (fmt == FMT_OCTAL) {
			n = nchars - (nbits + 2) / 3
			if (n > 0) {
			    call strcpy (obuf[n+2], obuf[2], SZ_OBUF)
			    obuf[1] = '1'
			}
		    } else if (fmt == FMT_HEX) {
			n = nchars - (nbits + 3) / 4
			if (n > 0)
			    call strcpy (obuf[n+1], obuf[1], SZ_OBUF)
		    }
		}
	    }

	case FMT_EXPON, FMT_FIXED, FMT_GENERAL, FMT_HMS, FMT_MINSEC:
	    if (decpl == USE_DEFAULT || decpl == 0)
		switch (fmt) {
		case FMT_EXPON, FMT_GENERAL:
		    decpl = precision
		case FMT_HMS, FMT_MINSEC:
		    if (decpl == USE_DEFAULT)
			decpl = 1
		default:
		    if (decpl == USE_DEFAULT)
			decpl = precision
		}
	    repeat {
		# Need the case sensitive format char here.
		n = dtoc (value, obuf, SZ_OBUF, decpl, format_char, width+1)
		decpl = decpl - 1
	    } until (n <= width || decpl <= 0)

	case FMT_TOCOLUMN:				# advance to column
	    for (i=int(value);  col < i;  col=col+1)
		call putci (fd, ' ')
	    call fpradv()
	    return

	case FMT_WHITESPACE:				# output whitespace
	    for (i=0;  i < int(value);  i=i+1)
		call putci (fd, ' ')
	    col = col + i
	    call fpradv()
	    return

	case FMT_COMPLEX:
	    if (decpl == USE_DEFAULT)			# set defaults
		decpl = precision
	    else
		decpl = abs (decpl)

	    if (IS_INDEFD (value))
		xnum = INDEFX
	    else
		xnum = complex (value)

	    # Convert, decrease precision until it fits in given field width.
	    repeat {
		n = xtoc (xnum, obuf, SZ_OBUF, decpl, 'e', SZ_OBUF)
		decpl = decpl - 1
	    } until (n <= width || decpl <= 0)
	}
	    
	# Move the string in OBUF to the output file, left or right justifying
	# as specified.  Advance to the next format spec (or finish up).

	if (width == SZ_OBUF)				# free format?
	    width = 0
	call fmtstr (fd, obuf, col, fill_char, left_justify, SZ_OBUF, width)
	call fpradv ()
end
