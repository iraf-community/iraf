# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<printf.h>

# PARGX -- Pass a numeric argument of type complex to printf.  Get the format
# spec and format the number on the output file.  Try to provide reasonable
# automatic type conversions.  Avoid any type coercion of indefinites.

procedure pargx (xval)

complex	xval			# complex value to be encoded
double	value
int	n, xtoc()
include "fmt.com"

begin
	call fmt_read()					# read format

	if (format_char == FMT_COMPLEX || format_char == USE_DEFAULT) {
	    if (width == USE_DEFAULT)			# print as (r,r)
		width = SZ_OBUF

	    if (decpl == USE_DEFAULT || decpl == 0)
		decpl = NDIGITS_RP
	    else
		decpl = abs (decpl)

	    # Encode number in the available field width, decreasing the
	    # precision until the number fits.

	    repeat {
		n = xtoc (xval, obuf, SZ_OBUF, decpl, FMT_EXPON, SZ_OBUF)
		decpl = decpl - 1
	    } until (n <= width || decpl <= 0)

	    # Move the string in OBUF to the output file, left or right
	    # justifying as specified.  Advance to the next format spec
	    # (or finish up).

	    if (width == SZ_OBUF)				# free format?
		width = 0
	    call fmtstr (fd, obuf, col, fill_char, left_justify, SZ_OBUF, width)
	    call fpradv ()

	} else {
	    # Print real part of complex number in some format other than
	    # complex.

	    value = real (xval)
	    if (IS_INDEFR (real(xval)))
		value = INDEFD

	    call pargg (value, TY_REAL)
	}
end
