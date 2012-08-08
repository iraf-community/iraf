# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>
include	<lexnum.h>

define	OCTAL		8
define	DECIMAL		10
define	HEX		16


# GCTOD -- General character string to double precision real.  Any legal
# number, e.g., integer, floating point, complex, or character constant,
# is decoded and returned as a double.

int procedure gctod (str, ip, odval)

char	str[ARB]		# input string
int	ip			# pointer into input string
double	odval			# output double

char	ch
double	dval
complex	xval
long	lval
int	ip_save, radix, nchars, vtype
int	ctox(), cctoc(), ctod(), gctol(), lexnum()

begin
	vtype = TY_DOUBLE				# val to be returned
	while (IS_WHITE (str[ip]))
	    ip = ip + 1

	ip_save = ip
	ch = str[ip]					# first nonwhite

	if (ch == '(') {				# complex number?
	    if (ctox (str, ip, xval) <= 0)
		return (0)				# not a number
	    else
		vtype = TY_COMPLEX

	} else if (ch == SQUOTE || ch == ESCAPE) {
	    if (cctoc (str, ip, ch) <= 0)		# character constant?
		return (0)
	    else
		dval = ch

	} else {				# determine type of number
	    switch (lexnum (str, ip, nchars)) {
	    case LEX_OCTAL:
		radix = OCTAL
	    case LEX_DECIMAL:
		radix = DECIMAL
	    case LEX_HEX:
		radix = HEX
	    case LEX_REAL:
		radix = TY_REAL
	    default:
		return (0)
	    }

	    if (radix == TY_REAL)		# perform the conversion
		nchars = ctod (str, ip, dval)
	    else {
		nchars = gctol (str, ip, lval, radix)
		dval = lval
		if (IS_INDEFL (lval))
		    dval = INDEFD
	    }
	}

	if (vtype == TY_COMPLEX) {
	    odval = xval
	    if (IS_INDEFX (xval))
		odval = INDEFD
	} else
	    odval = dval

	return (ip - ip_save)
end
