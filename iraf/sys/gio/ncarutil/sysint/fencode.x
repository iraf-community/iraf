# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<error.h>
include	<ctype.h>

define	SZ_FORMAT	11

# FENCD -- Format a real variable and return as a spp character string.
# A packed format string is passed as an input argument to define how the
# number is to be encoded.  The format of the format string is:
#	format string = "(cW.D)"
# where c is one of [EFGI], and where W and D are the field width and
# number of decimal places or precision, respectively.

procedure fencd (nchars, f_format, spp_outstr, rval)

int	nchars			# desired number of output chars
char	f_format[SZ_FORMAT] 	# SPP string containing format
char	spp_outstr[nchars+1]    # SPP string containing encoded number
real	rval			# value to be encoded

char	fmtchar, outstr[MAX_DIGITS], spp_format[SZ_FORMAT+1]
int	ip, op, stridxs()
real	x

begin
	# Encode format string for SPRINTF, format "%w.d".  Start copying
	# Fortran format at char 3, which should follow the EFGI char.

	spp_format[1] = '%'
	op = 2

	if (f_format[1] != '(')
	    call fatal (1, "Missing lparen in Ncar ENCODE format")
	for (ip=3;  f_format[ip] != ')' && f_format[ip] != EOS;  ip=ip+1) {
	    spp_format[op] = f_format[ip]
	    op = op + 1
	}

	# Now add the SPP format character.  EFG are the same for sprintf as
	# as for Fortran.  The integer format is 'd' for decimal in SPP.

	fmtchar = f_format[2]
	if (IS_UPPER(fmtchar))
	    fmtchar = TO_LOWER (fmtchar)
	    
	switch (fmtchar) {
	case 'e', 'f', 'g':
	    spp_format[op] = fmtchar
	case 'i':
	    spp_format[op] = 'd'
	default:
	    call fatal (1, "Unknown Ncar ENCODE format code")
	}
	op = op + 1
	spp_format[op] = EOS
	x = rval
	if (rval > 0)
	    x = -x

	# Now encode the user supplied variable and return it as a spp
	# string.

	iferr {
	    call sprintf (outstr, MAX_DIGITS, spp_format)
		call pargr (x)
	} then
	    call erract (EA_FATAL)

	# Let's try adding a "+" prefix to positive numbers to set if that
	# makes nicer plots.  Sep86 - This was not a good idea - changed to
	# a blank.

	op = stridxs ("-", outstr)
	if (rval > 0 && op > 0) 
	    outstr[op] = ' '

	call strcpy (outstr, spp_outstr, SZ_LINE)
end
