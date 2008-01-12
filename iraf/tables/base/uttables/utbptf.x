include <tbset.h>

# utbptf -- convert format to Fortran format
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

procedure utbptf (sppfmt, ftnfmt)

char	sppfmt[ARB]		# i: Print format in SPP style
char	ftnfmt[ARB]		# o: The corresponding Fortran format
#--
char	p_ftnfmt[SZ_COLFMT]	# pseudo-Fortran format (incl SPP extensions)
char	dot			# '.'
int	w_num, d_num		# field width and number of decimals (as in w.d)
int	nchar, ip		# for reading w and d using ctoi, and for itoc
int	dot_loc			# location of "." in format
int	stridx(), ctoi(), itoc()

begin
	# Convert to pseudo-Fortran print format, which may not be a valid
	# Fortran format.
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
		if (ctoi (p_ftnfmt, ip, d_num) <= 0)	# number of decimals
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
	} else if (p_ftnfmt[1] == 'Z') {		# hexadecimal
	    ftnfmt[1] = 'I'
	    w_num = w_num * LOG10_16 + 1		# need more room
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	} else {
	    # No change except possibly removing a '-' sign.
	    call strcpy (p_ftnfmt, ftnfmt, SZ_COLFMT)
	}
end
