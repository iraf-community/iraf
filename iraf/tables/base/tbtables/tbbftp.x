include <tbset.h>

# tbbftp -- convert format to SPP
# This procedure converts a Fortran-style format for display to an SPP
# format.  The input may be in upper or lower case.  If the input is
# not valid, the output will be a null string (which will result in the
# default print format being assigned if when tbcdef is called.)
# If the input begins with a "%" then it will simply be copied to output.
# The input is assumed to be a single letter followed by a number; the
# number may contain a decimal point in some cases.
# The following table shows examples of equivalences between Fortran and
# SPP formats:
#	ftnfmt   sppfmt       comments
#	f12.5    %12.5f    floating-point value
#	e12.5    %12.5e    floating-point value
#	d12.5    %12.5e    floating-point value
#	g12.5    %12.5g    general floating-point value
#	i12      %12d      integer
#	i12.12   %012d     integer padded with '0' on the left
#	l12      %12b      logical (Boolean)
#	a17      %-17s     character string, left justified
#	a-17     %-17s     character string, explicitly left justified
#	h12.2    %12.2h    hh:mm:ss.dd
#	m12.2    %12.2m    mm:ss.dd
#	z12      %12x      hexadecimal integer
#
# Phil Hodge, 17-Jun-1987  Subroutine created.
# B. Simon,   10-Nov-1987  Rewritten.
# Phil Hodge, 29-Apr-1997  Left justify character strings and boolean.
# Phil Hodge, 19-Mar-2003  Check for '.' in Z format, and zero fill at
#			left if '.' is found.

procedure tbbftp (infmt, sppfmt)

char	infmt[ARB]		# i: print format in Fortran style
char	sppfmt[ARB]		# o: the corresponding SPP format
#--
char	ftnfmt[SZ_COLFMT]	# copy of print format in Fortran style
char	dot			# '.'
int	fmtlen			# length of string ftnfmt
int	index			# index of character in format string
int	i

string	ftnchr	"fgiedhmlaz"
string	sppchr	"fgdeehmbsx"

int	strlen(), stridx()

begin
	fmtlen = strlen (infmt)

	if (fmtlen < 1) {
	    sppfmt[1] = EOS			# empty string in, empty out
	    return
	} else if (infmt[1] == '%') {		# already in SPP style
	    call strcpy (infmt, sppfmt, SZ_COLFMT)
	    return
	}

	# Make a local copy of the input format.
	do i = 1, SZ_COLFMT
	    ftnfmt[i] = EOS
	call strcpy (infmt, ftnfmt, SZ_COLFMT)
	call strlwr (ftnfmt)

	# Set sppfmt to % followed by the numerical specification.
	dot = '.'
	index = stridx (dot, ftnfmt)
 	if ((ftnfmt[1] == 'i' || ftnfmt[1] == 'z') && index > 0) {
	    call strcpy ("%0", sppfmt, SZ_COLFMT)	# zero fill at left
	    call strcat (ftnfmt[2], sppfmt, index)
	} else {
	    call strcpy ("%", sppfmt, SZ_COLFMT)
	    if (ftnfmt[2] != '-') {
		if (ftnfmt[1] == 'a' || ftnfmt[1] == 'l')
		    call strcat ("-", sppfmt, SZ_COLFMT)	# left justify
	    }
	    call strcat (ftnfmt[2], sppfmt, SZ_COLFMT)
	}

	# Append spp type character corresponding to fortran type character
	index = stridx (ftnfmt[1], ftnchr)
	if (index == 0) {
	    sppfmt[1] = EOS
	} else {
	    fmtlen = strlen (sppfmt)
	    sppfmt[fmtlen+1] = sppchr[index]
	    sppfmt[fmtlen+2] = EOS
	}
end
