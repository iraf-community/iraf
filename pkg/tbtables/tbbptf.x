include <tbset.h>

# tbbptf -- convert format to Fortran format
# This procedure converts an SPP-stype format for display to a Fortran
# format.  The input is case sensitive, but the output will
# be in upper case.  The input and output may be the same string.
# The output may not be a legal Fortran format, but it will be valid as
# input to utcdef.
#
# Bug:  Note that "H" and "h" are both converted to "H", and "M" and "m"
# are both converted to "M".  That is, the option to divide by 15 is lost.
#
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
#	a17      %17s      character string
#	h12.2    %12.2h    hh:mm:ss.dd
#	m12.2    %12.2m    mm:ss.dd
#	z12      %12x      hexadecimal integer
#
# Phil Hodge,  7-Aug-1987  Subroutine created.
# B. Simon,   10-Nov-1987  Rewritten.
# Phil Hodge, 18-Jan-1995  Add "H" and "M" for SPP.

procedure tbbptf (sppfmt, ftnfmt)

char	sppfmt[ARB]		# i: Print format in SPP style
char	ftnfmt[ARB]		# o: The corresponding Fortran format
#--
char	numpart[SZ_COLFMT]	# Copy of numerical portion of print format
int	fmtlen			# Length of string sppfmt
int	numlen			# Number of digits in numerical portion
int	index			# Position of character in format string

string	sppchr	"fgdeHhMmbsx"
string	ftnchr	"FGIEHHMMLAZ"

int	strlen(), stridx()

begin
	fmtlen = strlen (sppfmt)
	numlen = fmtlen - 2				# may be zero
	call strcpy (sppfmt[2], numpart, numlen)	# copy numerical portion

	# Get fortran type character corresponding to spp type character
	index = stridx (sppfmt[fmtlen], sppchr)
	if (index == 0) {
	    call strcpy (" ", ftnfmt, SZ_COLFMT)
	    return
	} else {
	    ftnfmt[1] = ftnchr[index]
	    ftnfmt[2] = EOS
	}

	# Append numerical portion of format
	if (numpart[1] == '0' && sppfmt[fmtlen] == 'd') {
	    call strcat (numpart[2], ftnfmt, SZ_COLFMT)
	    call strcat (".", ftnfmt, SZ_COLFMT)
	    call strcat (numpart[2], ftnfmt, SZ_COLFMT)	# e.g. I4.4
	} else {
	    call strcat (numpart, ftnfmt, SZ_COLFMT) # e.g. A10
	}
	
end
