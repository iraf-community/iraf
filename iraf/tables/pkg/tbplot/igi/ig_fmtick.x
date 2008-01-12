include <ctype.h>
include "igi.h"

procedure ig_fmtick(igs)

#  IG_FMTICK -- The igi FMTICK command to specify the format for axis
#  tick labels.  This format is a string containig a Fortran format
#  specification, translated to an SPP printf() format.  This format
#  overrides the default igi label formatting with superscript notation
#  for exponents and so forth.
#  2/8/91  ZGL

pointer	igs			# igi parameters structure descriptor
	
int	tokvals			# Token value structure
int     token
pointer	sp
pointer	fmt, sfmt
int	ip

int	gettok()

begin
        call lcmdcat (igs, YES)

	tokvals = TOKEN_VALUE(igs)

	call smark (sp)
	call salloc (fmt,  SZ_LINE, TY_CHAR)
	call salloc (sfmt, SZ_LINE, TY_CHAR)

	token = gettok (igs)

        if (IS_NEWCOMMAND(token))
	    # No argument;  reset the format to null That is,
	    # use the default igi formatting
	    Memc[fmt] = EOS

	else
	    # Interpret the argument as the format string
	    call strcpy (LOP_VALC(tokvals), Memc[fmt], SZ_LINE)

	for (ip = fmt;  IS_WHITE(Memc[ip]);  ip = ip + 1)
	    # Discard leading whitespace
	    ;

	# Fill in the igi parameter
	call ii_fmtick (igs, Memc[fmt])

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Axis tick format, input: %s, converted: %s ")
		call pargstr (Memc[fmt])
		call pargstr (Memc[sfmt])
	}

	call sfree (sp)
end


procedure ii_fmtick (igs, fmt)

pointer	igs			# igi parameters structure descriptor
char	fmt[ARB]		# Fortran format string

pointer igps			# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	# Convert the input Fortran format to SPP printf format
	call fmtcnv (fmt, MG_TICKFMT(igps), SZ_LINE)
end


# fmtcnv -- convert format to SPP
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
#	i12.     %012d     integer padded with '0' on the left
#	l12      %12b      logical (Boolean)
#	a17      %17a      character string, right justified
#	a-17     %-17a     character string, left justified
#	h12.2    %12.2h    hh:mm:ss.dd
#	m12.2    %12.2m    mm:ss.dd
#	z12      %12x      hexadecimal integer
#
# P.E. Hodge, 17-Jun-87  Subroutine created.
# B. Simon,   10-Nov-87  Rewritten.
# 2/8/91 Stolen for igi.  Changed the name and added the maxch argument.  ZGL

procedure fmtcnv (infmt, sppfmt, maxch)

char	infmt[ARB]		# i: print format in Fortran style
char	sppfmt[ARB]		# o: the corresponding SPP format
int	maxch			# i: maximum string size
#--
char	ftnfmt[SZ_LINE]		# copy of print format in Fortran style
char	dot			# '.'
int	fmtlen			# length of string ftnfmt
int	index			# index of character in format string

string	ftnchr	"fgiedhmlaz"
string	sppchr	"fgdeehmbsx"

int	strlen(), stridx()

begin
	fmtlen = strlen (infmt)

	if (fmtlen <= 1) {
	    sppfmt[1] = EOS			# empty string in, empty out
	    return
	} else if (infmt[1] == '%') {		# already in SPP style
	    call strcpy (infmt, sppfmt, maxch)
	    return
	}

	# Make a local copy of the input format.
	call strcpy (infmt, ftnfmt, maxch)
	call strlwr (ftnfmt)

	# Set sppfmt to % followed by the numerical specification.
	dot = '.'
	index = stridx (dot, ftnfmt)
 	if (ftnfmt[1] == 'i' && index > 0) {
	    call strcpy ("%0", sppfmt, maxch)	# zero fill at left
	    call strcat (ftnfmt[2], sppfmt, index)
	} else {
	    call strcpy ("%", sppfmt, maxch)
	    call strcat (ftnfmt[2], sppfmt, maxch)
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
