# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_ENCODE -- Encode a range structure into a string, return the
# number of characters that were written or ERR for string overflow.

int procedure rg_encode (rg, outstr, maxch)

pointer	rg				# First set of ranges
char	outstr[maxch]			# String to receive the ranges
int	maxch				# Maximum length of the string

char	tmpstr[SZ_LINE]
int	i, outlen

int	strlen()

begin
	if (rg == NULL)
	    call error (0, "Range descriptor undefined")

	outlen = 0
	outstr[1] = EOS

	do i = 1, RG_NRGS(rg) {
	    if (RG_X1(rg, i) != RG_X2(rg, i)) {
		call sprintf (tmpstr, maxch, "%d:%d,")
		    call pargi (RG_X1(rg, i))
		    call pargi (RG_X2(rg, i))
	    } else {
		call sprintf (tmpstr, maxch, "%d,")
		    call pargi (RG_X1(rg, i))
	    }

	    outlen = outlen + strlen (tmpstr)

	    if (outlen <= maxch)
		call strcat (tmpstr, outstr, maxch)
	    else {
		outstr[1] = EOS
		return (ERR)
	    }
	}

	# remove the last comma

	outstr[outlen] = EOS
	outlen = outlen - 1

	return (outlen)
end
