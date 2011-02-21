include <tbset.h>

# tbhkeq -- keywords equal?
# This procedure compares an SPP string (i.e. one terminated with an EOS)
# with the keyword at the beginning of a parameter record.  Such a keyword
# is padded on the right with blanks rather than being terminated with EOS.
# There must not be any embedded blanks in the keyword.

# Phil Hodge, 10-Jul-91  Remove unnecessary ELSE before last IF statement.

bool procedure tbhkeq (sppstr, parrec)

char	sppstr[SZ_KEYWORD]	# i: string terminated with EOS
char	parrec[SZ_PARREC]	# i: parameter record; keyword is blank padded
#--
int	k

begin
	do k = 1, SZ_KEYWORD {
	    if (sppstr[k] == EOS) {
		if (parrec[k] == ' ')
		    return (true)
		else
		    return (false)
	    }
	    if (sppstr[k] != parrec[k])
		return (false)
	}
	return (true)
end
