# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGWRD -- Return the next whitespace delimited token or quoted string from
# the scan buffer.

procedure gargwrd (outstr, maxch)

char	outstr[ARB]
int	maxch, ctowrd()
include	"scan.com"

begin
	if (sc_stopscan) {
	    outstr[1] = EOS
	    return
	}

	if (ctowrd (sc_scanbuf, sc_ip, outstr, maxch) > 0)
	    sc_ntokens = sc_ntokens + 1
	else
	    sc_stopscan = true
end
