# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGSTR -- Return the remainder of the scanned input line as a string.

procedure gargstr (outstr, maxch)

char	outstr[ARB]
int	maxch, op
include	"scan.com"

begin
	if (sc_stopscan)
	    return

	for (op=1;  op <= maxch && sc_scanbuf[sc_ip] != EOS;  op=op+1) {
	    if (sc_scanbuf[sc_ip] == '\n')
		break					# don't keep newlines
	    outstr[op] = sc_scanbuf[sc_ip]
	    sc_ip = sc_ip + 1
	}

	outstr[op] = EOS
	sc_ntokens = sc_ntokens + 1			# null strings are ok
end
