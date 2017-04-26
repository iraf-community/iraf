# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# GARGB -- Interpret the next token in the input as a boolean quantity
# (token "y...." or "n....").

procedure gargb (bval)

bool	bval
include	"scan.com"

begin
	if (sc_stopscan)
	    return

	while (IS_WHITE (sc_scanbuf[sc_ip]))
	    sc_ip = sc_ip + 1

	switch (sc_scanbuf[sc_ip]) {
	case 'Y','y':
	    bval = true
	case 'N','n':
	    bval = false
	default:
	    sc_stopscan = true
	    return
	}

	while (IS_ALPHA(sc_scanbuf[sc_ip]) || sc_scanbuf[sc_ip] == '_')
	    sc_ip = sc_ip + 1
	sc_ntokens = sc_ntokens + 1
end
