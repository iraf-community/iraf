# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGX -- Interpret the next input token as a complex number.

procedure gargx (xval)

complex	xval
int	gctox()
include	"scan.com"

begin
	if (sc_stopscan)
	    return

	if (gctox (sc_scanbuf, sc_ip, xval) > 0)
	    sc_ntokens = sc_ntokens + 1
	else
	    sc_stopscan = true
end
