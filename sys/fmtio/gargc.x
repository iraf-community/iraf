# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGC -- Interpret the next input token as a character constant.

procedure gargc (cval)

char	cval
int	cctoc()
include	"scan.com"

begin
	if (sc_stopscan)
	    return

	if (cctoc (sc_scanbuf, sc_ip, cval) > 0)
	    sc_ntokens = sc_ntokens + 1
	else
	    sc_stopscan = true
end
