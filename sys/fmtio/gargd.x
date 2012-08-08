# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGD -- Interpret the next input token as a double precision floating
# number.

procedure gargd (dval)

double	dval
int	gctod()
include	"scan.com"

begin
	if (sc_stopscan)
	    return

	if (gctod (sc_scanbuf, sc_ip, dval) > 0)
	    sc_ntokens = sc_ntokens + 1
	else
	    sc_stopscan = true
end
