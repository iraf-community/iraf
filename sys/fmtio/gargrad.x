# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGRAD -- Convert the next number using the radix given as the second
# argument.

procedure gargrad (lval, radix)

long	lval
int	radix, gctol()
include	"scan.com"

begin
	if (sc_stopscan)
	    return

	if (gctol (sc_scanbuf, sc_ip, lval, radix) > 0)
	    sc_ntokens = sc_ntokens + 1
	else
	    sc_stopscan = true
end
