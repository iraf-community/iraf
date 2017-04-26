# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SCANC -- Return the next character from the scanned input.

procedure scanc (cval)

char	cval
include	"scan.com"

begin
	cval = sc_scanbuf[sc_ip]
	if (cval != EOS)
	    sc_ip = sc_ip + 1
end
