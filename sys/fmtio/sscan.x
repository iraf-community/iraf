# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SSCAN -- Begin a scan from a string.  Only the first newline terminated
# line in the string buffer will be scanned.  If a string buffer containing
# more than a single line must be scanned, MEMOPEN and FSCAN may be used.

procedure sscan (str)

char	str[ARB]
int	ip, op
include	"scan.com"

begin
	op = 1
	for (ip=1;  str[ip] != EOS && str[ip] != '\n';  ip=ip+1) {
	    sc_scanbuf[op] = str[ip]
	    op = op + 1
	    if (op >= SZ_SCANBUF)
		break
	}

	sc_scanbuf[op] = EOS
	call reset_scan()				# initialize scan
end
