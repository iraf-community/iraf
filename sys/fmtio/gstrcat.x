# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GSTRCAT -- String concatenation.  String STR is appended to OUTSTR.

int procedure gstrcat (str, outstr, maxch)

char	str[ARB], outstr[ARB]
int	maxch

int	ip, op, n

begin
	do op = 0, maxch-1
	    if (outstr[op+1] == EOS)
		break

	n = maxch - op
	do ip = 1, n {
	    outstr[op+ip] = str[ip]
	    if (str[ip] == EOS)
		return (op + ip-1)
	}

	outstr[maxch+1] = EOS
	return (maxch)
end
