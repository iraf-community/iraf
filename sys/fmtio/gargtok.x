# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GARGTOK -- Return the next token from the scanned input line.

procedure gargtok (token, outstr, maxch)

int	token
char	outstr[ARB]
int	maxch, ctotok()
include	"scan.com"

begin
	if (sc_stopscan)
	    return

	sc_ntokens = sc_ntokens + 1		# Newline, EOS are legal tokens
	token = ctotok (sc_scanbuf, sc_ip, outstr, maxch)
end
