# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>

# XT_GIDS -- Get identifier tokens from a string and match with a dictionary.
#
# The input string is scanned for identifier tokens (see definition of
# identifier token in ctotok) and each token is checked against the
# dictionary string.  An array of YES/NO values for each dictionary entry,
# up to a maximum of maxids, is returned.

procedure xt_gids (str, dicstr, ids, maxids)

char	str[ARB]		# Input string
char	dicstr[ARB]		# Dictionary string
int	ids[maxids]		# Identifier indices in dictionary
int	maxids			# Maximum number of identifiers

int	i, ip, token
char	tokstr[SZ_LINE]

int	ctotok(), strdic()

begin
	call amovki (NO, ids, maxids)

	ip = 1
	repeat {
	    token = ctotok (str, ip, tokstr, SZ_LINE)
	    switch (token) {
	    case TOK_EOS:
		return
	    case TOK_IDENTIFIER:
		i = strdic (tokstr, tokstr, SZ_LINE, dicstr)
		if ((i > 0) && (i <= maxids))
		    ids[i] = YES
	    }
	}
end
