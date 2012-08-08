# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# IDB_NAXIS -- Determine if the named keyword is one of the NAXIS* keywords,
# and if so return the value of the numeric suffix.

int procedure idb_naxis (keyw, axnum)

char	keyw[ARB]		# keyword name
int	axnum			# receives numeric axis code (0=no suffix)

int	ch, ip
int	strncmp(), ctoi()

begin
	if (strncmp (keyw, "i_naxis", 7) == 0)
	    ip = 8
	else if (strncmp (keyw, "naxis", 5) == 0)
	    ip = 6
	else
	    return (NO)

	ch = keyw[ip]
	if (!IS_DIGIT(ch) && ch != ' ' && ch != EOS)
	    return (NO)

	if (ctoi (keyw, ip, axnum) <= 0)
	    axnum = 0

	return (YES)
end
