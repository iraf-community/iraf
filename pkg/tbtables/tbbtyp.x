include "tblerr.h"

define	SZ_SH_STR	21	# local buffer size

# Convert a data type expressed as a character string to an integer.
# The input string may be in upper or lower case.  The recognized data
# types are as follows:
# "r" --> real; "d" --> double; "i" --> int; "s" --> short; "b" --> bool;
# "ch*n" --> char string of length n (output datatype is -n).
#
# Phil Hodge, 10-Aug-1987  Delete third argument (datalen); ch*n --> -n.
# Phil Hodge, 28-Dec-1987  Use fixed size for local buffer str.
# Phil Hodge, 30-Mar-1993  Include short datatype.

procedure tbbtyp (chdtype, datatype)

char	chdtype[ARB]		# i: data type expressed as a string
int	datatype		# o: data type expressed as an int
#--
char	str[SZ_SH_STR]		# scratch space for copy of chdtype
char	asterisk		# ASCII equivalent of '*'
int	dlen			# number in char type, e.g. 12 in ch*12
int	ip, nchar		# for reading number from string, e.g. "ch*12"
int	stridx(), strncmp(), ctoi()

begin
	call strcpy (chdtype, str, SZ_SH_STR)
	call strlwr (str)

	if (str[1] == 'r') {
	    datatype = TY_REAL
	} else if (str[1] == 'd') {
	    datatype = TY_DOUBLE
	} else if (str[1] == 'i') {
	    datatype = TY_INT
	} else if (str[1] == 's') {
	    datatype = TY_SHORT
	} else if (str[1] == 'b') {
	    datatype = TY_BOOL
	} else if ((strncmp (str, "ch", 2) == 0) ||
		   (strncmp (str, "c*", 2) == 0)) {
	    asterisk = '*'
	    ip = stridx (asterisk, str) + 1	# go past the '*'
	    nchar = ctoi (str, ip, dlen)
	    if ((nchar < 1) || (dlen < 1)) {
		call error (ER_TBBADTYPE, "tbbtyp:  bad data type")
	    }
	    datatype = -dlen			# NOTE:  not an SPP data type
	} else {
	    call error (ER_TBBADTYPE, "tbbtyp:  bad data type")
	}
end
