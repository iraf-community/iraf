include <mach.h>
include <ctype.h>	# for IS_WHITE, IS_LOWER, TO_UPPER
include <tbset.h>
include "tbtables.h"

define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)	# size of packed par record

# tbhrpr -- read parameter record
# This procedure reads a packed header parameter record, unpacks it, and
# returns the record containing keyword and value.
#
# Phil Hodge, 14-Feb-1992  Add option for text table type.
# Phil Hodge, 25-Apr-1994  Set str to "" for text table.
# Phil Hodge, 10-Jun-1999  Handle text tables.
# Phil Hodge, 10-May-2000  For text tables, check for history or comment
#			when determining the data type.

procedure tbhrpr (tp, parnum, str)

pointer tp			# i: pointer to table descriptor
int	parnum			# i: number of the parameter to be gotten
char	str[SZ_PARREC]		# o: string containing the keyword and value
#--
pointer sp
pointer par			# scratch for reading the keyword
pointer word			# value extracted from str
int	i, ip, op		# loop indexes
int	maxch			# length of keyword string
bool	done
int	datatype		# data type of parameter
int	width, prec, fcode	# returned by tbbwrd and ignored
int	tbbwrd()
int	stat
long	locn			# location for reading in file
int	ch			# a character in the string
int	read(), strlen()
bool	streq()
errchk	seek, read

begin
	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {

	    if (parnum < 1 || parnum > TB_NPAR(tp)) {
		str[1] = EOS
		return
	    }

	    maxch = max (strlen (Memc[TB_KEYWORD(tp,parnum)]), SZ_PARREC)
	    call smark (sp)
	    call salloc (par, maxch, TY_CHAR)

	    call strcpy (Memc[TB_KEYWORD(tp,parnum)], Memc[par], maxch)

	    # Copy out the keyword, converting to upper case.
	    ip = 3				# zero indexed
	    while (IS_WHITE(Memc[par+ip]))
		ip = ip + 1
	    op = 1
	    done = false
	    while (!done) {
		ch = Memc[par+ip]
		if (IS_LOWER(ch)) {
		    str[op] = TO_UPPER(ch)
		} else if (IS_WHITE(ch) || ch == '=' || ch == EOS) {
		    str[op] = ' '
		    done = true
		} else {
		    str[op] = ch
		}
		op = op + 1
		if (op > SZ_KEYWORD)
		    done = true
		if (!done)
		    ip = ip + 1
	    }
	    # We're done with op after the following, but we still need ip.
	    do i = op, SZ_KEYWORD
		str[i] = ' '			# pad keyword with blanks
	    str[SZ_KEYWORD+1] = EOS

	    # Have we truncated the keyword?
	    if (!IS_WHITE(ch) && ch != '=') {
		# Skip over the rest of the keyword in the input string.
		done = false
		while (!done) {
		    ch = Memc[par+ip]
		    if (IS_WHITE(ch) || ch == '=') {
			done = true
		    } else if (ch == EOS) {		# a blank value
			call strcat ("t", str, SZ_PARREC)
			call sfree (sp)
			return
		    } else {
			ip = ip + 1
		    }
		}
	    }

	    # Skip over any intervening whitespace, allowing for one '='.
	    while (IS_WHITE(Memc[par+ip]))
		ip = ip + 1
	    if (Memc[par+ip] == '=')
		ip = ip + 1
	    while (IS_WHITE(Memc[par+ip]))
		ip = ip + 1

	    # Now ip (zero indexed) is the beginning of the value.
	    # Determine the data type.
	    call salloc (word, maxch, TY_CHAR)
	    i = ip + 1				# one indexed
	    if (streq (str, "HISTORY ") || streq (str, "COMMENT ")) {
		datatype = TY_CHAR
	    } else if (tbbwrd (Memc[par], i, Memc[word], maxch,
			width, prec, datatype, fcode) < 1) {
		datatype = TY_CHAR
	    }

	    # Append the data type code and the value.
	    if (datatype == TY_DOUBLE)
		call strcat ("d", str, SZ_PARREC)
	    else if (datatype == TY_INT)
		call strcat ("i", str, SZ_PARREC)
	    else if (datatype == TY_BOOL)
		call strcat ("b", str, SZ_PARREC)
	    else
		call strcat ("t", str, SZ_PARREC)

	    call strcat (Memc[par+ip], str, SZ_PARREC)

	    call sfree (sp)

	} else {

	    locn = SZ_PACKED_REC * (parnum - 1) + SZ_SIZINFO + 1

	    call seek (TB_FILE(tp), locn)
	    stat = read (TB_FILE(tp), str, SZ_PACKED_REC)
	    call strupk (str, str, SZ_PARREC)
	}
end
