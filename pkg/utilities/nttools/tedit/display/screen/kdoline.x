include	<ctype.h>

define	SYNTAX		1
define	BOUNDS		2

# K_DOLINE -- Add a line containing an escape sequence to the key table
#
# B.Simon	23-Jan-89	Original

procedure k_doline (escstr, tag, maxtab, ntab, table) 

char	escstr[ARB]	#  i: Key sequence
int	tag		#  i: Key tag
int	maxtab		#  i: Maximum number of entries
int	ntab		# io: Current number of entries
int	table[4,ARB]	# io: Key sequence table
#--
int	ic, link, new, old
pointer	sp, escseq

int	strlen()

string	notctrl  "Key sequence must begin with a control character"
string	isambig  "Ambiguous key sequence"
string	toomany  "Too many key definitions"

begin
	# Convert escape sequence

	call smark (sp)
	call salloc (escseq, SZ_FNAME, TY_CHAR)
	call k_convert (escstr, Memc[escseq], SZ_FNAME)

	# Don't process null sequences

	if (Memc[escseq] == EOS)
	    return

	# Check to see if escape sequence is valid

	if (IS_PRINT(Memc[escseq]))
	    call error (SYNTAX, notctrl)

	# Find first character in key sequence that is new

	ic = 0
	link = 0
	for (new = 1; new != 0; new = table[link,old]) {
	    old = new
	    if (link == 1)
		ic = ic + 1
	    if (Memc[escseq+ic] == table[3,old])
		link = 1
	    else
		link = 2
	}

	if (link == 1) {

	    # Redefinition of existing sequence

	    if (Memc[escseq+ic] != EOS)
		call error (SYNTAX, isambig)
	    table[4,old] = tag

	} else {

	    # New sequence

	    if (Memc[escseq+ic] == EOS)
		call error (SYNTAX, isambig)

	    # Check for table overflow

	    if (strlen (Memc[escseq+ic]) + ntab > maxtab)
		call error (BOUNDS, toomany)

	    # Insert remainder of key sequence in table

	    table[2,old] = ntab + 1
	    while (Memc[escseq+ic] != EOS) {
		ntab = ntab + 1
		table[1,ntab] = ntab + 1
		table[2,ntab] = 0
		table[3,ntab] = Memc[escseq+ic]
		table[4,ntab] = 0
		ic = ic + 1
	    }
	    table[1,ntab] = 0
	    table[4,ntab] = tag

	}

	call sfree (sp)

end
