include	<ctype.h>

define	CAN_ABORT	NO
define	ABORT		'\003'

# K_GET -- Read a character, translating function keys

int procedure k_get ()

#--
include	"screen.com"

int	ch

int	first
int	k_cget(), k_lookup()

string	abortmsg  "Task aborted by ^C"

begin
	if (keych != EOF) {
	    # Check for a pushed back character, returning it if found

	    ch = keych
	    keych = EOF

	} else {
	    # Check character to see if it is the start of a control sequence
	    # If not, return the character without searching the table

	    first = k_cget ()

	    if (IS_PRINT(first))
		ch = first
	    else if ((CAN_ABORT == YES) && (first == ABORT))
		call error (1, abortmsg)
	    else
		ch = k_lookup (first, Memi[keytab]) 
	}

	return (ch)

end

# K_LOOKUP -- Look up a function key sequence in a table

int procedure k_lookup (first, table)

int	first		# i: First character in the sequence
int	table[4,ARB]	# i: Table of function key sequences
#--
include	"screen.com"

int	key, new, old, link
int	k_cget()

begin
	# Search the table for the control sequence

	link = 0
	key = first

	for (new = 1; new != 0; new = table[link,old]) {
	    old = new
	    if (link == 1)
		key = k_cget ()
	    if (key == table[3,old])
		link = 1
	    else
		link = 2
	}

	# Return the control sequence tag if the sequence was found,
	# otherwise return the first unrecognized key

	if (link == 1)
	    key = table[4,old]

	return (key)

end

# K_CGET -- Read a single character from the terminal

int procedure k_cget ()

#--
include	"screen.com"

int	ch
int	and(), getci()

begin
	ch = getci (ttyin, ch)
	return (and (ch, 177B))
end
