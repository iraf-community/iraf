# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STFIND -- Search the symbol table for the named key and return a pointer to
# the symstruct or NULL.  This is the main table lookup procedure.  If the
# thread is empty NULL is returned after only a hash function call.  If there
# is only one element on a thread (common for well conditioned symbol tables)
# the expense is essentialy two traversals of the key string plus procedure
# overhead (pointer calculations, etc.).

pointer procedure stfind (stp, key)

pointer	stp			# symbol table descriptor
char	key[ARB]		# symbol name

long	sum
char	first_char
int	head, ip, thread
pointer	el, cp, stab, sbuf

begin
	# When a symbol is entered in the table a flag is set in the ST_ASCII
	# array to flag that the symbol table contains at least one key
	# beginning with that character.  If the flag is not set we can thus
	# determine very quickly that the symbol is not present.  This is
	# important for applications such as mapping identifiers for macro
	# expansion, where most macros have upper case keys but most program
	# identifiers have lower case keys.  (Subtle note: since the first
	# element of ST_ASCII is for ascii value 0=EOS, the following also
	# serves to detect null keys).

	if (ST_ASCII(stp,key[1]) == 0)
	    return (NULL)

	# Hash the key onto a thread in the index.
	sum = 0
	do ip = 1, MAX_HASHCHARS {
	    if (key[ip] == EOS)
		break
	    sum = sum + (sum + key[ip])
	}

	thread = mod (sum, ST_INDEXLEN(stp))
	head = Memi[ST_INDEX(stp)+thread]

	# If thread is not empty search down it for the named key and return
	# the symbol pointer if found.  Note that the value of the E_NEXTHASH
	# pointer is given as an integer offset to facilitate reallocation
	# upon overflow.

	if (head != NULL) {
	    first_char = key[1]
	    sbuf = ST_SBUFP(stp)
	    stab = ST_STABP(stp)

	    for (el=stab+head;  el > stab;  el=stab+E_NEXTHASH(el)) {
		cp = sbuf + E_KEY(el)
		if (Memc[cp] != first_char)
		    next

		# Compare target key to symbol key.
		do ip = 1, MAX_SZKEY {
		    if (key[ip] != Memc[cp])
			break
		    if (key[ip] == EOS)
			return (E_USERFIELDS(el))	# found key
		    cp = cp + 1
		}
	    }
	}

	return (NULL)
end
