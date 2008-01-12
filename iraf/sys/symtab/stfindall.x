# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STFINDALL -- Search the symbol table for the named key and return an array
# of symstruct pointers to all symbols with the given key.  The array is
# ordered with the most recently entered symbols at the beginning.  The number
# of symbols found is returned as the function value.

int procedure stfindall (stp, key, symbols, max_symbols)

pointer	stp			# symbol table descriptor
char	key[ARB]		# symbol name
pointer	symbols[max_symbols]	# pointers to the symstructs
int	max_symbols

long	sum
char	first_char
int	head, ip, nsym, thread
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
	# pointers to all occurrences of the symbol.

	nsym = 0

	if (head != NULL && max_symbols > 0) {
	    first_char = key[1]
	    sbuf = ST_SBUFP(stp)
	    stab = ST_STABP(stp)

	    for (el=stab+head;  el > stab;  el=stab+E_NEXTHASH(el)) {
		cp = sbuf + E_KEY(el)
		if (Memc[cp] != first_char)
		    next

		# If the first character of the key matches compare the full
		# string and output a symstruct pointer if we have a match.

		do ip = 1, MAX_SZKEY {
		    if (key[ip] != Memc[cp])
			break
		    if (key[ip] == EOS) {
			nsym = nsym + 1
			symbols[nsym] = E_USERFIELDS(el)
			if (nsym >= max_symbols)
			    return (max_symbols)
			break
		    }
		    cp = cp + 1
		}
	    }
	}

	return (nsym)
end
