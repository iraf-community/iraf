# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STENTER -- Add a symbol to the symbol table.  If the named symbol is already
# present in the table it will be redefined until STFREE is called to return
# the storage allocated for the current symbol.  A pointer to the user part
# of the symstruct is returned as the function value.

pointer	procedure stenter (stp, key, u_symlen)

pointer	stp			# symbol table descriptor
char	key[ARB]		# symbol name
int	u_symlen		# length of user part of symstruct (su)

long	sum
pointer	el, tp
int	symlen, new_symbol, thread, ip
int	stpstr(), stalloc()
errchk	stalloc, stpstr

begin
	if (key[1] == EOS)
	    call error (1, "stenter: null key string")

	# Hash the key onto a thread in the index.
	sum = 0
	do ip = 1, MAX_HASHCHARS {
	    if (key[ip] == EOS)
		break
	    sum = sum + (sum + key[ip])
	}

	thread = mod (sum, ST_INDEXLEN(stp))
	tp = ST_INDEX(stp) + thread

	# Allocate space in STAB.
	symlen = LEN_SYMSTRUCT + u_symlen
	new_symbol = stalloc (stp, symlen)

	# Initialize symstruct.
	el = ST_STABP(stp) + new_symbol
	E_NEXTHASH(el)	= Memi[tp]
	E_NEXTGLOB(el)	= ST_LASTSYMBOL(stp)
	E_THREAD(el)    = thread
	E_KEY(el)	= stpstr (stp, key, 0)

	# Set the head of thread list and the head of the global list to
	# point to the new symbol.  Flag the first key character (used to
	# quickly determine that a key beginning with a certain character
	# is not present in the table).

	Memi[tp] = new_symbol
	ST_LASTSYMBOL(stp) = new_symbol
	ST_NSYMBOLS(stp) = ST_NSYMBOLS(stp) + 1
	ST_ASCII(stp,key[1]) = 1

	return (E_USERFIELDS(el))
end
