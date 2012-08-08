include "../ptkeysdef.h"

# PT_FMKREC -- Construct the output record and mark the place of the
# requested field.

int procedure pt_fmkrec (key, field, element, line, nchars, first_rec,
	recptr, ncontinue)

pointer	key		# pointer to record structure
int	field		# the index of the requested field
int	element 	# the requested element of the field
char	line[ARB]	# input line
int	nchars		# length of line array
int	first_rec	# first record read
int	recptr		# line per record index
int	ncontinue	# number of unique lines per record

int	i, cip, nokeys, nckeys, nkeys, nper_line, len_move, offset
pointer	op

begin
	# Initialize the offset.
	offset = 0

	# Reinitialize if this is the start of a new record.
	if (recptr == 1)
	    nokeys = KY_NPKEYS(key)

	# Check repeat character.
	if (line[nchars-2] == '*')
	    ncontinue = ncontinue + 1
	else
	    ncontinue = 0

	# Fill in the record.
	cip = 1
	if (ncontinue < 1) {

	    nper_line = Memi[KY_NPLINE(key)+recptr-1]
	    nkeys = nokeys + nper_line
	    call amovki (int(1), Memi[KY_NELEMS(key)+nokeys], nper_line)

	    len_move = 0
	    do i = nokeys + 1, nkeys {
		if (field == i)
		    offset = cip + len_move
	        len_move = len_move + Memi[KY_KINDICES(key)+i-1]
	    }
	    op = Memi[KY_PTRS(key)+nokeys]
	    call amovc (line[cip], Memc[op], len_move)

	    cip = cip + len_move
	    recptr = recptr + 1
	    nokeys = nkeys

	} else if (ncontinue == 1) {

	    nckeys = nokeys + 1
	    nkeys = nokeys + Memi[KY_NPLINE(key)+recptr-1]

	    if (first_rec == YES) {
		Memi[KY_NCONTINUE(key)+recptr-1] = KY_NLINES
		do i = nckeys, nkeys
		    call malloc (Memi[KY_PTRS(key)+i-1], KY_NLINES *
		        Memi[KY_KINDICES(key)+i-1], TY_CHAR)
	    }

	    do i = nckeys, nkeys {
		if ((field == i) && (element == 1))
		    offset = cip
		call amovc (line[cip], Memc[Memi[KY_PTRS(key)+i-1]],
		    Memi[KY_KINDICES(key)+i-1])
		cip = cip + Memi[KY_KINDICES(key)+i-1]
	    }

	    nokeys = nkeys
	    recptr = recptr + 1

	} else {

	    if (ncontinue > Memi[KY_NCONTINUE(key)+recptr-2]) {
		Memi[KY_NCONTINUE(key)+recptr-2] = Memi[KY_NCONTINUE(key)+
		    recptr-2] + KY_NLINES
		do i = nckeys, nkeys
		    call realloc (Memi[KY_PTRS(key)+i-1], 
			Memi[KY_NCONTINUE(key)+recptr-2] *
			Memi[KY_KINDICES(key)+i-1], TY_CHAR) 
	    }

	    do i = nckeys, nkeys {
		op = Memi[KY_PTRS(key)+i-1] + (ncontinue - 1) *
		    Memi[KY_KINDICES(key)+i-1]
		if ((field == i) && (element == ncontinue))
		    offset = cip
		call amovc (line[cip], Memc[op], Memi[KY_KINDICES(key)+i-1])
		Memi[KY_NELEMS(key)+i-1] = ncontinue
		cip = cip + Memi[KY_KINDICES(key)+i-1]
	    }
	}

	return (offset)
end
