include	<tbset.h>
include "tunits.h"

#* HISTORY *
#* B.Simon	07-Jan-99	Original

# FIND_ABREV -- Find the abbreviation for a units string

int procedure find_abrev (ab, units, abrev, maxch)

pointer	ab		# i: abbreviation hash table descriptor
char	units[ARB]	# i: units string
char	abrev[ARB]	# o: abbreviation string
int	maxch		# i: maximum length of abbreviation string
#--
int	status
pointer	ptr

int	get_unhash()

begin 
	status = get_unhash (ab, units, ptr)
	if (status == NO) {
	    abrev[1] = EOS
	} else {
	    call strcpy (Memc[ptr], abrev, maxch)
	}

	return (status)
end

# FREE_ABREV -- Free the abbreviation hash table

procedure free_abrev (ab)

pointer	ab		# i: abbreviation hash table descriptor
#--
int	index
pointer	sp, keyword, value

int	each_unhash()

begin
	call smark (sp)
	call salloc (keyword, LEN_UNIT, TY_CHAR)

	index = 0
	while (each_unhash (ab, index, Memc[keyword], 
			    value, LEN_UNIT) != EOF) {
	    if (value != NULL)
		call mfree (value, TY_CHAR)
	}


	call free_unhash (ab)
	call sfree (sp)
end

# READ_ABREV -- Read abbreviations from a table and load into a hash

pointer procedure read_abrev (abrevtab)

char	abrevtab[ARB]	# i: abbreviation table name
#--
int	irow, nrow
pointer	tp, c1, c2, sp, units, abrev, ab

string	nocolumn  "The abbreviation table must have two coulmns"

int	tbpsta()
pointer	tbtopn(), tbcnum(), new_unhash()

begin
	# Dynamic memory for strings

	call smark (sp)
	call salloc (units, LEN_UNIT, TY_CHAR)

	# Refer to columns numerically because 
	# this is supposed to be a text file

	tp = tbtopn (abrevtab, READ_ONLY, NULL)
	c1 = tbcnum (tp, 1)
	c2 = tbcnum (tp, 2)

	if (c1 == NULL || c2 == NULL)
	    call tuniterr (nocolumn, abrevtab)

	# Create hash

	nrow = tbpsta (tp, TBL_NROWS)
	ab = new_unhash (nrow, LEN_UNIT)

	# Read each row into hash

	do irow = 1, nrow {
	    call malloc (abrev, LEN_UNIT, TY_CHAR)

	    call tbegtt (tp, c1, irow, Memc[units], LEN_UNIT)
	    call tbegtt (tp, c2, irow, Memc[abrev], LEN_UNIT)

	    call strlwr (Memc[units])
	    call strlwr (Memc[abrev])

	    call add_unhash (ab, Memc[units], abrev)
	} 

	# Close table and free memory

	call tbtclo (tp)
	call sfree (sp)
	return (ab)
end
