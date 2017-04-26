include	<tbset.h>
include "tunits.h"

#* HISTORY *
#* B.Simon	07-Jan-99	Original

# FIND_UNITS -- Find the conversion factor for a set of units

int procedure find_units (ut, units, punit)

pointer	ut		# i: units hash table descriptor
char	units[ARB]	# i: units string
pointer	punit		# o: conversion factor as units structure
#--
int	get_unhash()

begin 
	return (get_unhash (ut, units, punit))
end

# FREE_UNITS -- Free the abbreviation hash table

procedure free_units (ut)

pointer	ut		# i: units hash table descriptor
#--
int	index
pointer	sp, units, punit

int	each_unhash()

begin
	call smark (sp)
	call salloc (units, LEN_UNIT, TY_CHAR)

	index = 0
	while (each_unhash (ut, index, Memc[units], 
			    punit, LEN_UNIT) != EOF) {
	    if (punit != NULL)
		call free_unstr (punit)
	}

	call free_unhash (ut)
	call sfree (sp)
end

# READ_UNITS -- Read units conversions from a table and load into a hash

pointer procedure read_units (ab, unittab)

pointer	ab		# i: abbreviation table descriptor
char	unittab[ARB]	# i: units conversion table name
#--
bool	swap, verbose
double	factor
int	irow, nrow
pointer	sp, temp, oldunits, newunits
pointer	tp, c1, c2, c3, c4
pointer	ut, punit1, punit2, punit3

data	verbose   / false /

string	nocolumn  "The units conversion table must have four columns"
string	badfactor "Error in units table: factor must be greater than zero"
string	nofinal   "Error in units table: conversion from final units not allowed"

int	tbpsta(), word_match()
pointer	tbtopn(), tbcnum(), new_unhash()
pointer	parse_units(), div_unstr()

begin
	# Dynamic memory for strings

	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (oldunits, SZ_FNAME, TY_CHAR)
	call salloc (newunits, SZ_FNAME, TY_CHAR)

	# Refer to columns numerically because 
	# this is supposed to be a text file

	tp = tbtopn (unittab, READ_ONLY, NULL)
	c1 = tbcnum (tp, 1)
	c2 = tbcnum (tp, 2)
	c3 = tbcnum (tp, 3)
	c4 = tbcnum (tp, 4)

	if (c1 == NULL || c2 == NULL || c3 == NULL || c4 == NULL)
	    call tuniterr (nocolumn, unittab)

	# Create hash

	nrow = tbpsta (tp, TBL_NROWS)
	ut = new_unhash (nrow, LEN_UNIT)

	# Read each row into hash

	do irow = 1, nrow {
	    # Read table columns

	    call tbegtd (tp, c1, irow, factor)
	    call tbegtt (tp, c2, irow, Memc[oldunits], SZ_FNAME)
	    call tbegtt (tp, c3, irow, Memc[newunits], SZ_FNAME)
	    call tbegtb (tp, c4, irow, swap)

	    # Check conversion factor

	    if (factor <= 0.0)
		call tuniterr (badfactor, Memc[oldunits])

	    # Swap the units string and the conversion factor

	    if (swap) {
		call strcpy (Memc[oldunits], Memc[temp], SZ_FNAME)
		call strcpy (Memc[newunits], Memc[oldunits], SZ_FNAME)
		call strcpy (Memc[temp], Memc[newunits], SZ_FNAME)
	    }

	    # Check to see that old units aren't one of the final forms

	    if (word_match (Memc[oldunits], FINALS) != 0)
		call tuniterr (nofinal, Memc[oldunits])

	    # Parse the old and new units strings

	    call strlwr (Memc[newunits])
	    punit1 = parse_units (ab, Memc[newunits])

	    call strlwr (Memc[oldunits])
	    punit2 = parse_units (ab, Memc[oldunits])

	    # The conversion factor is ratio of the two sets of units

	    punit3 = div_unstr (punit1, punit2)
	    if (swap) {
		TUN_FACTOR(punit3) = factor
	    } else {
		TUN_FACTOR(punit3) = 1.0 / factor
	    }

	    if (verbose) {
		call str_unstr (punit3, Memc[temp], SZ_FNAME)

		call eprintf ("The conversion factor is %s\n\n")
		call pargstr (Memc[temp])
	    }

	    # Add it to the hash

	    call abrev_unstr (ab, Memc[oldunits], Memc[temp], SZ_FNAME)
	    call add_unhash (ut, Memc[temp], punit3)

	    call free_unstr (punit1)
	    call free_unstr (punit2)
	} 

	# Close table and free memory

	call tbtclo (tp)
	call sfree (sp)
	return (ut)
end
