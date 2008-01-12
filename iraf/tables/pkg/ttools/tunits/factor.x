include "tunits.h"

#* HISTORY *
#* B.Simon	07-Jan-99	Original

# FIND_FACTOR -- Find conversion factor between two sets of units

double procedure find_factor (ut, punit1, punit2, verbose)

pointer	ut		# i: units hash descriptor
pointer	punit1		# i: old set of units
pointer	punit2		# i: new set of units
bool	verbose		# i: diagnostic message flag
#--
double	factor
pointer	punit3, punit4, punit5

string	noconvert  "The old and new units are not compatible"

pointer	reduce_factor(), div_unstr()

begin
	# Reduce old and new units to a common form

	punit3 = reduce_factor (ut, punit1, verbose)
	punit4 = reduce_factor (ut, punit2, verbose)

	# The conversion factor is the ratio of 
	# the two sets of units when in common form

	punit5 = div_unstr (punit3, punit4)

	# Check to make sure units actually have a common form

	if (TUN_UNPTR(punit5,1) != NULL)
	    call error (1, noconvert)

	factor = TUN_FACTOR (punit5)

	# Print conversion factor
	if (verbose) {
	    call eprintf ("The conversion factor is %g\n")
	    call pargd (factor)
	}

	# Free temporary units descriptors

	call free_unstr (punit3)
	call free_unstr (punit4)
	call free_unstr (punit5)

	return (factor)
end

# REDUCE_FACTOR -- Reduce units descriptor to a common set of units (mks)

pointer procedure reduce_factor (ut, punit, verbose)

pointer	ut		# i: Units hash descriptor
pointer	punit		# i: Units string descriptor
bool	verbose		# i: diagnostic message flag
#--
bool	done
int	idx
pointer	sp, units, punit1, punit2, punit3, punit4

int	find_units()
pointer	copy_unstr(), pow_unstr(), mul_unstr()

begin
	# Allocate memory for units string

	call smark (sp)
	call salloc (units, SZ_FNAME, TY_CHAR)

	# Loop until no more reductions can be performed

	punit1 = copy_unstr (punit)

	repeat {
	    if (verbose) {
		call str_unstr (punit1, Memc[units], SZ_FNAME)
		call eprintf ("%s")
		call pargstr (Memc[units])
	    }

	    # Search for a reduction for any term

	    done = true
	    do idx = 1, MAXUNIT {
		if (TUN_UNPTR(punit1,idx) == NULL)
		    break

		if (find_units (ut, TUN_UNITS(punit1,idx), punit2) ==YES) {
		    # Reduction found. Raise conversion factor to
		    # degree of term in descriptor and then multiply
		    # the units by it 

		    punit3 = pow_unstr (punit2, TUN_POWER(punit1,idx))
		    punit4 = mul_unstr (punit1, punit3)

		    call free_unstr (punit1)
		    call free_unstr (punit3)

		    punit1 = punit4
		    done = false
		    break
		}
	    }

	    if (verbose) {
		if (done) {
		    call eprintf ("\n") 
		} else {
		    call eprintf (" is \n") 
		}
	    }
	} until (done)

	if (verbose)
	    call eprintf ("\n")

	call sfree (sp)
	return (punit1)
end
