# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<pkg/rg.h>

# RG_NEXT -- Return the next value in a set of ordered and merged ranges.
# Number is set to the next value in the set of ranges or is unchanged
# (and EOF is returned) if there are no more values.

int procedure rg_next (rg, number)

pointer	rg			# RANGES pointer
int	number			# Both input and output parameter

int	next_number, i

begin
	next_number = number + 1

	do i = 1, RG_NRGS(rg)
	    if (next_number > RG_X2(rg, i)) {
		next
	    } else if (next_number < RG_X1(rg, i)) {
		number = RG_X1(rg, i)
	    	return (number)
	    } else {
		number = next_number
		return (number)
	    }

	return (EOF)
end
