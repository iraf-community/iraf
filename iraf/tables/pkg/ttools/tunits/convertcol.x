include	<tbset.h>

#* HISTORY *
#* B.Simon	07-Jan-99	Original

# CONVERT_COL -- Convert the units of a table column

procedure convert_col (tp, cp, newunits, factor)

pointer	tp		# i: table descriptor
pointer	cp		# i: column descriptor
char	newunits[ARB]	# i: new column units
double	factor		# i: conversion factor
#--
double	value
int	nrow, nelem, irow, nlen, ilen
pointer	sp, buffer

int	tbpsta(), tbcigi(), tbagtd()

begin
	# Change column units

	call tbcnit (tp, cp, newunits)

	# Get column dimensions

	nrow = tbpsta (tp, TBL_NROWS)
	nelem = tbcigi (cp, TBL_COL_LENDATA)

	# Allocate buffer to hold array elements

	call smark (sp)
	call salloc (buffer, nelem, TY_DOUBLE)

	# Multiply column values by conversion factor

	if (nelem == 1) {
	    # Scalar column, use element get and put

	    do irow = 1, nrow {
		call tbegtd (tp, cp, irow, value)
		if (! IS_INDEFD (value)) {
		    value = factor * value
		    call tbeptd (tp, cp, irow, value)
		}
	    }

	} else {
	    # Array element, use array get and put

	    do irow = 1, nrow {
		nlen = tbagtd (tp, cp, irow, Memd[buffer], 1, nelem)

		do ilen = 0, nlen-1 {
		    if (! IS_INDEFD (Memd[buffer+ilen])) {
			Memd[buffer+ilen] = factor * Memd[buffer+ilen]
		    }
		}

		call tbaptd (tp, cp, irow, Memd[buffer], 1, nlen)
	    }
	}

	call sfree (sp)
end


