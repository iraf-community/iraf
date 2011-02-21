include	<math.h>
include	<tbset.h>

#* HISTORY *
#* B.Simon	24-Aug-94	original

# GETWEIGHT -- Get array of weights from list of factors or tables

procedure getweight (ncol, col1, col2, factor, weight)

int	ncol		# i: number of match columns
pointer	col1[ARB]	# i: match columns from first table
pointer	col2[ARB]	# i: match columns from second table
char	factor[ARB]	# i: list of factors
double	weight[ARB]	# o: array of weights
#--
double	unitval[6]
int	invert[6]
int	ic, jc, nc, icol, jcol, type1, type2, item
pointer	sp, value, unit1, unit2, errmsg


data	unitval	  / 1.0, 3600.0, 60.0, 1.0, 15.0, RADIAN /
data	invert	  / NO,  YES,    YES,  NO,  NO,   NO     /

string	unitlist  "|seconds|minutes|degrees|hours|radians|"
string	badvalue  "Value in factor string is not a number (%s)"
string	badunits  "Units mismatch in column %d of tables"

int	ctod(), word_fetch(), strdic()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (unit1, SZ_FNAME, TY_CHAR)
	call salloc (unit2, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Get each string from the list and convert to a number

	ic = 1
	icol = 0
	while (word_fetch (factor, ic, Memc[value], SZ_FNAME) > 0) {
	    icol = icol + 1

	    jc = 1
	    nc = ctod (Memc[value], jc, weight[icol])
	    if (Memc[value+jc-1] != EOS) {
		call sprintf (Memc[errmsg], SZ_LINE, badvalue)
		call pargstr (Memc[value])

		call error (1, Memc[errmsg])
	    }
	}

	# Set remaining weights according to column units

	do jcol = icol+1, ncol {
	    # Read units from table

	    call tbcigt (col1[jcol], TBL_COL_UNITS, Memc[unit1], SZ_FNAME)
	    call tbcigt (col2[jcol], TBL_COL_UNITS, Memc[unit2], SZ_FNAME)

	    # Search for units in dictionary

	    call strlwr (Memc[unit1])
	    call strlwr (Memc[unit2])

	    type1 = strdic (Memc[unit1], Memc[unit1], SZ_FNAME, unitlist)
	    type2 = strdic (Memc[unit2], Memc[unit2], SZ_FNAME, unitlist)

	    # Take exit if units do not match

	    if (type1 != type2) {
		call sprintf (Memc[errmsg], SZ_LINE, badunits)
		call pargi (jcol)

		call error (1, Memc[errmsg])
	    }

	    # Read corresponding weight from unit value array
	    # The first weight (1.0) is for missing or unknown units

	    item = type1 + 1

	    if (invert[item] == NO) {
		weight[jcol] = unitval[item]
	    } else {
		weight[jcol] = 1.0 / unitval[item]
	    }
	}

	call sfree (sp)
end
