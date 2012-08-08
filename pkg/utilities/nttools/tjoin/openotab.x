include <tbset.h>
include "tjoin.h"

# B.Simon	16-Apr-99	first code

# OPEN_OTAB -- Open the output table

pointer procedure open_otab (outtable, tj1, tj2)

char	outtable[ARB]	# i: Output table name
pointer	tj1		# i: First input table descriptor
pointer	tj2		# i: Second input table descriptor
#--
int	type1, type2, icol, jcol, itab, tji[2]
int	colnum, datatype, lendata, lenfmt
pointer	tjo, sp, colname, colunits, colfmt, errtxt

string  notopen   "Could not open table (%s)"

int	tbpsta()
pointer	tbtopn()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (colunits, SZ_COLUNITS, TY_CHAR)
	call salloc (colfmt, SZ_COLFMT, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Allocate memory for data structure

	call malloc (tjo, LEN_TJSTRUCT, TY_INT)

	# Open table and put descriptor in structure

	iferr {
	    TJ_TAB(tjo) = tbtopn (outtable, NEW_FILE, NULL)
	} then {
	    call sprintf (Memc[errtxt], SZ_LINE, notopen)
	    call pargstr (outtable)
	    call error (1, Memc[errtxt])
	}

	# Set table type based on input tables

	type1 = tbpsta (TJ_TAB(tj1), TBL_WHTYPE)
	type2 = tbpsta (TJ_TAB(tj2), TBL_WHTYPE)
	if (type1 == type2)
	    call tbpset (TJ_TAB(tjo), TBL_WHTYPE, type1)

	# No join columns are used for output table

	TJ_JNUM(tjo) = 0
	TJ_JPTR(tjo) = NULL

	# Allocate array to hold output table data columns

	TJ_DNUM(tjo) = TJ_DNUM(tj1) + TJ_DNUM(tj2)
	call malloc (TJ_DPTR(tjo), TJ_DNUM(tjo), TY_INT)

	# Copy column information from the input tables to the output table

	tji[1] = tj1
	tji[2] = tj2

	jcol = 1
	do itab = 1, 2 {
	    do icol = 1, TJ_DNUM(tji[itab]) {
		call tbcinf (TJ_DCOL(tji[itab],icol), colnum, Memc[colname], 
			     Memc[colunits], Memc[colfmt], datatype, 
			     lendata, lenfmt)

		call renamecol (tji, itab, icol, Memc[colname], SZ_COLNAME)

		call tbcdef (TJ_TAB(tjo), TJ_DCOL(tjo,jcol), Memc[colname], 
			     Memc[colunits], Memc[colfmt], datatype, 
			     lendata, 1)

		jcol = jcol + 1
	    }
	}

	call tbtcre (TJ_TAB(tjo))
	call tbhcal (TJ_TAB(tj1), TJ_TAB(tjo))
	call tbhcal (TJ_TAB(tj2), TJ_TAB(tjo))

	call sfree (sp)
	return (tjo)
end
