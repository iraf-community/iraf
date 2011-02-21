include <tbset.h>
include "tjoin.h"

# B.Simon	16-Apr-99	first code

# OPEN_ITAB -- Open one of the input tables used in the join

pointer procedure open_itab (intable, column)

char	intable[ARB]	# i: Input table name
char	column[ARB]	# i: List of join columns
#--
int	ic, icol
pointer	tj, sp, cname, errtxt

string	nojoincol "No column supplied as join column"
string	badcolnam "Column name not found in table (%s[c:%s])"
string  notopen   "Could not open table (%s)"

bool	strne()
int	tbpsta(), tbcnum(), word_count(), word_fetch()
pointer	tbtopn()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (cname, SZ_COLNAME, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Allocate memory for data structure

	call calloc (tj, LEN_TJSTRUCT, TY_INT)

	# Open table and put descriptor in structure

	iferr {
	    TJ_TAB(tj) = tbtopn (intable, READ_ONLY, NULL)
	} then {
	    call sprintf (Memc[errtxt], SZ_LINE, notopen)
	    call pargstr (intable)
	    call error (1, Memc[errtxt])
	}

	# Create array of data columns

	TJ_DNUM(tj) = tbpsta (TJ_TAB(tj), TBL_NCOLS)
	call malloc (TJ_DPTR(tj), TJ_DNUM(tj), TY_INT)

	do icol = 1, TJ_DNUM(tj)
	    TJ_DCOL(tj,icol) = tbcnum (TJ_TAB(tj), icol)

	# Create array of join columns

	TJ_JNUM(tj) = word_count (column)
	if (TJ_JNUM(tj) == 0)
	    call error (1, nojoincol)

	call malloc (TJ_JPTR(tj), TJ_JNUM(tj), TY_INT)

	ic = 1
	icol = 1
	while (word_fetch (column, ic, Memc[cname], SZ_COLNAME) > 0) {
	    call tbcfnd (TJ_TAB(tj), Memc[cname], TJ_JCOL(tj,icol), 1)

	    if (TJ_JCOL(tj,icol) == NULL) {
		if (strne (Memc[cname], ROWNAME)) {
		    call sprintf (Memc[errtxt], SZ_LINE, badcolnam)
		    call pargstr (intable)
		    call pargstr (Memc[cname])
		    call error (1, Memc[errtxt])
		}
	    }

	    icol = icol + 1
	}

	# Free temporary memory and return descriptor of new structure
 
	call sfree (sp)
	return (tj)
end
