include	<error.h>
include <evexpr.h>
include	<tbset.h>
include "../../lib/ptkeysdef.h"

# PT_APCONVERT -- Procedure to select records from a text file in pseudo list
# format.

procedure pt_convert (fd, td, fields, expr, append)

int	fd		# text file descriptor
int	td		# table file descriptor
char	fields[ARB]	# fields to be output
char	expr[ARB]	# boolean expression to be evaluated
int	append		# append to an existing table

bool	oexpr
int	nchars, nunique, uunique, funique, ncontinue, recptr, rownum
int 	ntotkeys, first_rec, printall, table_defined
pointer	sp, line, colpoints, key, o

bool	streq()
extern	pt_getop ()
int	getline(), strncmp(), tbpsta(), pt_deftable(), pt_apptable()
pointer	evexpr(), locpr()
real	asumi()

begin

	# Allocate temporary working space.
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Initialize the keys structure.
	call pt_kyinit (key)

	# Initialize the counters.
	nunique = 0
	uunique = 0
	funique = 0

	# Initalize the record reading code.
	table_defined = NO
	first_rec = YES
	recptr = 1
	ncontinue = 0

	# Initialize the expression evaluator.
	o = NULL
	if (streq (expr, "yes")) {
	    oexpr = true
	    printall = YES
	} else {
	    oexpr = false
	    printall = NO
	}

	# Initialize the pointer for writing rows.
	if (append == YES)
	    rownum = tbpsta (td, TBL_NROWS)
	else
	    rownum = 0

	# Loop over the text file records.
	nchars = getline (fd, Memc[line])
	while (nchars != EOF) {

	    # Determine the type of record.
	    if (Memc[line] ==  KY_CHAR_POUND) {
	        if (strncmp (Memc[line], KY_CHAR_KEYWORD, KY_LEN_STR) == 0) {
		    call pt_kyadd (key, Memc[line], nchars)
	        } else if (strncmp (Memc[line], KY_CHAR_NAME,
		    KY_LEN_STR) == 0) {
		    nunique = nunique + 1
		    call pt_kname (key, Memc[line], nchars, nunique)
	        } else if (strncmp (Memc[line], KY_CHAR_UNITS,
		    KY_LEN_STR) == 0) {
		    uunique = uunique + 1
		    call pt_knunits (key, Memc[line], nchars, uunique)
	        } else if (strncmp (Memc[line], KY_CHAR_FORMAT,
		    KY_LEN_STR) == 0) {
		    funique = funique + 1
		    call pt_knformats (key, Memc[line], nchars, funique)
	        }

	    } else if (Memc[line] == KY_CHAR_NEWLINE) {
		# skip blank lines

	    } else {

		# Construct the table record.
		call pt_mkrec (key, Memc[line], nchars, first_rec, recptr,
		    ncontinue) 

	        # Construct output record when there is no continuation char.
	        if (Memc[line+nchars-2] != KY_CHAR_CONT) {

		    # Construct the output table if not defined.
		    if (table_defined == NO) {

			# Compute the maximum number of keys.
			ntotkeys = nint (asumi (Memi[KY_NELEMS(key)],
			    KY_NKEYS(key)))

			# Allocate space for the column pointers.
			call salloc (colpoints, ntotkeys, TY_INT)

			# Initialize the table.
		        if (append == NO) {
		            if (pt_deftable (td, key, fields,
			        Memi[colpoints]) <= 0)
				break
		        } else if (append == YES) {
			    if (pt_apptable (td, key, fields,
			        Memi[colpoints]) <= 0)
				break
			}

			table_defined = YES
		    }

		    # Evaluate the expression.
		    iferr {
			if (printall == NO) {
		    	    call pt_apset (key)
		    	    o = evexpr (expr, locpr (pt_getop), 0)
		    	    if (O_TYPE(o) != TY_BOOL)
				call error (0, "Expression must be a boolean")
			    oexpr = O_VALB(o)
			}
		    }  then {
			call erract (EA_WARN)
			call xev_freeop (o)
		        call mfree (o, TY_STRUCT)
			break
		    }

		    # Construct the output record.
		    if (oexpr) {
			rownum = rownum + 1
			call pt_putrec (td, Memi[colpoints], key, rownum)
		    }

		    # Get ready for next record.
		    ncontinue = 0
		    recptr = 1
		    first_rec = NO
		    if (o != NULL) {
			call xev_freeop (o)
		        call mfree (o, TY_STRUCT)
		    }
	        }
	    }

	    # Read the next line.
	    nchars = getline (fd, Memc[line])
	}

	# Free the keys structure.
	call pt_kyfree (key)
	call sfree (sp)
end


# PT_PUTREC -- Add a row to the table.

procedure pt_putrec (td, colpoints, key, rownum)

pointer	td			# table descriptor
pointer colpoints[ARB]		# pointers for columns
pointer	key			# key structure
int	rownum			# row number

int 	i, index, elem, kip, maxch, n, nk, ncols
int	number, datatype, lendata, lenfmt, ival
pointer	sp, colname, colunits, colfmt, str
real	rval
bool	streq()
int	ctor(), ctoi()

begin
	call smark (sp)
	call salloc (colname, SZ_COLNAME+1, TY_CHAR)
	call salloc (colunits, SZ_PARREC+1, TY_CHAR)
	call salloc (colfmt, SZ_PARREC+1, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# For each selected field move into the table
	ncols = 0
	do i = 1, KY_NSELECT(key) {

	    # Get the pointer to the desired value.
	    index = Memi[KY_SELECT(key)+i-1]
	    elem = Memi[KY_ELEM_SELECT(key)+i-1]
	    maxch = Memi[KY_LEN_SELECT(key)+i-1]
	    kip = Memi[KY_PTRS(key)+index-1] + (elem - 1) * maxch

	    # Trim leading and trailing whitespace from the value and 
	    # copy value to output buffer.

	    for (n = 0; Memc[kip] == ' '; n = n + 1)
		kip = kip + 1
	    for (nk = 0; Memc[kip+nk] != ' ' && n <= maxch; nk = nk + 1)
		n = n + 1
	    call strcpy (Memc[kip], Memc[str], nk)

	    # Find info about this column.
	    ncols = ncols + 1
	    call tbcinf (colpoints[ncols], number, Memc[colname],
	        Memc[colunits], Memc[colfmt], datatype, lendata, lenfmt)

	    # Move the selected value into the column.
	    kip = 1
	    switch (datatype) {
	    case TY_INT:
		if (ctoi (Memc[str], kip, ival) <= 0)
		    call tbrpti (td, colpoints[ncols], INDEFI, 1, rownum)
		else
		    call tbrpti (td, colpoints[ncols], ival, 1, rownum)
	    case TY_REAL:
		if (ctor (Memc[str], kip, rval) <= 0)
		    call tbrptr (td, colpoints[ncols], INDEFR, 1, rownum)
		else
		    call tbrptr (td, colpoints[ncols], rval, 1, rownum)
	    case TY_BOOL:
		if (streq ("yes", Memc[str]))
		    call tbrptb (td, colpoints[ncols], true, 1, rownum)
		else
		    call tbrptb (td, colpoints[ncols], false, 1, rownum)
	    default:
		call tbrptt (td, colpoints[ncols], Memc[str], nk, 1, rownum)
	    }
	}

	call sfree (sp)
end
