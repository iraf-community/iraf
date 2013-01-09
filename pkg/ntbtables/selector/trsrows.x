include "trs.h"

.help ---------------------------------------------------------------------

TRSROWS -- Return a set of rows for which an expression is true

This procedure evalutes a row selection expression and returns a set
containing the row numbers for which the expression is true. The set
can be accessed and maniputlauted using the functions in rst.x, which
are further described in the help block in that file. One example of
how to use this function is:

.nf
	set = trsrows (tp, expr)
	nset = rst_nelem (set)
	do iset = 1, nset {
	    irow = rst_rownum (set, iset)
	    # do something with the row here
	}
	call rst_free (set)
.fi

In the above example, we create the set, query to get the number of
rows in the set, and then access the rows in sequential order. This
approach is useful when it is necessary to determine the number of
rows matched before doing any processing, so that one can allocate
arrays or take error actions based on the number of rows returned. If
neither of these is necessary, one can alternatively use a repeat
loop.

.nf
	set = trsrows (tp, expr)
	iset = 1
	repeat {
	    irow = rst_rownum (set, iset)
	    if (irow == 0)
		break
	    # do something with the row here
	    iset = iset + 1
	}
	call rst_free (set)
.fi

The loop ends because rst_rownum returns zero when asked for an
element less than one or greater than the number of rows in the set.
While both of these examples access the set sequentially, rst_rownum
also supports random access.

.endhelp ------------------------------------------------------------------

pointer procedure trsrows (tp, expr)

pointer	tp		# i: table descriptor
char	expr[ARB]	# i: expression to be evaluated
#--
int	iset, irow
pointer	pcode, code, set

bool	trscalc()
int	rst_rownum()
pointer	trsopen(), rst_copy(), rst_create()
errchk	trsopen, trscalc, trsclose

begin
	# Compile the expression into pseudocode

	pcode = trsopen (tp, expr)

	# If the code is a null program, just return the set, otherwise 
	# calculate the result for each element in the set

	code = TRS_CODE (pcode)
	if (Memi[code] == YDONE) {
	    set = rst_copy (TRS_ROWS(pcode))

	} else {
	    # Start with an empty set. Calculate the result for each
	    # element in the row set and if true, add it to the output set

	    set = rst_create (0, 0)

	    iset = 1
	    repeat {
		irow = rst_rownum (TRS_ROWS(pcode), iset)
		if (irow == 0)
		    break

		if (trscalc (tp, irow, code))
		    call rst_addval (set, irow)

		iset = iset + 1
	    }
	}

	# Release the pseudocode structure, return the set

	call trsclose (pcode)
	return (set)
end
