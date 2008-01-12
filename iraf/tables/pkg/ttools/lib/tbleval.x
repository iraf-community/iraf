include	<config.h>
include	<error.h>
include	<evexpr.h>
include	<xwhen.h>
include	"reloperr.h"

# TBL_EVAL -- Evaluate an arbitrary expression over table columns
#
# This procedure receives as input a table descriptor, an index array, and 
# a character string containing an algebraic expression. The terms in the 
# expression are column names. The expression is evaluated for each row in 
# the index array using the values from the indicated columns and the results 
# stored in the output array (aryptr). The array pointed to by nulptr 
# contains null flags. A null flag is set to true if any of the table elements
# in the expression is null or an arithmetic error ocurs during the 
# evaluation of the expression. Otherwise the null flag is set to false. 
# The type of the output array is determined by the type of the expression 
# unless all the elements are null, in which case the type input by the 
# calling routine is used. The two arrays pointed to by aryptr and nulptr 
# must be deallocated by the calling routine.
#
# B.Simon	29-Sept-87	First Code
# B.Simon	16-Dec-87	Changed to handle table subsets
# B.Simon	13-Apr-88	tbl_term, tbl_func moved to separate file

procedure tbl_eval (tp, nindex, index, expr, dtype, aryptr, nulptr)

pointer	tp		#  i: Table descriptor
int	nindex		#  i: Number of elements in index array
int	index[ARB]	#  i: Array of row indices
char	expr[ARB]	#  i: Expression to be evaluated
int	dtype		# io: Type of output array
pointer	aryptr		#  o: Array of output values
pointer	nulptr		#  o: Array of null flags
#--
include	"tblterm.com"

int	iary, status, junk
int	old_handler, tbl_term_adr, tbl_func_adr
pointer	op

string	badtype		"Character expressions not allowed"

int	locpr(), errcode()
pointer	evexpr()

extern	tbl_handler(), tbl_term(), tbl_func()

begin
	# Initialize output variables

	aryptr = NULL
	call malloc (nulptr, nindex, TY_BOOL)

	# Set up error handler to catch arithmetic errors

	call xwhen (X_ARITH, locpr(tbl_handler), old_handler)

	table = tp
	nterm = 0
	constant = true

	tbl_term_adr = locpr (tbl_term)
	tbl_func_adr = locpr (tbl_func)

	# Loop over all rows of the table

	do iary = 1, nindex {

	    irow = index[iary]
	    iterm = 0

	    # Execution will resume here when an arithmetic error occurs

	    call zsvjmp (jumpbuf, status)

	    if (status != OK) {
		Memb[nulptr+iary-1] = true

	    # Special case to speed up the evaluation of constant expressions

	    } else if (constant && (iary != 1)) {
		Memb[nulptr+iary-1] = false
		switch (dtype) {
		case TY_BOOL:
		    Memb[aryptr+iary-1] = Memb[aryptr]
		case TY_INT:
		    Memi[aryptr+iary-1] = Memi[aryptr]
		case TY_REAL:
		    Memr[aryptr+iary-1] = Memr[aryptr]
		}

	    # Evaluate the expression using the values in the current row

	    } else {
		iferr {
		    op = evexpr (expr, tbl_term_adr, tbl_func_adr)
		} then {

		    # Catch the error sent when a table element is null

		    if (errcode() == PUTNULL)
			Memb[nulptr+iary-1] = true
		    else {
			call mfree (nulptr, TY_BOOL)
			call xwhen (X_ARITH, old_handler, junk)
			call erract (EA_ERROR)
		    }

		# Usual case

		} else {

		    Memb[nulptr+iary-1] = false

		    # Determine array type from type of expression

		    if (aryptr == NULL) {
			if (O_TYPE(op) == TY_CHAR) {
			    call mfree (nulptr, TY_BOOL)
			    call xwhen (X_ARITH, old_handler, junk)
			    call error (SYNTAX, badtype)
			}
			dtype = O_TYPE(op)
			call calloc (aryptr, nindex, dtype)
		    }

		    # Assign the result of the expression to the output
		    # array

		    switch (dtype) {
		    case TY_BOOL:
			Memb[aryptr+iary-1] = O_VALB(op)
		    case TY_INT:
			Memi[aryptr+iary-1] = O_VALI(op)
		    case TY_REAL:
			Memr[aryptr+iary-1] = O_VALR(op)
		    }

		    call mfree (op, TY_STRUCT)	# Bug fix (BPS 04.20.93)
		}
	    }
	}

	# Allocate array when all results are null

	if (aryptr == NULL) {
	    if (dtype == TY_CHAR) {
		call mfree (nulptr, TY_BOOL)
		call xwhen (X_ARITH, old_handler, junk)
		call error (SYNTAX, badtype)
	    }
	    call calloc (aryptr, nindex, dtype)
	}
	# Restore old error handler

	call xwhen (X_ARITH, old_handler, junk)

end
