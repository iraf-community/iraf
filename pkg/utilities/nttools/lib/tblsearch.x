include	<config.h>
include	<evexpr.h>
include	<xwhen.h>
include	"reloperr.h"

# TBL_SEARCH -- Search table for a row which makes an expression true
#
# This procedure evaluates a boolean expession for the indicated rows in a 
# table. When it finds a row which makes the expression true, it returns
# the row number. If it does not find any such row, it returns zero. If
# there is a syntax error in the expression, it returns ERR.
#
# B.Simon	13-Apr-1988	First Code
# Phil Hodge	 4-Mar-2002	Free memory allocated by evexpr.
# Phil Hodge	23-Apr-2002	Move xev_freeop and mfree.

int procedure tbl_search (tp, expr, first, last)

pointer	tp		# i: Table descriptor
char	expr[ARB]	# i: Boolean expression used in search
int	first		# i: First row to look at
int	last		# i: Last row to look at
#--
include	"tblterm.com"

int	old_handler, tbl_term_adr, tbl_func_adr
int	status, found, dir, iary, junk
pointer	sp, op, newexp
bool	done

int	locpr(), errcode()
pointer	evexpr()

extern	tbl_handler(), tbl_term(), tbl_func()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (newexp, SZ_COMMAND, TY_CHAR)

	# Convert Fortran relational operators to SPP

	call ftnexpr (expr, Memc[newexp], SZ_COMMAND)
	    
	# Set up error handler to catch arithmetic errors

	call xwhen (X_ARITH, locpr(tbl_handler), old_handler)

	table = tp
	nterm = 0
	constant = false

	tbl_term_adr = locpr (tbl_term)
	tbl_func_adr = locpr (tbl_func)

	found = 0
	done = false

	dir = sign (1, last - first)
	do iary = first, last, dir {

	    irow = iary
	    iterm = 0

	    # Execution will resume here when an arithmetic error occurs

	    call zsvjmp (jumpbuf, status)

	    if (status != OK)
		next

	    # Evaluate expression. Check if result is true

	    ifnoerr {
		op = evexpr (Memc[newexp], tbl_term_adr, tbl_func_adr)
	    } then {

		if (O_TYPE(op) != TY_BOOL) {
		    found = ERR
		    done = true
		} else if (O_VALB(op)) {
		    found = irow
		    done = true
		}
		call xev_freeop (op)
		call mfree (op, TY_STRUCT)

	    } else if (errcode() != PUTNULL) {
		# Ignore errors caused by nulls
		found = ERR
		done = true
	    }
	    if (done)
		break
	}

	# Restore old error handler

	call xwhen (X_ARITH, old_handler, junk)
	call sfree (sp)

	return (found)
end
