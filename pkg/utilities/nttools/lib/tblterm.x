include	<config.h>
include	<evexpr.h>
include	<tbset.h>
include	<xwhen.h>
include	"reloperr.h"

define	MAXTERM		64

# TBL_TERM -- Return the value of the term in the expression
#
# B.Simon	13-Apr-88	Separated from tbl_eval

procedure tbl_term (term, op)

char	term[ARB]	# i: The name of the term
pointer	op		# o: A structure holding the term value and type
#--
include	"tblterm.com"

bool	isnull
int	datalen[MAXTERM], datatype[MAXTERM], dtype
pointer	colptr[MAXTERM]
pointer	sp, errtxt 

string	badname		"Column name not found (%s)"
string	badnum		"Too many terms in expression"
string	nulvalue	"Null found in table element"

int	tbcigi()

errchk	tbcfnd, tbcigi, tbegtb, tbegtt, tbegti, tbegtr

begin
	# Allocate storage for character strings

	call smark (sp)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	constant = false
	iterm = iterm + 1
	if (iterm > MAXTERM)
	    call error (BOUNDS, badnum)

	# If this is a new term, get its column pointer, type, and length

	if (iterm > nterm) {
	    nterm = iterm
	    call tbcfnd (table, term, colptr[iterm], 1)

	    if (colptr[iterm] == NULL) {
		call sprintf (Memc[errtxt], SZ_LINE, badname)
		call pargstr (term)
		call error (SYNTAX, Memc[errtxt])
	    }

	    dtype = tbcigi (colptr[iterm], TBL_COL_DATATYPE)
	    switch (dtype) {
	    case TY_BOOL:
		datalen[iterm] = 0
		datatype[iterm] = TY_BOOL
	    case TY_CHAR:
		datalen[iterm] = 1
		datatype[iterm] = TY_CHAR
	    case TY_SHORT,TY_INT,TY_LONG:
		datalen[iterm] = 0
		datatype[iterm] = TY_INT
	    case TY_REAL,TY_DOUBLE:
		datalen[iterm] = 0
		datatype[iterm] = TY_REAL
	    default:
		datalen[iterm] = - dtype
		datatype[iterm] = TY_CHAR
	    }
	}
	    
	# Read the table to get the value of term

	call xev_initop (op, datalen[iterm], datatype[iterm])

	switch (datatype[iterm]) {
	case TY_BOOL:
	    call tbegtb (table, colptr[iterm], irow, O_VALB(op))
	    isnull = false
	case TY_CHAR:
	    call tbegtt (table, colptr[iterm], irow, O_VALC(op),
			 datalen[iterm]) 
	    isnull = O_VALC(op) == EOS
	case TY_SHORT,TY_INT,TY_LONG:
	    call tbegti (table, colptr[iterm], irow, O_VALI(op))
	    isnull = IS_INDEFI (O_VALI(op))
	case TY_REAL,TY_DOUBLE:
	    call tbegtr (table, colptr[iterm], irow, O_VALR(op))
	    isnull = IS_INDEFR (O_VALR(op))
	}	

	# Error exit if table element is null

	if (isnull)
	    call error (PUTNULL, nulvalue)

	call sfree (sp)
end

# TBL_FUNC -- Return the value of a nonstandard function in the expression

procedure tbl_func (func_name, arg_ptr, nargs, op)

char	func_name[ARB]		# i: String containing function name
pointer	arg_ptr[ARB]		# i: Pointers to function arguments
int	nargs			# i: Number of function arguments
pointer op			# o: Pointer to output structure
#--
include	"tblterm.com"

bool	valflag
int	type, iarg
pointer	sp, errtxt

string	badtyp	"Invalid argument type in %s"
string	badarg	"Incorrect number of arguments for %s"
string	badfun	"Unknown function named %s"

bool	streq()
double	mjd()

errchk	mjd()

begin
	# Allocate storage for character strings

	call smark (sp)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Call appropriate function according to name

	if (streq (func_name, "row")) {

	    # Table row number function: row()

	    constant = false
	    if (nargs != 0) {
		call sprintf (Memc[errtxt], SZ_LINE, badarg)
		call pargstr (func_name)
		call error (SYNTAX, Memc[errtxt])
	    }
	    call xev_initop (op, 0, TY_INT)
	    O_VALI(op) = irow

	} else if (streq (func_name, "delta")) {

	    # Difference between two Julian dates: mjd(date1) - mjd(date2)

	    if (nargs != 2) {
		call sprintf (Memc[errtxt], SZ_LINE, badarg)
		call pargstr (func_name)
		call error (SYNTAX, Memc[errtxt])
	    }
	    if (O_TYPE(arg_ptr[1]) != TY_CHAR ||
		O_TYPE(arg_ptr[2]) != TY_CHAR    ) {
		call sprintf (Memc[errtxt], SZ_LINE, badtyp)
		call pargstr (func_name)
		call error (SYNTAX, Memc[errtxt])
	    }
	    call xev_initop (op, 0, TY_REAL)
	    O_VALR(op) = mjd (O_VALC(arg_ptr[1])) - mjd (O_VALC(arg_ptr[2]))

	} else if (streq (func_name, "match")) {
	    if (nargs < 2) {
		call sprintf (Memc[errtxt], SZ_LINE, badarg)
		call pargstr (func_name)
		call error (SYNTAX, Memc[errtxt])
	    }

	    type = O_TYPE(arg_ptr[1])
	    do iarg = 2, nargs {
		if (type != O_TYPE(arg_ptr[iarg])) {
		    call sprintf (Memc[errtxt], SZ_LINE, badtyp)
		    call pargstr (func_name)
		    call error (SYNTAX, Memc[errtxt])
	        }
	    }

	    valflag = false
	    call xev_initop (op, 0, TY_BOOL)

	    switch (type) {
	    case TY_BOOL:
		if (O_VALB(arg_ptr[1])) {
		    do iarg = 2, nargs {
			if (O_VALB(arg_ptr[iarg])) {
			    valflag = true
			    break
			}
		    }
		} else {
		    do iarg = 2, nargs {
			if (! O_VALB(arg_ptr[iarg])) {
			    valflag = true
			    break
			}
		    }
		}
	    case TY_CHAR:
		do iarg = 2, nargs {
		    if (streq (O_VALC(arg_ptr[1]), O_VALC(arg_ptr[iarg]))) {
			valflag = true
			break
		    }
		}
	    case TY_SHORT,TY_INT,TY_LONG:
		do iarg = 2, nargs {
		    if (O_VALI(arg_ptr[1]) == O_VALI(arg_ptr[iarg])) {
			valflag = true
			break
		    }
		}
	    case TY_REAL:
		do iarg = 2, nargs {
		    if (O_VALR(arg_ptr[1]) == O_VALR(arg_ptr[iarg])) {
			valflag = true
			break
		    }
		}
	    }
	    O_VALB(op) = valflag

	} else {

	    call sprintf (Memc[errtxt], SZ_LINE, badfun)
	    call pargstr (func_name)
	    call error (SYNTAX, Memc[errtxt])

	}

	call sfree (sp)
end

# TBL_HANDLER -- Error handler to catch arithmetic errors

procedure tbl_handler (code, nxt_handler)

int	code		# i: error code which trigerred this exception
int	nxt_handler	# o: handler called after this handler exits
#--
include	"tblterm.com"

bool	junk
bool	xerpop()

begin
	# Resume execution at zsvjmp

	nxt_handler = X_IGNORE
	junk = xerpop()
	call zdojmp (jumpbuf, code)
end
