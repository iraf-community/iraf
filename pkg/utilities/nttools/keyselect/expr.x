include	<evexpr.h>
include	"keyselect.h"

#* HISTORY *
#* B.Simon	12-Mar-1992	Original
#* Phil Hodge	 4-Mar-2002	Free memory allocated by evexpr.

# EVAL_EXPR -- Evaluate a boolean expression using image header keywords

bool procedure eval_expr (im, expr)

pointer	im		# i: image descriptor
char	expr[ARB]	# i: boolean expression
#--
include	"keyselect.com"

pointer	op, sp, errmsg

string	badtype  "Expression is not of boolean type"
string	badname  "Warning: header keyword %s not found in %s\n"

int	errget()
pointer	evexpr(), locpr()
extern	fun_expr, var_expr

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	img = im
	iferr {
	    op = evexpr (expr, locpr(var_expr), locpr (fun_expr))

	} then {
	    if (errget(Memc[errmsg], SZ_LINE) == ERR_SYNTAX) {
		call xer_reset
		call error (ERR_SYNTAX, Memc[errmsg])

	    } else {
		call xer_reset
		call eprintf ("Warning: %s\n")
		call pargstr (Memc[errmsg])
		call mfree (op, TY_STRUCT)

		return (false)
	    }
	}

	if (O_TYPE(op) != TY_BOOL)
	    call error (ERR_SYNTAX, badtype)

	call xev_freeop (op)
	call mfree (op, TY_STRUCT)
	call sfree (sp)
	return (O_VALB(op))
end

# FMT_EXPR -- Format an expression to make it easier to parse

procedure fmt_expr (expr)

char	expr[ARB]	# i: expression
#--
int	ic, jc

begin
	# Find first non-white character

	for (ic = 1; expr[ic] != EOS; ic = ic + 1) {
	    if (expr[ic] > ' ')
		break
	}

	# Copy remaining characters, replacing newlines with blanks

	jc = 1
	for ( ; expr[ic] != EOS; ic = ic + 1) {
	    if (expr[ic] == '\n') {
		expr[jc] = ' '
	    } else if (jc < ic) {
		expr[jc] = expr[ic]
	    }
	    jc = jc + 1
	}

	expr[jc] = EOS
end

# FUN_EXPR -- Evaluate non-standard functions used in expression

procedure fun_expr (func, argptr, nargs, op)

char	func[ARB]	# i: function name
pointer	argptr[ARB]	# i: pointers to function arguments
int	nargs		# i: number of function arguments
pointer	op		# o: structure containing function value
#--
include "keyselect.com"

int	arg
pointer	sp, errmsg

string	flist   "find"
string	badfun	"Unknown function name (%s)"
string	badtyp	"Invalid argument type for %s"

int	word_match(), imaccf()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	switch (word_match (func, flist)) {
	case 0: # unrecognized function name
	    call sprintf (Memc[errmsg], SZ_LINE, badfun)
	    call pargstr (func)
	    call error (ERR_SYNTAX, Memc[errmsg])

	case 1: # find keyword in header ?
	    call xev_initop (op, 0, TY_BOOL)
	    O_VALB(op) = true

	    do arg = 1, nargs {
		if (O_TYPE(argptr[arg]) != TY_CHAR) {
		    call sprintf (Memc[errmsg], SZ_LINE, badtyp)
		    call pargstr (func)
		    call error (ERR_SYNTAX, Memc[errmsg])
		}

		if (imaccf (img, O_VALC(argptr[arg])) == NO)
		    O_VALB(op) = false
	    }
	}

	call sfree (sp)
end

# VAR_EXPR -- Retrieve keyword used in expression

procedure var_expr (name, op)

char	name[ARB]	# i: keyword name
pointer	op		# o: structure containing value of variable
#--
include	"keyselect.com"

int	ic, dtype, type, length, junk
pointer	sp, value

string	badname "Expression cannot be evaluated because keyword not found"

bool	streq()
int	ctoi(), ctor()

begin
	call smark(sp) 
	call salloc (value, SZ_BIGCOL, TY_CHAR)

	# Retrieve keyword value from image header

	call get_keyword (img, name, dtype, Memc[value], SZ_BIGCOL)

	# Allocate structure to hold value

	if (dtype == 0) {
	    call error (ERR_NOFIND, badname)
	} else if (dtype < 0) {
	    type = TY_CHAR
	    length = - dtype
	} else {
	    type = dtype
	    length = 0
	}

	call xev_initop (op, length, type)

	# Copy value to structure

	switch (type) {
	case TY_BOOL:
	    O_VALB(op) = streq (Memc[value], "yes")
	case TY_CHAR:
	    call strcpy (Memc[value], O_VALC(op), length)
	case TY_SHORT,TY_INT,TY_LONG:
	    ic = 1
	    junk = ctoi (Memc[value], ic, O_VALI(op))
	case TY_REAL,TY_DOUBLE:
	    ic = 1
	    junk = ctor (Memc[value], ic, O_VALR(op))
	}

	call sfree(sp)
end
