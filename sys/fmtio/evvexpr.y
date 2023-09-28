%{
include	<lexnum.h>
include	<ctype.h>
include	<mach.h>
include	<math.h>
include	<evvexpr.h>

.help evvexpr
.nf --------------------------------------------------------------------------
EVVEXPR.GY -- Generic XYacc source for a general vector expression evaluator.

	    o = evvexpr (expr, getop, getop_data, ufcn, ufcn_data, flags)
		evvfree (o)

Client callbacks:

	          getop (client_data, opname, out)
	           ufcn (client_data, fcn, args, nargs, out)

here "out" is the output operand returned to EVVEXPR.  Client_data is any
arbitrary integer or pointer value passed in to EVVEXPR when by the client
when the callback was registered.  "args" is an array of operand structs,
the arguments for the user function being called.  If the operand or
function call cannot be completed normally an error exit may be made (call
error) or an invalid operand may be returned (O_TYPE set to 0).  The client
should not free the "args" input operands, this will be handled by EVVEXPR.

Operand struct (lib$evvexpr.h):

	struct operand {
		int	O_TYPE			# operand type (bcsilrd)
		int	O_LEN			# operand length (0=scalar)
		int	O_FLAGS			# O_FREEVAL, O_FREEOP
		union {
			char*	O_VALC		# string
			short	O_VALS
			int	O_VALI		# int or bool
			long	O_VALL
			real	O_VALR
			double	O_VALD
			pointer	O_VALP		# vector data
		}
	}

The macro O_VALC references the string value of a TY_CHAR operand.  The
flags are O_FREEVAL and O_FREEOP, which tell EVVEXPR and EVVFREE whether or
not to free any vector operand array or the operand struct when the operand
is freed.  The client should set these flags on operands returned to EVVEXPR
if it wants EVVEXPR to free any operand storage.

Supported types are bool, char (string), and SILRD.  Bool is indicated as
TY_BOOL in the O_TYPE field of the operand struct, but is stored internally
as an integer and the value field of a boolean operand is given by O_VALI.

Operands may be either scalars or vectors.  A vector is indicated by a O_LEN
value greater than zero.  For vector operands O_VALP points to the data array.
A special case is TY_CHAR (string), in which case O_LEN is the allocated
length of the EOS-terminated string.  A string is logically a scalar value
even though it is physically stored in the operand as a character vector.

The trig functions operate upon angles in units of radians.  The intrinsic
functions RAD and DEG are available for converting between radians and
degrees.  A string can be coerced to a binary value and vice versa, using
the INT, STR, etc. intrinsic functions.

This is a generalization of the older EVEXPR routine, adding additional
datatypes, support for vector operands, and numerous minor enhancements.
.endhelp ---------------------------------------------------------------------

define	YYMAXDEPTH	64		# parser stack length
define	MAX_ARGS	17		# max args in a function call
define	yyparse		xvv_parse

# Arglist structure.
define	LEN_ARGSTRUCT	(1+MAX_ARGS+(MAX_ARGS*LEN_OPERAND))
define	A_NARGS		Memi[$1]	# number of arguments
define	A_ARGP		Memi[$1+$2]	# array of pointers to operand structs
define	A_OPS		($1+MAX_ARGS+1)	# offset to operand storage area

# Intrinsic functions.

define	LEN_STAB	300		# for symbol table
define	LEN_SBUF	256
define	LEN_INDEX	97

define	LEN_SYM		1		# symbol data
define	SYM_CODE	Memi[$1]

define	KEYWORDS	"|abs|acos|asin|atan|atan2|bool|cos|cosh|deg|double|\
			 |exp|hiv|int|len|log|log10|long|lov|max|mean|median|\
			 |min|mod|nint|rad|real|repl|stddev|shift|short|sin|\
			 |sinh|sort|sqrt|str|sum|tan|tanh|"

define	F_ABS		01		# function codes
define	F_ACOS		02
define	F_ASIN		03
define	F_ATAN		04
define	F_ATAN2		05
define	F_BOOL		06
define	F_COS		07
define	F_COSH		08
define	F_DEG		09		# radians to degrees
define	F_DOUBLE	10
	# newline	11
define	F_EXP		12
define	F_HIV		13		# high value
define	F_INT		14
define	F_LEN		15		# vector length
define	F_LOG		16
define	F_LOG10		17
define	F_LONG		18
define	F_LOV		19		# low value
define	F_MAX		20
define	F_MEAN		21
define	F_MEDIAN	22
	# newline	23
define	F_MIN		24
define	F_MOD		25
define	F_NINT		26
define	F_RAD		27		# degrees to radians
define	F_REAL		28
define	F_REPL		29		# replicate
define	F_STDDEV	30		# standard deviation
define	F_SHIFT		31
define	F_SHORT		32
define	F_SIN		33
	# newline	34
define	F_SINH		35
define	F_SORT		36		# sort
define	F_SQRT		37		# square root
define	F_STR		38
define	F_SUM		39
define	F_TAN 		40
define	F_TANH 		41

define	T_B		TY_BOOL
define	T_C		TY_CHAR
define	T_S		TY_SHORT
define	T_I		TY_INT
define	T_L		TY_LONG
define	T_R		TY_REAL
define	T_D		TY_DOUBLE


# EVVEXPR -- Evaluate a general mixed type vector expression.  Input consists
# of the expression to be evaluated (a string) and, optionally, user
# procedures for fetching external operands and executing external functions.
# Output is a pointer to an operand structure containing the computed value of
# the expression.  The output operand structure is dynamically allocated by
# EVVEXPR and must be freed by the user.
# 
# NOTE: this is not intended to be an especially efficient procedure.  Rather,
# this is a high level, easy to use procedure, intended to provide greater
# flexibility in the parameterization of applications programs.  The main
# inefficiency is that, since compilation and execution are not broken out as
# separate steps, when the routine is repeatedly called to evaluate the same
# expression with different data, all the compile time computation (parsing
# etc.) has to be repeated.

pointer procedure evvexpr (expr, getop, getop_data, ufcn, ufcn_data, flags)

char	expr[ARB]		#I expression to be evaluated
int	getop			#I user supplied get operand procedure
int	getop_data		#I client data for above function
int	ufcn			#I user supplied function call procedure
int	ufcn_data		#I client data for above function
int	flags			#I flag bits

int	junk
pointer	sp, ip
bool	debug, first_time
int	strlen(), xvv_parse()
pointer	xvv_loadsymbols()
extern	xvv_gettok()

errchk	xvv_parse, calloc
include	"evvexpr.com"
data	debug /false/
data	first_time /true/

begin
	call smark (sp)

	if (first_time) {
	    # This creates data which remains for the life of the process.
	    ev_st = xvv_loadsymbols (KEYWORDS)
	    first_time = false
	}

	# Set user function entry point addresses.
	ev_getop = getop
	ev_getop_data = getop_data
	ev_ufcn = ufcn
	ev_ufcn_data = ufcn_data
	ev_flags = flags

	# Allocate an operand struct for the expression value.
	call calloc (ev_oval, LEN_OPERAND, TY_STRUCT)

	# Make a local copy of the input string.
	call salloc (ip, strlen(expr), TY_CHAR)
	call strcpy (expr, Memc[ip], ARB)

	# Evaluate the expression.  The expression value is copied into the
	# output operand structure by XVV_PARSE, given the operand pointer
	# passed in common.  A common must be used since the standard parser
	# subroutine has a fixed calling sequence.

	junk = xvv_parse (ip, debug, xvv_gettok)
	O_FLAGS(ev_oval) = or (O_FLAGS(ev_oval), O_FREEOP)

	call sfree (sp)
	return (ev_oval)
end


# EVVFREE -- Free an operand struct such as is returned by EVVEXPR.

procedure evvfree (o)

pointer	o			# operand struct

begin
	call xvv_freeop (o)
end

%L
# XVV_PARSE -- SPP/Yacc parser for the evaluation of an expression passed as
# a text string.  Expression evaluation is carried out as the expression is
# parsed, rather than being broken into separate compile and execute stages.
# There is only one statement in this grammar, the expression.  Our function
# is to reduce an expression to a single value of type bool, string, int,
# or real.

pointer	ap
bool	streq()
errchk	zcall3, xvv_error1, xvv_unop, xvv_binop, xvv_boolop
errchk	xvv_quest, xvv_callfcn, xvv_addarg
include	"evvexpr.com"

%}

# The $/ following causes the generic preprocessor to pass this block of code
# through unchanged.



%token		CONSTANT IDENTIFIER NEWLINE YYEOS
%token		PLUS MINUS STAR SLASH EXPON CONCAT QUEST COLON
%token		LT GT LE GT EQ NE SE LAND LOR LNOT BAND BOR BXOR BNOT AT

%nonassoc	QUEST
%left		LAND LOR
%left		BAND BOR BXOR
%nonassoc	EQ NE SE
%nonassoc	LT GT LE GE
%left		CONCAT
%left		PLUS MINUS
%left		STAR SLASH
%right		UMINUS LNOT BNOT
%left		EXPON
%right		AT

%%

stmt	:	exprlist YYEOS {
			# Normal exit.  Move the final expression value operand
			# into the operand structure pointed to by the global
			# variable ev_oval.

			YYMOVE ($1, ev_oval)
			call sfree (yysp)
			return (OK)
		}
	|	error {
			call error (1, "syntax error")
		}
	;

exprlist:	expr {
			YYMOVE ($1, $$)
		}
	|	exprlist ',' opnl expr {
			YYMOVE ($4, $$)
			call xvv_freeop ($1)
		}


expr	:	CONSTANT {
			# Numeric constant.
			YYMOVE ($1, $$)
		    }
	|	IDENTIFIER {
			# The boolean constants "yes" and "no" are implemented
			# as reserved operands.

			call xvv_initop ($$, 0, TY_BOOL)
			if (streq (O_VALC($1), "yes")) {
			    O_VALI($$) = YES
			} else if (streq (O_VALC($1), "no")) {
			    O_VALI($$) = NO
			} else if (ev_getop != NULL) {
			    call zcall3 (ev_getop,ev_getop_data, O_VALC($1), $$)
			    if (O_TYPE($$) <= 0)
				call xvv_error1 ("unknown operand `%s'",
				    O_VALC($1))
			} else
			    call xvv_error1 ("illegal operand `%s'", O_VALC($1))
			call xvv_freeop ($1)
		    }
	|	AT CONSTANT {
			# e.g., @"param"
			if (ev_getop != NULL) {
			    call zcall3 (ev_getop,ev_getop_data, O_VALC($2), $$)
			    if (O_TYPE($$) <= 0)
				call xvv_error1 ("unknown operand `%s'",
				    O_VALC($1))
			} else
			    call xvv_error1 ("illegal operand `%s'", O_VALC($2))
			call xvv_freeop ($2)
		    }
	|	MINUS expr %prec UMINUS {
			# Unary arithmetic minus.
			call xvv_unop (MINUS, $2, $$)
		    }
	|	LNOT expr {
			# Logical not.
			call xvv_unop (LNOT, $2, $$)
		    }
	|	BNOT expr {
			# Boolean not.
			call xvv_unop (BNOT, $2, $$)
		    }
	|	expr PLUS opnl expr {
			# Addition.
			call xvv_binop (PLUS, $1, $4, $$)
		    }
	|	expr MINUS opnl expr {
			# Subtraction.
			call xvv_binop (MINUS, $1, $4, $$)
		    }
	| 	expr STAR opnl expr {
			# Multiplication.
			call xvv_binop (STAR, $1, $4, $$)
		    }
	|	expr SLASH opnl expr {
			# Division.
			call xvv_binop (SLASH, $1, $4, $$)
		    }
	|	expr EXPON opnl expr {
			# Exponentiation.
			call xvv_binop (EXPON, $1, $4, $$)
		    }
	|	expr CONCAT opnl expr {
			# Concatenate two operands.
			call xvv_binop (CONCAT, $1, $4, $$)
		    }
	|	expr LAND opnl expr {
			# Logical and.
			call xvv_boolop (LAND, $1, $4, $$)
		    }
	|	expr LOR opnl expr {
			# Logical or.
			call xvv_boolop (LOR, $1, $4, $$)
		    }
	|	expr BAND opnl expr {
			# Boolean and.
			call xvv_binop (BAND, $1, $4, $$)
		    }
	|	expr BOR opnl expr {
			# Boolean or.
			call xvv_binop (BOR, $1, $4, $$)
		    }
	|	expr BXOR opnl expr {
			# Boolean xor.
			call xvv_binop (BXOR, $1, $4, $$)
		    }
	|	expr LT opnl expr {
			# Boolean less than.
			call xvv_boolop (LT, $1, $4, $$)
		    }
	|	expr GT opnl expr {
			# Boolean greater than.
			call xvv_boolop (GT, $1, $4, $$)
		    }
	|	expr LE opnl expr {
			# Boolean less than or equal.
			call xvv_boolop (LE, $1, $4, $$)
		    }
	|	expr GE opnl expr {
			# Boolean greater than or equal.
			call xvv_boolop (GE, $1, $4, $$)
		    }
	|	expr EQ opnl expr {
			# Boolean equal.
			call xvv_boolop (EQ, $1, $4, $$)
		    }
	|	expr SE opnl expr {
			# String pattern-equal.
			call xvv_boolop (SE, $1, $4, $$)
		    }
	|	expr NE opnl expr {
			# Boolean not equal.
			call xvv_boolop (NE, $1, $4, $$)
		    }
	|	expr QUEST opnl expr COLON opnl expr {
			# Conditional expression.
			call xvv_quest ($1, $4, $7, $$)
		    }
	|	funct '(' arglist ')' {
			# Call an intrinsic or external function.
			ap = O_VALP($3)
			call xvv_callfcn (O_VALC($1),
			    A_ARGP(ap,1), A_NARGS(ap), $$)
			call xvv_freeop ($1)
			call xvv_freeop ($3)
		    }
	|	'(' expr ')' {
			YYMOVE ($2, $$)
		    }
	;


funct	:	IDENTIFIER {
			YYMOVE ($1, $$)
		    }
	|	CONSTANT {
			if (O_TYPE($1) != TY_CHAR)
			    call error (1, "illegal function name")
			YYMOVE ($1, $$)
		    }
	;


arglist	:	{
			# Empty.
			call xvv_startarglist (NULL, $$)
		    }
	|	expr {
			# First arg; start a nonnull list.
			call xvv_startarglist ($1, $$)
		    }
	|	arglist ',' opnl expr {
			# Add an argument to an existing list.
			call xvv_addarg ($4, $1, $$)
		    }
	;


opnl	:	# Empty.
	|	opnl NEWLINE
	;

%%

# End generic preprocessor escape.



# XVV_UNOP -- Unary operation.  Perform the indicated unary operation on the
# input operand, returning the result as the output operand.

procedure xvv_unop (opcode, in, out)

int	opcode			#I operation to be performed
pointer	in			#I input operand
pointer	out			#I output operand

short	val_s
long	val_l
int	val_i, nelem
errchk	xvv_error, xvv_initop
string	s_badswitch "unop: bad switch"

begin
	nelem = O_LEN(in)

	switch (opcode) {
	case MINUS:
	    # Unary negation.
	    call xvv_initop (out, nelem, O_TYPE(in))
	    switch (O_TYPE(in)) {
	    case TY_BOOL, TY_CHAR:
		call xvv_error ("negation of a nonarithmetic operand")

	    case TY_SHORT:
		if (nelem > 0)
		    call anegs (Mems[O_VALP(in)], Mems[O_VALP(out)], nelem)
		else
		    O_VALS(out) = -O_VALS(in)

	    case TY_INT:
		if (nelem > 0)
		    call anegi (Memi[O_VALP(in)], Memi[O_VALP(out)], nelem)
		else
		    O_VALI(out) = -O_VALI(in)

	    case TY_LONG:
		if (nelem > 0)
		    call anegl (Meml[O_VALP(in)], Meml[O_VALP(out)], nelem)
		else
		    O_VALL(out) = -O_VALL(in)

	    case TY_REAL:
		if (nelem > 0)
		    call anegr (Memr[O_VALP(in)], Memr[O_VALP(out)], nelem)
		else
		    O_VALR(out) = -O_VALR(in)

	    case TY_DOUBLE:
		if (nelem > 0)
		    call anegd (Memd[O_VALP(in)], Memd[O_VALP(out)], nelem)
		else
		    O_VALD(out) = -O_VALD(in)

	    default:
		call xvv_error (s_badswitch)
	    }

	case LNOT:
	    # Logical NOT.

	    call xvv_initop (out, nelem, TY_BOOL)
	    switch (O_TYPE(in)) {
	    case TY_BOOL:
		if (nelem > 0)
		    call abeqki (Memi[O_VALP(in)], NO, Memi[O_VALP(out)], nelem)
		else {
		    if (O_VALI(in) == NO)
			O_VALI(out) = YES
		    else
			O_VALI(out) = NO
		}

	    case TY_SHORT:
		if (nelem > 0) {
		    val_s = NO
		    call abeqks (Mems[O_VALP(in)], val_s, Memi[O_VALP(out)],
			nelem)
		} else {
		    if (O_VALS(in) == NO)
			O_VALS(out) = YES
		    else
			O_VALS(out) = NO
		}

	    case TY_INT:
		if (nelem > 0) {
		    val_i = NO
		    call abeqki (Memi[O_VALP(in)], val_i, Memi[O_VALP(out)],
			nelem)
		} else {
		    if (O_VALI(in) == NO)
			O_VALI(out) = YES
		    else
			O_VALI(out) = NO
		}

	    case TY_LONG:
		if (nelem > 0) {
		    val_l = NO
		    call abeqkl (Meml[O_VALP(in)], val_l, Memi[O_VALP(out)],
			nelem)
		} else {
		    if (O_VALL(in) == NO)
			O_VALL(out) = YES
		    else
			O_VALL(out) = NO
		}

	    case TY_CHAR, TY_REAL, TY_DOUBLE:
		call xvv_error ("not of a nonlogical")
	    default:
		call xvv_error (s_badswitch)
	    }

	case BNOT:
	    # Bitwise boolean NOT.

	    call xvv_initop (out, nelem, O_TYPE(in))
	    switch (O_TYPE(in)) {
	    case TY_BOOL, TY_CHAR, TY_REAL, TY_DOUBLE:
		call xvv_error ("boolean not of a noninteger operand")

	    case TY_SHORT:
		if (nelem > 0)
		    call anots (Mems[O_VALP(in)], Mems[O_VALP(out)], nelem)
		else
		    O_VALS(out) = not(O_VALS(in))

	    case TY_INT:
		if (nelem > 0)
		    call anoti (Memi[O_VALP(in)], Memi[O_VALP(out)], nelem)
		else
		    O_VALI(out) = not(O_VALI(in))

	    case TY_LONG:
		if (nelem > 0)
		    call anotl (Meml[O_VALP(in)], Meml[O_VALP(out)], nelem)
		else
		    O_VALL(out) = not(O_VALL(in))

	    default:
		call xvv_error (s_badswitch)
	    }

	default:
	    call xvv_error (s_badswitch)
	}

	call xvv_freeop (in)
end


# XVV_BINOP -- Binary operation.  Perform the indicated arithmetic binary
# operation on the two input operands, returning the result as the output
# operand.

procedure xvv_binop (opcode, in1, in2, out)

int	opcode			#I operation to be performed
pointer	in1, in2		#I input operands
pointer	out			#I output operand


short	v_s
short	xvv_nulls()
extern	xvv_nulls()

int	v_i
int	xvv_nulli()
extern	xvv_nulli()

long	v_l
long	xvv_nulll()
extern	xvv_nulll()

real	v_r
real	xvv_nullr()
extern	xvv_nullr()

double	v_d
double	xvv_nulld()
extern	xvv_nulld()

pointer	sp, otemp, p1, p2, po
int	dtype, nelem, len1, len2
include	"evvexpr.com"

int	xvv_newtype(), strlen()
errchk	xvv_newtype, xvv_initop, xvv_chtype, xvv_error
string	s_badswitch "binop: bad case in switch"
string	s_boolop "binop: bitwise boolean operands must be an integer type"
define	done_ 91

begin
	# Set the datatype of the output operand, taking an error action if
	# the operands have incompatible datatypes.

	dtype = xvv_newtype (O_TYPE(in1), O_TYPE(in2))

	# Compute the size of the output operand.  If both input operands are
	# vectors the length of the output vector is the shorter of the two.

	switch (dtype) {
	case TY_BOOL:
	    call xvv_error ("binop: operation illegal for boolean operands")
	case TY_CHAR:
	    nelem = strlen (O_VALC(in1)) + strlen (O_VALC(in2))
	default:
	    if (opcode == CONCAT)
		nelem = max (1, O_LEN(in1)) + max (1, O_LEN(in2))
	    else {
		if (O_LEN(in1) > 0 && O_LEN(in2) > 0)
		    nelem = min (O_LEN(in1), O_LEN(in2))
		else if (O_LEN(in1) > 0)
		    nelem = O_LEN(in1)
		else if (O_LEN(in2) > 0)
		    nelem = O_LEN(in2)
		else
		    nelem = 0
	    }
	}

	# Convert input operands to desired type.
	if (O_TYPE(in1) != dtype)
	    call xvv_chtype (in1, in1, dtype)
	if (O_TYPE(in2) != dtype)
	    call xvv_chtype (in2, in2, dtype)

	# If this is a scalar/vector operation make sure the vector is the
	# first operand.

	len1 = O_LEN(in1)
	len2 = O_LEN(in2)

	if (len1 == 0 && len2 > 0) {
	    switch (opcode) {
	    case PLUS:
		# Swap operands.
		call smark (sp)
		call salloc (otemp, LEN_OPERAND, TY_STRUCT)
		YYMOVE (in1, otemp)
		YYMOVE (in2, in1)
		YYMOVE (otemp, in2)
		call sfree (sp)

	    case CONCAT:
		; # Do nothing

	    default:
		# Promote operand to a constant vector.  Inefficient, but
		# better than aborting.

		switch (dtype) {

		case TY_SHORT:
		    v_s = O_VALS(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovks (v_s, Mems[O_VALP(in1)], nelem)

		case TY_INT:
		    v_i = O_VALI(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovki (v_i, Memi[O_VALP(in1)], nelem)

		case TY_LONG:
		    v_l = O_VALL(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovkl (v_l, Meml[O_VALP(in1)], nelem)

		case TY_REAL:
		    v_r = O_VALR(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovkr (v_r, Memr[O_VALP(in1)], nelem)

		case TY_DOUBLE:
		    v_d = O_VALD(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovkd (v_d, Memd[O_VALP(in1)], nelem)

		}
	    }

	    len1 = O_LEN(in1)
	    len2 = O_LEN(in2)
	}

	# Initialize the output operand.
	call xvv_initop (out, nelem, dtype)

	p1 = O_VALP(in1)
	p2 = O_VALP(in2)
	po = O_VALP(out)

	# The bitwise boolean binary operators a special case since only the
	# integer datatypes are permitted.  Otherwise the bitwise booleans
	# are just like arithmetic booleans.

	if (opcode == BAND || opcode == BOR || opcode == BXOR) {
	    switch (dtype) {

	    case TY_SHORT:
		switch (opcode) {
		case BAND:
		    if (len1 <= 0) {
			O_VALS(out) = and (O_VALS(in1), O_VALS(in2))
		    } else if (len2 <= 0) {
			call aandks (Mems[p1], O_VALS(in2),
			    Mems[po], nelem)
		    } else {
			call aands (Mems[p1], Mems[p2],
			    Mems[po], nelem)
		    }
		case BOR:
		    if (len1 <= 0) {
			O_VALS(out) = or (O_VALS(in1), O_VALS(in2))
		    } else if (len2 <= 0) {
			call aborks (Mems[p1], O_VALS(in2),
			    Mems[po], nelem)
		    } else {
			call abors (Mems[p1], Mems[p2],
			    Mems[po], nelem)
		    }
		case BXOR:
		    if (len1 <= 0) {
			O_VALS(out) = xor (O_VALS(in1), O_VALS(in2))
		    } else if (len2 <= 0) {
			call axorks (Mems[p1], O_VALS(in2),
			    Mems[po], nelem)
		    } else {
			call axors (Mems[p1], Mems[p2],
			    Mems[po], nelem)
		    }
		}

	    case TY_INT:
		switch (opcode) {
		case BAND:
		    if (len1 <= 0) {
			O_VALI(out) = and (O_VALI(in1), O_VALI(in2))
		    } else if (len2 <= 0) {
			call aandki (Memi[p1], O_VALI(in2),
			    Memi[po], nelem)
		    } else {
			call aandi (Memi[p1], Memi[p2],
			    Memi[po], nelem)
		    }
		case BOR:
		    if (len1 <= 0) {
			O_VALI(out) = or (O_VALI(in1), O_VALI(in2))
		    } else if (len2 <= 0) {
			call aborki (Memi[p1], O_VALI(in2),
			    Memi[po], nelem)
		    } else {
			call abori (Memi[p1], Memi[p2],
			    Memi[po], nelem)
		    }
		case BXOR:
		    if (len1 <= 0) {
			O_VALI(out) = xor (O_VALI(in1), O_VALI(in2))
		    } else if (len2 <= 0) {
			call axorki (Memi[p1], O_VALI(in2),
			    Memi[po], nelem)
		    } else {
			call axori (Memi[p1], Memi[p2],
			    Memi[po], nelem)
		    }
		}

	    case TY_LONG:
		switch (opcode) {
		case BAND:
		    if (len1 <= 0) {
			O_VALL(out) = and (O_VALL(in1), O_VALL(in2))
		    } else if (len2 <= 0) {
			call aandkl (Meml[p1], O_VALL(in2),
			    Meml[po], nelem)
		    } else {
			call aandl (Meml[p1], Meml[p2],
			    Meml[po], nelem)
		    }
		case BOR:
		    if (len1 <= 0) {
			O_VALL(out) = or (O_VALL(in1), O_VALL(in2))
		    } else if (len2 <= 0) {
			call aborkl (Meml[p1], O_VALL(in2),
			    Meml[po], nelem)
		    } else {
			call aborl (Meml[p1], Meml[p2],
			    Meml[po], nelem)
		    }
		case BXOR:
		    if (len1 <= 0) {
			O_VALL(out) = xor (O_VALL(in1), O_VALL(in2))
		    } else if (len2 <= 0) {
			call axorkl (Meml[p1], O_VALL(in2),
			    Meml[po], nelem)
		    } else {
			call axorl (Meml[p1], Meml[p2],
			    Meml[po], nelem)
		    }
		}

	    default:
		call xvv_error (s_boolop)
	    }

	    goto done_
	}

	# Perform an arithmetic binary operation.
	switch (dtype) {
	case TY_CHAR:
	    switch (opcode) {
	    case CONCAT:
		call strcpy (O_VALC(in1), O_VALC(out), ARB)
		call strcat (O_VALC(in2), O_VALC(out), ARB)
	    default:
		call xvv_error ("binop: operation illegal for string operands")
	    }

	case TY_SHORT:
	    switch (opcode) {
	    case PLUS:
		if (len1 <= 0) {
		    O_VALS(out) = O_VALS(in1) + O_VALS(in2)
		} else if (len2 <= 0) {
		    call aaddks (Mems[p1], O_VALS(in2),
			Mems[po], nelem)
		} else {
		    call aadds (Mems[p1], Mems[p2],
			Mems[po], nelem)
		}
	    case MINUS:
		if (len1 <= 0)
		    O_VALS(out) = O_VALS(in1) - O_VALS(in2)
		else if (len2 <= 0)
		    call asubks (Mems[p1], O_VALS(in2), Mems[po], nelem)
		else
		    call asubs (Mems[p1], Mems[p2], Mems[po], nelem)

	    case STAR:
		if (len1 <= 0)
		    O_VALS(out) = O_VALS(in1) * O_VALS(in2)
		else if (len2 <= 0)
		    call amulks (Mems[p1], O_VALS(in2), Mems[po], nelem)
		else
		    call amuls (Mems[p1], Mems[p2], Mems[po], nelem)

	    case SLASH:
		if (and (ev_flags, EV_RNGCHK) == 0) {
		    # No range checking.
		    if (len1 <= 0)
			O_VALS(out) = O_VALS(in1) / O_VALS(in2)
		    else if (len2 <= 0)
			call adivks (Mems[p1], O_VALS(in2), Mems[po], nelem)
		    else
			call adivs (Mems[p1], Mems[p2], Mems[po], nelem)
		} else {
		    # Check for divide by zero.
		    if (len1 <= 0) {
			if (O_VALS(in2) == 0)
			    O_VALS(out) = xvv_nulls(0)
			else
			    O_VALS(out) = O_VALS(in1) / O_VALS(in2)
		    } else if (len2 <= 0) {
			if (O_VALS(in2) == 0)
			    call amovks (xvv_nulls(0), Mems[po], nelem)
			else {
			    call adivks (Mems[p1], O_VALS(in2), Mems[po],
				nelem)
			}
		    } else {
			call advzs (Mems[p1], Mems[p2], Mems[po], nelem,
			    xvv_nulls)
		    }
		}
	    case EXPON:
		if (len1 <= 0)
		    O_VALS(out) = O_VALS(in1) ** O_VALS(in2)
		else if (len2 <= 0)
		    call aexpks (Mems[p1], O_VALS(in2), Mems[po], nelem)
		else
		    call aexps (Mems[p1], Mems[p2], Mems[po], nelem)

	    case CONCAT:
		# Concatenate two numeric operands.
		if (len1 <= 0) {
		    Mems[po] = O_VALS(in1)
		    po = po + 1
		} else {
		    call amovs (Mems[p1], Mems[po], len1)
		    po = po + len1
		}
		if (len2 <= 0)
		    Mems[po] = O_VALS(in2)
		else
		    call amovs (Mems[p2], Mems[po], len2)

	    default:
		call xvv_error (s_badswitch)
	    }

	case TY_INT:
	    switch (opcode) {
	    case PLUS:
		if (len1 <= 0) {
		    O_VALI(out) = O_VALI(in1) + O_VALI(in2)
		} else if (len2 <= 0) {
		    call aaddki (Memi[p1], O_VALI(in2),
			Memi[po], nelem)
		} else {
		    call aaddi (Memi[p1], Memi[p2],
			Memi[po], nelem)
		}
	    case MINUS:
		if (len1 <= 0)
		    O_VALI(out) = O_VALI(in1) - O_VALI(in2)
		else if (len2 <= 0)
		    call asubki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		else
		    call asubi (Memi[p1], Memi[p2], Memi[po], nelem)

	    case STAR:
		if (len1 <= 0)
		    O_VALI(out) = O_VALI(in1) * O_VALI(in2)
		else if (len2 <= 0)
		    call amulki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		else
		    call amuli (Memi[p1], Memi[p2], Memi[po], nelem)

	    case SLASH:
		if (and (ev_flags, EV_RNGCHK) == 0) {
		    # No range checking.
		    if (len1 <= 0)
			O_VALI(out) = O_VALI(in1) / O_VALI(in2)
		    else if (len2 <= 0)
			call adivki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    else
			call adivi (Memi[p1], Memi[p2], Memi[po], nelem)
		} else {
		    # Check for divide by zero.
		    if (len1 <= 0) {
			if (O_VALI(in2) == 0)
			    O_VALI(out) = xvv_nulli(0)
			else
			    O_VALI(out) = O_VALI(in1) / O_VALI(in2)
		    } else if (len2 <= 0) {
			if (O_VALI(in2) == 0)
			    call amovki (xvv_nulli(0), Memi[po], nelem)
			else {
			    call adivki (Memi[p1], O_VALI(in2), Memi[po],
				nelem)
			}
		    } else {
			call advzi (Memi[p1], Memi[p2], Memi[po], nelem,
			    xvv_nulli)
		    }
		}
	    case EXPON:
		if (len1 <= 0)
		    O_VALI(out) = O_VALI(in1) ** O_VALI(in2)
		else if (len2 <= 0)
		    call aexpki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		else
		    call aexpi (Memi[p1], Memi[p2], Memi[po], nelem)

	    case CONCAT:
		# Concatenate two numeric operands.
		if (len1 <= 0) {
		    Memi[po] = O_VALI(in1)
		    po = po + 1
		} else {
		    call amovi (Memi[p1], Memi[po], len1)
		    po = po + len1
		}
		if (len2 <= 0)
		    Memi[po] = O_VALI(in2)
		else
		    call amovi (Memi[p2], Memi[po], len2)

	    default:
		call xvv_error (s_badswitch)
	    }

	case TY_LONG:
	    switch (opcode) {
	    case PLUS:
		if (len1 <= 0) {
		    O_VALL(out) = O_VALL(in1) + O_VALL(in2)
		} else if (len2 <= 0) {
		    call aaddkl (Meml[p1], O_VALL(in2),
			Meml[po], nelem)
		} else {
		    call aaddl (Meml[p1], Meml[p2],
			Meml[po], nelem)
		}
	    case MINUS:
		if (len1 <= 0)
		    O_VALL(out) = O_VALL(in1) - O_VALL(in2)
		else if (len2 <= 0)
		    call asubkl (Meml[p1], O_VALL(in2), Meml[po], nelem)
		else
		    call asubl (Meml[p1], Meml[p2], Meml[po], nelem)

	    case STAR:
		if (len1 <= 0)
		    O_VALL(out) = O_VALL(in1) * O_VALL(in2)
		else if (len2 <= 0)
		    call amulkl (Meml[p1], O_VALL(in2), Meml[po], nelem)
		else
		    call amull (Meml[p1], Meml[p2], Meml[po], nelem)

	    case SLASH:
		if (and (ev_flags, EV_RNGCHK) == 0) {
		    # No range checking.
		    if (len1 <= 0)
			O_VALL(out) = O_VALL(in1) / O_VALL(in2)
		    else if (len2 <= 0)
			call adivkl (Meml[p1], O_VALL(in2), Meml[po], nelem)
		    else
			call adivl (Meml[p1], Meml[p2], Meml[po], nelem)
		} else {
		    # Check for divide by zero.
		    if (len1 <= 0) {
			if (O_VALL(in2) == 0)
			    O_VALL(out) = xvv_nulll(0)
			else
			    O_VALL(out) = O_VALL(in1) / O_VALL(in2)
		    } else if (len2 <= 0) {
			if (O_VALL(in2) == 0)
			    call amovkl (xvv_nulll(0), Meml[po], nelem)
			else {
			    call adivkl (Meml[p1], O_VALL(in2), Meml[po],
				nelem)
			}
		    } else {
			call advzl (Meml[p1], Meml[p2], Meml[po], nelem,
			    xvv_nulll)
		    }
		}
	    case EXPON:
		if (len1 <= 0)
		    O_VALL(out) = O_VALL(in1) ** O_VALL(in2)
		else if (len2 <= 0)
		    call aexpkl (Meml[p1], O_VALL(in2), Meml[po], nelem)
		else
		    call aexpl (Meml[p1], Meml[p2], Meml[po], nelem)

	    case CONCAT:
		# Concatenate two numeric operands.
		if (len1 <= 0) {
		    Meml[po] = O_VALL(in1)
		    po = po + 1
		} else {
		    call amovl (Meml[p1], Meml[po], len1)
		    po = po + len1
		}
		if (len2 <= 0)
		    Meml[po] = O_VALL(in2)
		else
		    call amovl (Meml[p2], Meml[po], len2)

	    default:
		call xvv_error (s_badswitch)
	    }

	case TY_REAL:
	    switch (opcode) {
	    case PLUS:
		if (len1 <= 0) {
		    O_VALR(out) = O_VALR(in1) + O_VALR(in2)
		} else if (len2 <= 0) {
		    call aaddkr (Memr[p1], O_VALR(in2),
			Memr[po], nelem)
		} else {
		    call aaddr (Memr[p1], Memr[p2],
			Memr[po], nelem)
		}
	    case MINUS:
		if (len1 <= 0)
		    O_VALR(out) = O_VALR(in1) - O_VALR(in2)
		else if (len2 <= 0)
		    call asubkr (Memr[p1], O_VALR(in2), Memr[po], nelem)
		else
		    call asubr (Memr[p1], Memr[p2], Memr[po], nelem)

	    case STAR:
		if (len1 <= 0)
		    O_VALR(out) = O_VALR(in1) * O_VALR(in2)
		else if (len2 <= 0)
		    call amulkr (Memr[p1], O_VALR(in2), Memr[po], nelem)
		else
		    call amulr (Memr[p1], Memr[p2], Memr[po], nelem)

	    case SLASH:
		if (and (ev_flags, EV_RNGCHK) == 0) {
		    # No range checking.
		    if (len1 <= 0)
			O_VALR(out) = O_VALR(in1) / O_VALR(in2)
		    else if (len2 <= 0)
			call adivkr (Memr[p1], O_VALR(in2), Memr[po], nelem)
		    else
			call adivr (Memr[p1], Memr[p2], Memr[po], nelem)
		} else {
		    # Check for divide by zero.
		    if (len1 <= 0) {
			if (O_VALR(in2) == 0.0)
			    O_VALR(out) = xvv_nullr(0.0)
			else
			    O_VALR(out) = O_VALR(in1) / O_VALR(in2)
		    } else if (len2 <= 0) {
			if (O_VALR(in2) == 0.0)
			    call amovkr (xvv_nullr(0.0), Memr[po], nelem)
			else {
			    call adivkr (Memr[p1], O_VALR(in2), Memr[po],
				nelem)
			}
		    } else {
			call advzr (Memr[p1], Memr[p2], Memr[po], nelem,
			    xvv_nullr)
		    }
		}
	    case EXPON:
		if (len1 <= 0)
		    O_VALR(out) = O_VALR(in1) ** O_VALR(in2)
		else if (len2 <= 0)
		    call aexpkr (Memr[p1], O_VALR(in2), Memr[po], nelem)
		else
		    call aexpr (Memr[p1], Memr[p2], Memr[po], nelem)

	    case CONCAT:
		# Concatenate two numeric operands.
		if (len1 <= 0) {
		    Memr[po] = O_VALR(in1)
		    po = po + 1
		} else {
		    call amovr (Memr[p1], Memr[po], len1)
		    po = po + len1
		}
		if (len2 <= 0)
		    Memr[po] = O_VALR(in2)
		else
		    call amovr (Memr[p2], Memr[po], len2)

	    default:
		call xvv_error (s_badswitch)
	    }

	case TY_DOUBLE:
	    switch (opcode) {
	    case PLUS:
		if (len1 <= 0) {
		    O_VALD(out) = O_VALD(in1) + O_VALD(in2)
		} else if (len2 <= 0) {
		    call aaddkd (Memd[p1], O_VALD(in2),
			Memd[po], nelem)
		} else {
		    call aaddd (Memd[p1], Memd[p2],
			Memd[po], nelem)
		}
	    case MINUS:
		if (len1 <= 0)
		    O_VALD(out) = O_VALD(in1) - O_VALD(in2)
		else if (len2 <= 0)
		    call asubkd (Memd[p1], O_VALD(in2), Memd[po], nelem)
		else
		    call asubd (Memd[p1], Memd[p2], Memd[po], nelem)

	    case STAR:
		if (len1 <= 0)
		    O_VALD(out) = O_VALD(in1) * O_VALD(in2)
		else if (len2 <= 0)
		    call amulkd (Memd[p1], O_VALD(in2), Memd[po], nelem)
		else
		    call amuld (Memd[p1], Memd[p2], Memd[po], nelem)

	    case SLASH:
		if (and (ev_flags, EV_RNGCHK) == 0) {
		    # No range checking.
		    if (len1 <= 0)
			O_VALD(out) = O_VALD(in1) / O_VALD(in2)
		    else if (len2 <= 0)
			call adivkd (Memd[p1], O_VALD(in2), Memd[po], nelem)
		    else
			call adivd (Memd[p1], Memd[p2], Memd[po], nelem)
		} else {
		    # Check for divide by zero.
		    if (len1 <= 0) {
			if (O_VALD(in2) == 0.0D0)
			    O_VALD(out) = xvv_nulld(0.0D0)
			else
			    O_VALD(out) = O_VALD(in1) / O_VALD(in2)
		    } else if (len2 <= 0) {
			if (O_VALD(in2) == 0.0D0)
			    call amovkd (xvv_nulld(0.0D0), Memd[po], nelem)
			else {
			    call adivkd (Memd[p1], O_VALD(in2), Memd[po],
				nelem)
			}
		    } else {
			call advzd (Memd[p1], Memd[p2], Memd[po], nelem,
			    xvv_nulld)
		    }
		}
	    case EXPON:
		if (len1 <= 0)
		    O_VALD(out) = O_VALD(in1) ** O_VALD(in2)
		else if (len2 <= 0)
		    call aexpkd (Memd[p1], O_VALD(in2), Memd[po], nelem)
		else
		    call aexpd (Memd[p1], Memd[p2], Memd[po], nelem)

	    case CONCAT:
		# Concatenate two numeric operands.
		if (len1 <= 0) {
		    Memd[po] = O_VALD(in1)
		    po = po + 1
		} else {
		    call amovd (Memd[p1], Memd[po], len1)
		    po = po + len1
		}
		if (len2 <= 0)
		    Memd[po] = O_VALD(in2)
		else
		    call amovd (Memd[p2], Memd[po], len2)

	    default:
		call xvv_error (s_badswitch)
	    }

	default:
	    call xvv_error (s_badswitch)
	}
done_
	# Free any storage in input operands.
	call xvv_freeop (in1)
	call xvv_freeop (in2)
end


# XVV_BOOLOP -- Boolean (actually logical) binary operations.  Perform the
# indicated logical operation on the two input operands, returning the result
# as the output operand.  The opcodes implemented by this routine are
# characterized by the fact that they all return a logical result (YES or NO
# physically expressed as an integer).

procedure xvv_boolop (opcode, in1, in2, out)

int	opcode			#I operation to be performed
pointer	in1, in2		#I input operands
pointer	out			#I output operand


short	v_s

int	v_i

long	v_l

real	v_r

double	v_d

pointer	sp, otemp, p1, p2, po
int	dtype, nelem, len1, len2
int	xvv_newtype(), xvv_patmatch(), strncmp(), btoi()
errchk	xvv_newtype, xvv_initop, xvv_chtype, xvv_error
string	s_badop "boolop: illegal operation"
string	s_badswitch "boolop: illegal switch"

begin
	# Boolean operands are treated as integer within this routine.
	if (O_TYPE(in1) == TY_BOOL)
	    O_TYPE(in1) = TY_INT
	if (O_TYPE(in2) == TY_BOOL)
	    O_TYPE(in2) = TY_INT

	# Determine the computation type for the operation, i.e., the type
	# both input operands must have.  This is not the same as the type
	# of the output operand, which is always boolean for the operations
	# implemented by this routine.

	dtype = xvv_newtype (O_TYPE(in1), O_TYPE(in2))

	# Compute the size of the output operand.  If both input operands are
	# vectors the length of the output vector is the shorter of the two.

	if (dtype == TY_CHAR)
	    nelem = 0
	else {
	    if (O_LEN(in1) > 0 && O_LEN(in2) > 0)
		nelem = min (O_LEN(in1), O_LEN(in2))
	    else if (O_LEN(in1) > 0)
		nelem = O_LEN(in1)
	    else if (O_LEN(in2) > 0)
		nelem = O_LEN(in2)
	    else
		nelem = 0
	}

	# Convert input operands to desired computation type.
	if (O_TYPE(in1) != dtype)
	    call xvv_chtype (in1, in1, dtype)
	if (O_TYPE(in2) != dtype)
	    call xvv_chtype (in2, in2, dtype)

	# If this is a scalar/vector operation make sure the vector is the
	# first operand.

	len1 = O_LEN(in1)
	len2 = O_LEN(in2)

	if (len1 == 0 && len2 > 0) {
	    switch (opcode) {
	    case EQ, NE:
		call smark (sp)
		call salloc (otemp, LEN_OPERAND, TY_STRUCT)
		YYMOVE (in1, otemp)
		YYMOVE (in2, in1)
		YYMOVE (otemp, in2)
		call sfree (sp)
	    default:
		# Promote operand to a constant vector.  Inefficient, but
		# better than aborting.

		switch (dtype) {

		case TY_SHORT:
		    v_s = O_VALS(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovks (v_s, Mems[O_VALP(in1)], nelem)

		case TY_INT:
		    v_i = O_VALI(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovki (v_i, Memi[O_VALP(in1)], nelem)

		case TY_LONG:
		    v_l = O_VALL(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovkl (v_l, Meml[O_VALP(in1)], nelem)

		case TY_REAL:
		    v_r = O_VALR(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovkr (v_r, Memr[O_VALP(in1)], nelem)

		case TY_DOUBLE:
		    v_d = O_VALD(in1)
		    call xvv_initop (in1, nelem, dtype)
		    call amovkd (v_d, Memd[O_VALP(in1)], nelem)

		}
	    }

	    len1 = O_LEN(in1)
	    len2 = O_LEN(in2)
	}

	# Initialize the output operand.
	call xvv_initop (out, nelem, TY_BOOL)

	p1 = O_VALP(in1)
	p2 = O_VALP(in2)
	po = O_VALP(out)

	# Perform the operation.
	if (dtype == TY_CHAR) {
	    # Character data is a special case.

	    switch (opcode) {
	    case SE:
		O_VALI(out) = btoi(xvv_patmatch (O_VALC(in1), O_VALC(in2)) > 0)
	    case LT:
		O_VALI(out) = btoi(strncmp (O_VALC(in1), O_VALC(in2), ARB) < 0)
	    case LE:
		O_VALI(out) = btoi(strncmp (O_VALC(in1), O_VALC(in2), ARB) <= 0)
	    case GT:
		O_VALI(out) = btoi(strncmp (O_VALC(in1), O_VALC(in2), ARB) > 0)
	    case GE:
		O_VALI(out) = btoi(strncmp (O_VALC(in1), O_VALC(in2), ARB) >= 0)
	    case EQ:
		O_VALI(out) = btoi(strncmp (O_VALC(in1), O_VALC(in2), ARB) == 0)
	    case NE:
		O_VALI(out) = btoi(strncmp (O_VALC(in1), O_VALC(in2), ARB) != 0)
	    default:
		call xvv_error (s_badop)
	    }

	} else if (opcode == LAND || opcode == LOR) {
	    # Operations supporting only the integer types.

	    switch (dtype) {

	    case TY_SHORT:
		switch (opcode) {
		case LAND:
		    if (len1 <= 0) {
			O_VALI(out) =
			    btoi (O_VALS(in1) != 0 && O_VALS(in2) != 0)
		    } else if (len2 <= 0) {
			call alanks (Mems[p1], O_VALS(in2), Memi[po], nelem)
		    } else
			call alans (Mems[p1], Mems[p2], Memi[po], nelem)
		case LOR:
		    if (len1 <= 0) {
			O_VALI(out) =
			    btoi (O_VALS(in1) != 0 || O_VALS(in2) != 0)
		    } else if (len2 <= 0) {
			call alorks (Mems[p1], O_VALS(in2), Memi[po], nelem)
		    } else
			call alors (Mems[p1], Mems[p2], Memi[po], nelem)
		default:
		    call xvv_error (s_badop)
		}

	    case TY_INT:
		switch (opcode) {
		case LAND:
		    if (len1 <= 0) {
			O_VALI(out) =
			    btoi (O_VALI(in1) != 0 && O_VALI(in2) != 0)
		    } else if (len2 <= 0) {
			call alanki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    } else
			call alani (Memi[p1], Memi[p2], Memi[po], nelem)
		case LOR:
		    if (len1 <= 0) {
			O_VALI(out) =
			    btoi (O_VALI(in1) != 0 || O_VALI(in2) != 0)
		    } else if (len2 <= 0) {
			call alorki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    } else
			call alori (Memi[p1], Memi[p2], Memi[po], nelem)
		default:
		    call xvv_error (s_badop)
		}

	    case TY_LONG:
		switch (opcode) {
		case LAND:
		    if (len1 <= 0) {
			O_VALI(out) =
			    btoi (O_VALL(in1) != 0 && O_VALL(in2) != 0)
		    } else if (len2 <= 0) {
			call alankl (Meml[p1], O_VALL(in2), Memi[po], nelem)
		    } else
			call alanl (Meml[p1], Meml[p2], Memi[po], nelem)
		case LOR:
		    if (len1 <= 0) {
			O_VALI(out) =
			    btoi (O_VALL(in1) != 0 || O_VALL(in2) != 0)
		    } else if (len2 <= 0) {
			call alorkl (Meml[p1], O_VALL(in2), Memi[po], nelem)
		    } else
			call alorl (Meml[p1], Meml[p2], Memi[po], nelem)
		default:
		    call xvv_error (s_badop)
		}

	    default:
		call xvv_error (s_badswitch)
	    }
	} else {
	    # Operations supporting any arithmetic type.

	    switch (dtype) {

	    case TY_SHORT:
		switch (opcode) {
		case LT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALS(in1) < O_VALS(in2))
		    else if (len2 <= 0)
			call abltks (Mems[p1], O_VALS(in2), Memi[po], nelem)
		    else
			call ablts (Mems[p1], Mems[p2], Memi[po], nelem)

		case LE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALS(in1) <= O_VALS(in2))
		    else if (len2 <= 0)
			call ableks (Mems[p1], O_VALS(in2), Memi[po], nelem)
		    else
			call ables (Mems[p1], Mems[p2], Memi[po], nelem)

		case GT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALS(in1) > O_VALS(in2))
		    else if (len2 <= 0)
			call abgtks (Mems[p1], O_VALS(in2), Memi[po], nelem)
		    else
			call abgts (Mems[p1], Mems[p2], Memi[po], nelem)

		case GE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALS(in1) >= O_VALS(in2))
		    else if (len2 <= 0)
			call abgeks (Mems[p1], O_VALS(in2), Memi[po], nelem)
		    else
			call abges (Mems[p1], Mems[p2], Memi[po], nelem)

		case EQ:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALS(in1) == O_VALS(in2))
		    else if (len2 <= 0)
			call abeqks (Mems[p1], O_VALS(in2), Memi[po], nelem)
		    else
			call abeqs (Mems[p1], Mems[p2], Memi[po], nelem)

		case NE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALS(in1) != O_VALS(in2))
		    else if (len2 <= 0)
			call abneks (Mems[p1], O_VALS(in2), Memi[po], nelem)
		    else
			call abnes (Mems[p1], Mems[p2], Memi[po], nelem)

		default:
		    call xvv_error (s_badop)
		}

	    case TY_INT:
		switch (opcode) {
		case LT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALI(in1) < O_VALI(in2))
		    else if (len2 <= 0)
			call abltki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    else
			call ablti (Memi[p1], Memi[p2], Memi[po], nelem)

		case LE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALI(in1) <= O_VALI(in2))
		    else if (len2 <= 0)
			call ableki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    else
			call ablei (Memi[p1], Memi[p2], Memi[po], nelem)

		case GT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALI(in1) > O_VALI(in2))
		    else if (len2 <= 0)
			call abgtki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    else
			call abgti (Memi[p1], Memi[p2], Memi[po], nelem)

		case GE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALI(in1) >= O_VALI(in2))
		    else if (len2 <= 0)
			call abgeki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    else
			call abgei (Memi[p1], Memi[p2], Memi[po], nelem)

		case EQ:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALI(in1) == O_VALI(in2))
		    else if (len2 <= 0)
			call abeqki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    else
			call abeqi (Memi[p1], Memi[p2], Memi[po], nelem)

		case NE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALI(in1) != O_VALI(in2))
		    else if (len2 <= 0)
			call abneki (Memi[p1], O_VALI(in2), Memi[po], nelem)
		    else
			call abnei (Memi[p1], Memi[p2], Memi[po], nelem)

		default:
		    call xvv_error (s_badop)
		}

	    case TY_LONG:
		switch (opcode) {
		case LT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALL(in1) < O_VALL(in2))
		    else if (len2 <= 0)
			call abltkl (Meml[p1], O_VALL(in2), Memi[po], nelem)
		    else
			call abltl (Meml[p1], Meml[p2], Memi[po], nelem)

		case LE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALL(in1) <= O_VALL(in2))
		    else if (len2 <= 0)
			call ablekl (Meml[p1], O_VALL(in2), Memi[po], nelem)
		    else
			call ablel (Meml[p1], Meml[p2], Memi[po], nelem)

		case GT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALL(in1) > O_VALL(in2))
		    else if (len2 <= 0)
			call abgtkl (Meml[p1], O_VALL(in2), Memi[po], nelem)
		    else
			call abgtl (Meml[p1], Meml[p2], Memi[po], nelem)

		case GE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALL(in1) >= O_VALL(in2))
		    else if (len2 <= 0)
			call abgekl (Meml[p1], O_VALL(in2), Memi[po], nelem)
		    else
			call abgel (Meml[p1], Meml[p2], Memi[po], nelem)

		case EQ:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALL(in1) == O_VALL(in2))
		    else if (len2 <= 0)
			call abeqkl (Meml[p1], O_VALL(in2), Memi[po], nelem)
		    else
			call abeql (Meml[p1], Meml[p2], Memi[po], nelem)

		case NE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALL(in1) != O_VALL(in2))
		    else if (len2 <= 0)
			call abnekl (Meml[p1], O_VALL(in2), Memi[po], nelem)
		    else
			call abnel (Meml[p1], Meml[p2], Memi[po], nelem)

		default:
		    call xvv_error (s_badop)
		}

	    case TY_REAL:
		switch (opcode) {
		case LT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALR(in1) < O_VALR(in2))
		    else if (len2 <= 0)
			call abltkr (Memr[p1], O_VALR(in2), Memi[po], nelem)
		    else
			call abltr (Memr[p1], Memr[p2], Memi[po], nelem)

		case LE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALR(in1) <= O_VALR(in2))
		    else if (len2 <= 0)
			call ablekr (Memr[p1], O_VALR(in2), Memi[po], nelem)
		    else
			call abler (Memr[p1], Memr[p2], Memi[po], nelem)

		case GT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALR(in1) > O_VALR(in2))
		    else if (len2 <= 0)
			call abgtkr (Memr[p1], O_VALR(in2), Memi[po], nelem)
		    else
			call abgtr (Memr[p1], Memr[p2], Memi[po], nelem)

		case GE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALR(in1) >= O_VALR(in2))
		    else if (len2 <= 0)
			call abgekr (Memr[p1], O_VALR(in2), Memi[po], nelem)
		    else
			call abger (Memr[p1], Memr[p2], Memi[po], nelem)

		case EQ:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALR(in1) == O_VALR(in2))
		    else if (len2 <= 0)
			call abeqkr (Memr[p1], O_VALR(in2), Memi[po], nelem)
		    else
			call abeqr (Memr[p1], Memr[p2], Memi[po], nelem)

		case NE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALR(in1) != O_VALR(in2))
		    else if (len2 <= 0)
			call abnekr (Memr[p1], O_VALR(in2), Memi[po], nelem)
		    else
			call abner (Memr[p1], Memr[p2], Memi[po], nelem)

		default:
		    call xvv_error (s_badop)
		}

	    case TY_DOUBLE:
		switch (opcode) {
		case LT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALD(in1) < O_VALD(in2))
		    else if (len2 <= 0)
			call abltkd (Memd[p1], O_VALD(in2), Memi[po], nelem)
		    else
			call abltd (Memd[p1], Memd[p2], Memi[po], nelem)

		case LE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALD(in1) <= O_VALD(in2))
		    else if (len2 <= 0)
			call ablekd (Memd[p1], O_VALD(in2), Memi[po], nelem)
		    else
			call abled (Memd[p1], Memd[p2], Memi[po], nelem)

		case GT:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALD(in1) > O_VALD(in2))
		    else if (len2 <= 0)
			call abgtkd (Memd[p1], O_VALD(in2), Memi[po], nelem)
		    else
			call abgtd (Memd[p1], Memd[p2], Memi[po], nelem)

		case GE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALD(in1) >= O_VALD(in2))
		    else if (len2 <= 0)
			call abgekd (Memd[p1], O_VALD(in2), Memi[po], nelem)
		    else
			call abged (Memd[p1], Memd[p2], Memi[po], nelem)

		case EQ:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALD(in1) == O_VALD(in2))
		    else if (len2 <= 0)
			call abeqkd (Memd[p1], O_VALD(in2), Memi[po], nelem)
		    else
			call abeqd (Memd[p1], Memd[p2], Memi[po], nelem)

		case NE:
		    if (len1 <= 0)
			O_VALI(out) = btoi (O_VALD(in1) != O_VALD(in2))
		    else if (len2 <= 0)
			call abnekd (Memd[p1], O_VALD(in2), Memi[po], nelem)
		    else
			call abned (Memd[p1], Memd[p2], Memi[po], nelem)

		default:
		    call xvv_error (s_badop)
		}

	    default:
		call xvv_error (s_badswitch)
	    }
	}

	# Free any storage in input operands.
	call xvv_freeop (in1)
	call xvv_freeop (in2)
end


# XVV_PATMATCH -- Match a string against a pattern, returning the patmatch
# index if the string matches.  The pattern may contain any of the conventional
# pattern matching metacharacters.  Closure (i.e., "*") is mapped to "?*".

int procedure xvv_patmatch (str, pat)

char	str[ARB]		#I operand string
char	pat[ARB]		#I pattern

int	junk, ip, index
pointer	sp, patstr, patbuf, op
int	patmake(), patmatch()

begin
	call smark (sp)
	call salloc (patstr, SZ_FNAME, TY_CHAR)
	call salloc (patbuf, SZ_LINE,  TY_CHAR)
	call aclrc (Memc[patstr], SZ_FNAME)
	call aclrc (Memc[patbuf], SZ_LINE)

	# Map pattern, changing '*' into '?*'.
	op = patstr
	for (ip=1;  pat[ip] != EOS;  ip=ip+1) {
	    if (pat[ip] == '*') {
		Memc[op] = '?'
		op = op + 1
	    }
	    Memc[op] = pat[ip]
	    op = op + 1
	}

	# Encode pattern.
	junk = patmake (Memc[patstr], Memc[patbuf], SZ_LINE)

	# Perform the pattern matching operation.
	index = patmatch (str, Memc[patbuf])

	call sfree (sp)
	return (index)
end


# XVV_NEWTYPE -- Get the datatype of a binary operation, given the datatypes
# of the two input operands.  An error action is taken if the datatypes are
# incompatible, e.g., boolean and anything else or string and anything else.

int procedure xvv_newtype (type1, type2)

int	type1			#I datatype of first operand
int	type2			#I datatype of second operand

int	newtype, p, q, i
int	tyindex[NTYPES], ttbl[NTYPES*NTYPES]
data	tyindex	/T_B, T_C, T_S, T_I, T_L, T_R, T_D/

data	(ttbl(i),i= 1, 7)	/T_B,    0,    0,    0,    0,    0,    0/
data	(ttbl(i),i= 8,14)	/  0,  T_C,    0,    0,    0,    0,    0/
data	(ttbl(i),i=15,21)	/  0,    0,  T_S,  T_I,  T_L,  T_R,  T_D/
data	(ttbl(i),i=22,28)	/  0,    0,  T_I,  T_I,  T_L,  T_R,  T_D/
data	(ttbl(i),i=29,35)	/  0,    0,  T_L,  T_L,  T_L,  T_R,  T_D/
data	(ttbl(i),i=36,42)	/  0,    0,  T_R,  T_R,  T_R,  T_R,  T_D/
data	(ttbl(i),i=43,49)	/  0,    0,  T_D,  T_D,  T_D,  T_D,  T_D/

begin
	do i = 1, NTYPES {
	    if (tyindex[i] == type1)
		p = i
	    if (tyindex[i] == type2)
		q = i
	}

	newtype = ttbl[(p-1)*NTYPES+q]
	if (newtype == 0)
	    call xvv_error ("operands have incompatible types")
	else
	    return (newtype)
end


# XVV_QUEST -- Conditional expression.  If the condition operand is true
# return the first (true) operand, else return the second (false) operand.

procedure xvv_quest (cond, in1, in2, out)

pointer	cond			#I pointer to condition operand
pointer	in1, in2		#I pointer to true,false operands
pointer	out			#I pointer to output operand

int	dtype, nelem, i
pointer	sp, otemp, ip1, ip2, op, sel
errchk	xvv_error, xvv_newtype, xvv_initop, xvv_chtype
int	xvv_newtype(), btoi()

begin
	switch (O_TYPE(cond)) {
	case TY_BOOL, TY_INT:
	    ;
	case TY_SHORT, TY_LONG:
	    call xvv_chtype (cond, cond, TY_BOOL)
	default:
	    call xvv_error ("evvexpr: nonboolean condition operand")
	}

	if (O_LEN(cond) <= 0 &&
	    (O_LEN(in1) <= 0 || O_TYPE(in1) == TY_CHAR) &&
	    (O_LEN(in2) <= 0 || O_TYPE(in2) == TY_CHAR)) {

	    # Both operands and the conditional are scalars; the expression
	    # type is the type of the selected operand.

	    if (O_VALI(cond) != 0) {
		YYMOVE (in1, out)
		call xvv_freeop (in2)
	    } else {
		YYMOVE (in2, out)
		call xvv_freeop (in1)
	    }

	} else if (O_TYPE(in1) == TY_CHAR || O_TYPE(in2) == TY_CHAR) {
	    # This combination is not legal.
	    call xvv_error ("evvexpr: character and vector in cond expr")

	} else {
	    # Vector/scalar or vector/vector operation.  Both operands must
	    # be of the same type.

	    dtype = xvv_newtype (O_TYPE(in1), O_TYPE(in2))

	    # Compute the size of the output operand.  If both input operands
	    # are vectors the length of the output vector is the shorter of
	    # the two.  The condition operand contributes to the dimension of
	    # the expression result, although not to the datatype.

	    nelem = 0
	    if (O_LEN(in1) > 0 && O_LEN(in2) > 0)
		nelem = min (O_LEN(in1), O_LEN(in2))
	    else if (O_LEN(in1) > 0)
		nelem = O_LEN(in1)
	    else if (O_LEN(in2) > 0)
		nelem = O_LEN(in2)

	    if (O_LEN(cond) > 0 && nelem > 0)
		nelem = min (O_LEN(cond), nelem)
	    else if (O_LEN(cond) > 0)
		nelem = O_LEN(cond)

	    # If this is a scalar/vector operation make sure the vector is the
	    # first operand.

	    if (O_LEN(in1) == 0 && O_LEN(in2) > 0) {
		call smark (sp)
		call salloc (otemp, LEN_OPERAND, TY_STRUCT)
		YYMOVE (in1, otemp)
		YYMOVE (in2, in1)
		YYMOVE (otemp, in2)
		call sfree (sp)

		# Since we are swapping arguments we need to negate the cond.
		if (O_LEN(cond) <= 0)
		    O_VALI(cond) = btoi (O_VALI(cond) == 0)
		else {
		    call abeqki (Memi[O_VALP(cond)], NO, Memi[O_VALP(cond)],
			nelem)
		}
	    }

	    # Initialize the output operand.
	    call xvv_initop (out, nelem, dtype)

	    # Convert input operands to desired computation type.
	    if (O_TYPE(in1) != dtype)
		call xvv_chtype (in1, in1, dtype)
	    if (O_TYPE(in2) != dtype)
		call xvv_chtype (in2, in2, dtype)

	    ip1 = O_VALP(in1)
	    ip2 = O_VALP(in2)
	    op  = O_VALP(out)
	    sel = O_VALP(cond)

	    # Perform the operation.
	    switch (dtype) {

	    case TY_SHORT:
		if (O_LEN(in1) <= 0 && O_LEN(in2) <= 0) {
		    # Vector conditional, both operands are scalars.
		    do i = 1, nelem
			if (Memi[sel+i-1] != 0)
			    Mems[op+i-1] = O_VALS(in1)
			else
			    Mems[op+i-1] = O_VALS(in2)

		} else if (O_LEN(in2) <= 0) {
		    # Operand 1 is a vector, operand 2 is a scalar.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovs (Mems[ip1], Mems[op], nelem)
			else
			    call amovks (O_VALS(in2), Mems[op], nelem)
		    } else {
			# Conditional is a vector.
			call aselks (Mems[ip1], O_VALS(in2), Mems[op],
			    Memi[sel], nelem)
		    }
		} else {
		    # Both operands are vectors.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovs (Mems[ip1], Mems[op], nelem)
			else
			    call amovs (Mems[ip2], Mems[op], nelem)
		    } else {
			# Conditional is a vector.
			call asels (Mems[ip1], Mems[ip2], Mems[op],
			    Memi[sel], nelem)
		    }
		}

	    case TY_INT:
		if (O_LEN(in1) <= 0 && O_LEN(in2) <= 0) {
		    # Vector conditional, both operands are scalars.
		    do i = 1, nelem
			if (Memi[sel+i-1] != 0)
			    Memi[op+i-1] = O_VALI(in1)
			else
			    Memi[op+i-1] = O_VALI(in2)

		} else if (O_LEN(in2) <= 0) {
		    # Operand 1 is a vector, operand 2 is a scalar.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovi (Memi[ip1], Memi[op], nelem)
			else
			    call amovki (O_VALI(in2), Memi[op], nelem)
		    } else {
			# Conditional is a vector.
			call aselki (Memi[ip1], O_VALI(in2), Memi[op],
			    Memi[sel], nelem)
		    }
		} else {
		    # Both operands are vectors.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovi (Memi[ip1], Memi[op], nelem)
			else
			    call amovi (Memi[ip2], Memi[op], nelem)
		    } else {
			# Conditional is a vector.
			call aseli (Memi[ip1], Memi[ip2], Memi[op],
			    Memi[sel], nelem)
		    }
		}

	    case TY_LONG:
		if (O_LEN(in1) <= 0 && O_LEN(in2) <= 0) {
		    # Vector conditional, both operands are scalars.
		    do i = 1, nelem
			if (Memi[sel+i-1] != 0)
			    Meml[op+i-1] = O_VALL(in1)
			else
			    Meml[op+i-1] = O_VALL(in2)

		} else if (O_LEN(in2) <= 0) {
		    # Operand 1 is a vector, operand 2 is a scalar.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovl (Meml[ip1], Meml[op], nelem)
			else
			    call amovkl (O_VALL(in2), Meml[op], nelem)
		    } else {
			# Conditional is a vector.
			call aselkl (Meml[ip1], O_VALL(in2), Meml[op],
			    Memi[sel], nelem)
		    }
		} else {
		    # Both operands are vectors.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovl (Meml[ip1], Meml[op], nelem)
			else
			    call amovl (Meml[ip2], Meml[op], nelem)
		    } else {
			# Conditional is a vector.
			call asell (Meml[ip1], Meml[ip2], Meml[op],
			    Memi[sel], nelem)
		    }
		}

	    case TY_REAL:
		if (O_LEN(in1) <= 0 && O_LEN(in2) <= 0) {
		    # Vector conditional, both operands are scalars.
		    do i = 1, nelem
			if (Memi[sel+i-1] != 0)
			    Memr[op+i-1] = O_VALR(in1)
			else
			    Memr[op+i-1] = O_VALR(in2)

		} else if (O_LEN(in2) <= 0) {
		    # Operand 1 is a vector, operand 2 is a scalar.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovr (Memr[ip1], Memr[op], nelem)
			else
			    call amovkr (O_VALR(in2), Memr[op], nelem)
		    } else {
			# Conditional is a vector.
			call aselkr (Memr[ip1], O_VALR(in2), Memr[op],
			    Memi[sel], nelem)
		    }
		} else {
		    # Both operands are vectors.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovr (Memr[ip1], Memr[op], nelem)
			else
			    call amovr (Memr[ip2], Memr[op], nelem)
		    } else {
			# Conditional is a vector.
			call aselr (Memr[ip1], Memr[ip2], Memr[op],
			    Memi[sel], nelem)
		    }
		}

	    case TY_DOUBLE:
		if (O_LEN(in1) <= 0 && O_LEN(in2) <= 0) {
		    # Vector conditional, both operands are scalars.
		    do i = 1, nelem
			if (Memi[sel+i-1] != 0)
			    Memd[op+i-1] = O_VALD(in1)
			else
			    Memd[op+i-1] = O_VALD(in2)

		} else if (O_LEN(in2) <= 0) {
		    # Operand 1 is a vector, operand 2 is a scalar.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovd (Memd[ip1], Memd[op], nelem)
			else
			    call amovkd (O_VALD(in2), Memd[op], nelem)
		    } else {
			# Conditional is a vector.
			call aselkd (Memd[ip1], O_VALD(in2), Memd[op],
			    Memi[sel], nelem)
		    }
		} else {
		    # Both operands are vectors.
		    if (O_LEN(cond) <= 0) {
			# Conditional is a scalar.
			if (O_VALI(cond) != 0)
			    call amovd (Memd[ip1], Memd[op], nelem)
			else
			    call amovd (Memd[ip2], Memd[op], nelem)
		    } else {
			# Conditional is a vector.
			call aseld (Memd[ip1], Memd[ip2], Memd[op],
			    Memi[sel], nelem)
		    }
		}

	    default:
		call xvv_error ("evvexpr: bad datatype in cond expr")
	    }

	    call xvv_freeop (in1)
	    call xvv_freeop (in2)
	}

	call xvv_freeop (cond)
end


# XVV_CALLFCN -- Call an intrinsic function.  If the function named is not
# one of the standard intrinsic functions, call an external user function
# if a function call procedure was supplied.

procedure xvv_callfcn (fcn, args, nargs, out)

char	fcn[ARB]		#I function to be called
pointer	args[ARB]		#I pointer to arglist descriptor
int	nargs			#I number of arguments
pointer	out			#I output operand (function value)


short	v_s
short	ahivs(), alovs()
short	ameds()
int	aravs()

int	v_i
int	ahivi(), alovi()
int	amedi()
int	aravi()

long	v_l
long	ahivl(), alovl()
long	amedl()
int	aravl()

real	v_r
real	ahivr(), alovr()
real	amedr()
int	aravr()

double	v_d
double	ahivd(), alovd()
double	amedd()
int	aravd()


real	mean_r, sigma_r
double	mean_d, sigma_d
real	asums(), asumi(), asumr()
double	asuml(), asumd()

bool	rangecheck
int	optype, opcode
int	chunk, repl, nelem, v_nargs, ch, shift, i, j
pointer	sp, sym, buf, ap, ip, op, in1, in2
include	"evvexpr.com"

pointer	stfind()
int	xvv_newtype(), strlen(), gctod(), btoi()
errchk	xvv_chtype, xvv_initop, xvv_newtype, xvv_error1, xvv_error2
errchk	zcall5, malloc

string	s_badtype "%s: illegal operand type"
define	free_ 91

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	# Lookup the function name in the symbol table.
	sym = stfind (ev_st, fcn)
	if (sym != NULL)
	    opcode = SYM_CODE(sym)
	else
	    opcode = 0

	# If the function named is not a standard one and the user has supplied
	# the entry point of an external function evaluation procedure, call
	# the user procedure to evaluate the function, otherwise abort.

	if (opcode <= 0)
	    if (ev_ufcn != NULL) {
		call zcall5 (ev_ufcn, ev_ufcn_data, fcn, args, nargs, out)
		if (O_TYPE(out) <= 0)
		    call xvv_error1 ("unrecognized macro or function `%s'", fcn)
		goto free_
	    } else
		call xvv_error1 ("unknown function `%s' called", fcn)

	# Range checking on functions that need it?
	rangecheck = (and (ev_flags, EV_RNGCHK) != 0)

	# Verify correct number of arguments.
	switch (opcode) {
	case F_MOD, F_REPL, F_SHIFT:
	    v_nargs = 2
	case F_MAX, F_MIN, F_ATAN, F_ATAN2, F_MEAN, F_STDDEV, F_MEDIAN:
	    v_nargs = -1
	default:
	    v_nargs = 1
	}
	if (v_nargs > 0 && nargs != v_nargs)
	    call xvv_error2 ("function `%s' requires %d arguments",
		fcn, v_nargs)
	else if (v_nargs < 0 && nargs < abs(v_nargs))
	    call xvv_error2 ("function `%s' requires at least %d arguments",
		fcn, abs(v_nargs))

	# Some functions require that the input operand be a certain type,
	# e.g. floating.  Handle the simple cases, converting input operands
	# to the desired type.

	switch (opcode) {
	case F_ACOS, F_ASIN, F_ATAN, F_ATAN2, F_COS, F_COSH, F_DEG, F_EXP,
	     F_LOG, F_LOG10, F_RAD, F_SIN, F_SINH, F_SQRT, F_TAN, F_TANH:

	    # These functions want a floating point input operand.
	    optype = TY_REAL
	    do i = 1, nargs {
		if (O_TYPE(args[i]) == TY_DOUBLE || O_TYPE(args[i]) == TY_LONG)
		    optype = TY_DOUBLE
	    }
	    do i = 1, nargs {
		if (O_TYPE(args[i]) != optype)
		    call xvv_chtype (args[i], args[i], optype)
	    }
	    call xvv_initop (out, O_LEN(args[1]), optype)

	case F_MOD, F_MIN, F_MAX, F_MEDIAN:
	    # These functions may have multiple arguments, all of which
	    # should be the same type.

	    optype = O_TYPE(args[1])
	    nelem = O_LEN(args[1])
	    do i = 2, nargs {
		optype = xvv_newtype (optype, O_TYPE(args[i]))
		if (O_LEN(args[i]) > 0)
		    if (nelem > 0)
			nelem = min (nelem, O_LEN(args[i]))
		    else if (nelem == 0)
			nelem = O_LEN(args[i])
	    }

	    do i = 1, nargs
		if (O_TYPE(args[i]) != optype)
		    call xvv_chtype (args[i], args[i], optype)

	    if (nargs == 1 && opcode == F_MEDIAN)
		nelem = 0
	    call xvv_initop (out, nelem, optype)

	case F_LEN:
	    # This function always returns an integer scalar value.
	    nelem = 0
	    optype = TY_INT
	    call xvv_initop (out, nelem, optype)

	case F_HIV, F_LOV:
	    # These functions return a scalar value.
	    nelem = 0
	    optype = O_TYPE(args[1])
	    if (optype == TY_BOOL)
		optype = TY_INT
	    call xvv_initop (out, nelem, optype)

	case F_SUM, F_MEAN, F_STDDEV:
	    # These functions require a vector argument and return a scalar
	    # value.

	    nelem = 0
	    optype = O_TYPE(args[1])
	    if (optype == TY_BOOL)
		optype = TY_INT
	    
	    if (optype == TY_DOUBLE)
		call xvv_initop (out, nelem, TY_DOUBLE)
	    else
		call xvv_initop (out, nelem, TY_REAL)

	case F_SORT, F_SHIFT:
	    # Vector to vector, no type conversions.
	    nelem = O_LEN(args[1])
	    optype = O_TYPE(args[1])
	    call xvv_initop (out, nelem, optype)

	default:
	    optype = 0
	}

	# Evaluate the function.
	ap = args[1]

	switch (opcode) {
	case F_ABS:
	    call xvv_initop (out, O_LEN(ap), O_TYPE(ap))
	    switch (O_TYPE(ap)) {

	    case TY_SHORT:
		if (O_LEN(ap) > 0) {
		    call aabss (Mems[O_VALP(ap)], Mems[O_VALP(out)],
			O_LEN(ap))
		} else
		    O_VALS(out) = abs(O_VALS(ap))

	    case TY_INT:
		if (O_LEN(ap) > 0) {
		    call aabsi (Memi[O_VALP(ap)], Memi[O_VALP(out)],
			O_LEN(ap))
		} else
		    O_VALI(out) = abs(O_VALI(ap))

	    case TY_LONG:
		if (O_LEN(ap) > 0) {
		    call aabsl (Meml[O_VALP(ap)], Meml[O_VALP(out)],
			O_LEN(ap))
		} else
		    O_VALL(out) = abs(O_VALL(ap))

	    case TY_REAL:
		if (O_LEN(ap) > 0) {
		    call aabsr (Memr[O_VALP(ap)], Memr[O_VALP(out)],
			O_LEN(ap))
		} else
		    O_VALR(out) = abs(O_VALR(ap))

	    case TY_DOUBLE:
		if (O_LEN(ap) > 0) {
		    call aabsd (Memd[O_VALP(ap)], Memd[O_VALP(out)],
			O_LEN(ap))
		} else
		    O_VALD(out) = abs(O_VALD(ap))

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_ACOS:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = acos (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = acos (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = acos (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = acos (O_VALD(ap))

	case F_ASIN:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = asin (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = asin (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = asin (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = asin (O_VALD(ap))

	case F_COS:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = cos (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = cos (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = cos (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = cos (O_VALD(ap))

	case F_COSH:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = cosh (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = cosh (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = cosh (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = cosh (O_VALD(ap))

	case F_DEG:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = RADTODEG(Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = RADTODEG (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = RADTODEG(Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = RADTODEG (O_VALD(ap))

	case F_EXP:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = exp (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = exp (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = exp (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = exp (O_VALD(ap))

	case F_LOG:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    op = O_VALP(out)
		    do i = 1, O_LEN(ap) {
			v_r = Memr[O_VALP(ap)+i-1]
			if (rangecheck && v_r <= 0)
			    Memr[op] = 0
			else
			    Memr[op] = log (v_r)
			op = op + 1
		    }
		} else {
		    v_r = O_VALR(ap)
		    if (rangecheck && v_r <= 0)
			O_VALR(out) = 0
		    else
			O_VALR(out) = log (v_r)
		}

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    op = O_VALP(out)
		    do i = 1, O_LEN(ap) {
			v_d = Memd[O_VALP(ap)+i-1]
			if (rangecheck && v_d <= 0)
			    Memd[op] = 0
			else
			    Memd[op] = log (v_d)
			op = op + 1
		    }
		} else {
		    v_d = O_VALD(ap)
		    if (rangecheck && v_d <= 0)
			O_VALD(out) = 0
		    else
			O_VALD(out) = log (v_d)
		}

	case F_LOG10:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    op = O_VALP(out)
		    do i = 1, O_LEN(ap) {
			v_r = Memr[O_VALP(ap)+i-1]
			if (rangecheck && v_r <= 0)
			    Memr[op] = 0
			else
			    Memr[op] = log10 (v_r)
			op = op + 1
		    }
		} else {
		    v_r = O_VALR(ap)
		    if (rangecheck && v_r <= 0)
			O_VALR(out) = 0
		    else
			O_VALR(out) = log10 (v_r)
		}

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    op = O_VALP(out)
		    do i = 1, O_LEN(ap) {
			v_d = Memd[O_VALP(ap)+i-1]
			if (rangecheck && v_d <= 0)
			    Memd[op] = 0
			else
			    Memd[op] = log10 (v_d)
			op = op + 1
		    }
		} else {
		    v_d = O_VALD(ap)
		    if (rangecheck && v_d <= 0)
			O_VALD(out) = 0
		    else
			O_VALD(out) = log10 (v_d)
		}

	case F_RAD:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = DEGTORAD(Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = DEGTORAD (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = DEGTORAD(Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = DEGTORAD (O_VALD(ap))

	case F_SIN:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = sin (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = sin (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = sin (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = sin (O_VALD(ap))

	case F_SINH:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = sinh (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = sinh (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = sinh (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = sinh (O_VALD(ap))

	case F_SQRT:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    op = O_VALP(out)
		    do i = 1, O_LEN(ap) {
			v_r = Memr[O_VALP(ap)+i-1]
			if (rangecheck && v_r < 0)
			    Memr[op] = 0
			else
			    Memr[op] = sqrt (v_r)
			op = op + 1
		    }
		} else {
		    v_r = O_VALR(ap)
		    if (rangecheck && v_r <= 0)
			O_VALR(out) = 0
		    else
			O_VALR(out) = sqrt (v_r)
		}

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    op = O_VALP(out)
		    do i = 1, O_LEN(ap) {
			v_d = Memd[O_VALP(ap)+i-1]
			if (rangecheck && v_d < 0)
			    Memd[op] = 0
			else
			    Memd[op] = sqrt (v_d)
			op = op + 1
		    }
		} else {
		    v_d = O_VALD(ap)
		    if (rangecheck && v_d <= 0)
			O_VALD(out) = 0
		    else
			O_VALD(out) = sqrt (v_d)
		}

	case F_TAN:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = tan (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = tan (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = tan (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = tan (O_VALD(ap))

	case F_TANH:

	    if (optype == TY_REAL) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memr[O_VALP(out)+i-1] = tanh (Memr[O_VALP(ap)+i-1])
		} else
		    O_VALR(out) = tanh (O_VALR(ap))

	    if (optype == TY_DOUBLE) 
		if (O_LEN(ap) > 0) {
		    do i = 1, O_LEN(ap)
			Memd[O_VALP(out)+i-1] = tanh (Memd[O_VALP(ap)+i-1])
		} else
		    O_VALD(out) = tanh (O_VALD(ap))


	case F_LEN:
	    # Vector length.
	    O_VALI(out) = O_LEN(ap)

	case F_HIV:
	    # High value.
	    switch (optype) {

	    case TY_SHORT:
		if (O_LEN(ap) > 0)
		    O_VALS(out) = ahivs (Mems[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALS(out) = O_VALS(ap)

	    case TY_INT:
		if (O_LEN(ap) > 0)
		    O_VALI(out) = ahivi (Memi[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALI(out) = O_VALI(ap)

	    case TY_LONG:
		if (O_LEN(ap) > 0)
		    O_VALL(out) = ahivl (Meml[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALL(out) = O_VALL(ap)

	    case TY_REAL:
		if (O_LEN(ap) > 0)
		    O_VALR(out) = ahivr (Memr[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALR(out) = O_VALR(ap)

	    case TY_DOUBLE:
		if (O_LEN(ap) > 0)
		    O_VALD(out) = ahivd (Memd[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALD(out) = O_VALD(ap)

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }
	case F_LOV:
	    # Low value.
	    switch (optype) {

	    case TY_SHORT:
		if (O_LEN(ap) > 0)
		    O_VALS(out) = alovs (Mems[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALS(out) = O_VALS(ap)

	    case TY_INT:
		if (O_LEN(ap) > 0)
		    O_VALI(out) = alovi (Memi[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALI(out) = O_VALI(ap)

	    case TY_LONG:
		if (O_LEN(ap) > 0)
		    O_VALL(out) = alovl (Meml[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALL(out) = O_VALL(ap)

	    case TY_REAL:
		if (O_LEN(ap) > 0)
		    O_VALR(out) = alovr (Memr[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALR(out) = O_VALR(ap)

	    case TY_DOUBLE:
		if (O_LEN(ap) > 0)
		    O_VALD(out) = alovd (Memd[O_VALP(ap)], O_LEN(ap))
		else
		    O_VALD(out) = O_VALD(ap)

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_SUM:
	    # Vector sum.
	    switch (optype) {

	    case TY_SHORT:
		if (O_LEN(ap) > 0)
		    v_r = asums (Mems[O_VALP(ap)], O_LEN(ap))
		else
		    v_r = O_VALS(ap)

	    case TY_INT:
		if (O_LEN(ap) > 0)
		    v_r = asumi (Memi[O_VALP(ap)], O_LEN(ap))
		else
		    v_r = O_VALI(ap)

	    case TY_LONG:
		if (O_LEN(ap) > 0)
		    v_r = asuml (Meml[O_VALP(ap)], O_LEN(ap))
		else
		    v_r = O_VALL(ap)

	    case TY_REAL:
		if (O_LEN(ap) > 0)
		    v_r = asumr (Memr[O_VALP(ap)], O_LEN(ap))
		else
		    v_r = O_VALR(ap)

	    case TY_DOUBLE:
		if (O_LEN(ap) > 0)
		    v_d = asumd (Memd[O_VALP(ap)], O_LEN(ap))
		else
		    v_d = O_VALD(ap)
	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	    if (optype == TY_DOUBLE)
		O_VALD(out) = v_d
	    else
		O_VALR(out) = v_r

	case F_MEAN, F_STDDEV:
	    # Compute the mean or standard deviation of a vector.  An optional
	    # second argument may be supplied to compute a K-sigma rejection
	    # mean and sigma.

	    if (nargs == 2) {
		if (O_LEN(args[2]) > 0)
		    call xvv_error1 ("%s: ksigma arg must be a scalar" , fcn)

		switch (O_TYPE(args[2])) {
		case TY_REAL:
		    v_r = O_VALR(args[2])
		    v_d = v_r
		case TY_DOUBLE:
		    v_d = O_VALD(args[2])
		    v_r = v_d
		default:
		    call xvv_chtype (args[2], args[2], TY_REAL)
		    v_r = O_VALR(args[2])
		    v_d = v_r
		}
	    } else {
		v_r = 0.0
		v_d = 0.0
	    }

	    switch (optype) {

	    case TY_SHORT:
		v_i = aravs (Mems[O_VALP(ap)], O_LEN(ap), mean_r,sigma_r,v_r)

	    case TY_INT:
		v_i = aravi (Memi[O_VALP(ap)], O_LEN(ap), mean_r,sigma_r,v_r)

	    case TY_REAL:
		v_i = aravr (Memr[O_VALP(ap)], O_LEN(ap), mean_r,sigma_r,v_r)


	    case TY_LONG:
		v_i = aravl (Meml[O_VALP(ap)], O_LEN(ap), mean_d,sigma_d,v_d)

	    case TY_DOUBLE:
		v_i = aravd (Memd[O_VALP(ap)], O_LEN(ap), mean_d,sigma_d,v_d)

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	    if (opcode == F_MEAN) {
		if (O_TYPE(out) == TY_REAL)
		    O_VALR(out) = mean_r
		else
		    O_VALD(out) = mean_d
	    } else {
		if (O_TYPE(out) == TY_REAL)
		    O_VALR(out) = sigma_r
		else
		    O_VALD(out) = sigma_d
	    }

	case F_MEDIAN:
	    # Compute the median value of a vector, or the vector median
	    # of 3 or more vectors.

	    switch (nargs) {
	    case 1:
		switch (optype) {

		case TY_SHORT:
		    O_VALS(out) = ameds (Mems[O_VALP(ap)], O_LEN(ap))

		case TY_INT:
		    O_VALI(out) = amedi (Memi[O_VALP(ap)], O_LEN(ap))

		case TY_LONG:
		    O_VALL(out) = amedl (Meml[O_VALP(ap)], O_LEN(ap))

		case TY_REAL:
		    O_VALR(out) = amedr (Memr[O_VALP(ap)], O_LEN(ap))

		case TY_DOUBLE:
		    O_VALD(out) = amedd (Memd[O_VALP(ap)], O_LEN(ap))

		default:
		    call xvv_error1 (s_badtype, fcn)
		}
	    case 3:
		switch (optype) {

		case TY_SHORT:
		    call amed3s (Mems[O_VALP(args[1])],
				  Mems[O_VALP(args[2])],
				  Mems[O_VALP(args[3])],
				  Mems[O_VALP(out)], nelem)

		case TY_INT:
		    call amed3i (Memi[O_VALP(args[1])],
				  Memi[O_VALP(args[2])],
				  Memi[O_VALP(args[3])],
				  Memi[O_VALP(out)], nelem)

		case TY_LONG:
		    call amed3l (Meml[O_VALP(args[1])],
				  Meml[O_VALP(args[2])],
				  Meml[O_VALP(args[3])],
				  Meml[O_VALP(out)], nelem)

		case TY_REAL:
		    call amed3r (Memr[O_VALP(args[1])],
				  Memr[O_VALP(args[2])],
				  Memr[O_VALP(args[3])],
				  Memr[O_VALP(out)], nelem)

		case TY_DOUBLE:
		    call amed3d (Memd[O_VALP(args[1])],
				  Memd[O_VALP(args[2])],
				  Memd[O_VALP(args[3])],
				  Memd[O_VALP(out)], nelem)

		default:
		    call xvv_error1 (s_badtype, fcn)
		}
	    case 4:
		switch (optype) {

		case TY_SHORT:
		    call amed4s (Mems[O_VALP(args[1])],
				  Mems[O_VALP(args[2])],
				  Mems[O_VALP(args[3])],
				  Mems[O_VALP(args[4])],
				  Mems[O_VALP(out)], nelem)

		case TY_INT:
		    call amed4i (Memi[O_VALP(args[1])],
				  Memi[O_VALP(args[2])],
				  Memi[O_VALP(args[3])],
				  Memi[O_VALP(args[4])],
				  Memi[O_VALP(out)], nelem)

		case TY_LONG:
		    call amed4l (Meml[O_VALP(args[1])],
				  Meml[O_VALP(args[2])],
				  Meml[O_VALP(args[3])],
				  Meml[O_VALP(args[4])],
				  Meml[O_VALP(out)], nelem)

		case TY_REAL:
		    call amed4r (Memr[O_VALP(args[1])],
				  Memr[O_VALP(args[2])],
				  Memr[O_VALP(args[3])],
				  Memr[O_VALP(args[4])],
				  Memr[O_VALP(out)], nelem)

		case TY_DOUBLE:
		    call amed4d (Memd[O_VALP(args[1])],
				  Memd[O_VALP(args[2])],
				  Memd[O_VALP(args[3])],
				  Memd[O_VALP(args[4])],
				  Memd[O_VALP(out)], nelem)

		default:
		    call xvv_error1 (s_badtype, fcn)
		}
	    case 5:
		switch (optype) {

		case TY_SHORT:
		    call amed5s (Mems[O_VALP(args[1])],
				  Mems[O_VALP(args[2])],
				  Mems[O_VALP(args[3])],
				  Mems[O_VALP(args[4])],
				  Mems[O_VALP(args[5])],
				  Mems[O_VALP(out)], nelem)

		case TY_INT:
		    call amed5i (Memi[O_VALP(args[1])],
				  Memi[O_VALP(args[2])],
				  Memi[O_VALP(args[3])],
				  Memi[O_VALP(args[4])],
				  Memi[O_VALP(args[5])],
				  Memi[O_VALP(out)], nelem)

		case TY_LONG:
		    call amed5l (Meml[O_VALP(args[1])],
				  Meml[O_VALP(args[2])],
				  Meml[O_VALP(args[3])],
				  Meml[O_VALP(args[4])],
				  Meml[O_VALP(args[5])],
				  Meml[O_VALP(out)], nelem)

		case TY_REAL:
		    call amed5r (Memr[O_VALP(args[1])],
				  Memr[O_VALP(args[2])],
				  Memr[O_VALP(args[3])],
				  Memr[O_VALP(args[4])],
				  Memr[O_VALP(args[5])],
				  Memr[O_VALP(out)], nelem)

		case TY_DOUBLE:
		    call amed5d (Memd[O_VALP(args[1])],
				  Memd[O_VALP(args[2])],
				  Memd[O_VALP(args[3])],
				  Memd[O_VALP(args[4])],
				  Memd[O_VALP(args[5])],
				  Memd[O_VALP(out)], nelem)

		default:
		    call xvv_error1 (s_badtype, fcn)
		}
	    default:
		call xvv_error1 ("%s: wrong number of arguments", fcn)
	    }

	case F_REPL:
	    # Replicate an item to make a longer vector.

	    chunk = O_LEN(ap)
	    optype = O_TYPE(ap)
	    if (optype == TY_BOOL)
		optype = TY_INT

	    if (O_LEN(args[2]) > 0)
		call xvv_error1 ("%s: replication factor must be a scalar", fcn)
	    if (O_TYPE(args[2]) != TY_INT)
		call xvv_chtype (args[2], args[2], TY_INT)
	    repl = max (1, O_VALI(args[2]))

	    if (chunk <= 0)
		nelem = repl
	    else
		nelem = chunk * repl
	    call xvv_initop (out, nelem, optype)

	    switch (optype) {

	    case TY_SHORT:
		if (chunk > 0) {
		    ip = O_VALP(ap)
		    op = O_VALP(out)
		    do i = 1, repl {
			call amovs (Mems[ip], Mems[op], chunk)
			op = op + chunk
		    }
		} else
		    call amovks (O_VALS(ap), Mems[O_VALP(out)], nelem)

	    case TY_INT:
		if (chunk > 0) {
		    ip = O_VALP(ap)
		    op = O_VALP(out)
		    do i = 1, repl {
			call amovi (Memi[ip], Memi[op], chunk)
			op = op + chunk
		    }
		} else
		    call amovki (O_VALI(ap), Memi[O_VALP(out)], nelem)

	    case TY_LONG:
		if (chunk > 0) {
		    ip = O_VALP(ap)
		    op = O_VALP(out)
		    do i = 1, repl {
			call amovl (Meml[ip], Meml[op], chunk)
			op = op + chunk
		    }
		} else
		    call amovkl (O_VALL(ap), Meml[O_VALP(out)], nelem)

	    case TY_REAL:
		if (chunk > 0) {
		    ip = O_VALP(ap)
		    op = O_VALP(out)
		    do i = 1, repl {
			call amovr (Memr[ip], Memr[op], chunk)
			op = op + chunk
		    }
		} else
		    call amovkr (O_VALR(ap), Memr[O_VALP(out)], nelem)

	    case TY_DOUBLE:
		if (chunk > 0) {
		    ip = O_VALP(ap)
		    op = O_VALP(out)
		    do i = 1, repl {
			call amovd (Memd[ip], Memd[op], chunk)
			op = op + chunk
		    }
		} else
		    call amovkd (O_VALD(ap), Memd[O_VALP(out)], nelem)

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_SHIFT:
	    # Vector shift.
	    if (O_LEN(args[2]) > 0)
		call xvv_error1 ("%s: shift arg must be a scalar" , fcn)
	    if (O_TYPE(args[2]) != TY_INT)
		call xvv_chtype (args[2], args[2], TY_INT)
	    shift = O_VALI(args[2])

	    if (abs(shift) > nelem) {
		if (shift > 0)
		    shift = nelem
		else
		    shift = -nelem
	    }

	    switch (optype) {

	    case TY_SHORT:
		if (nelem > 0) {
		    do i = 1, nelem {
			j = i - shift
			if (j < 1)
			    j = j + nelem
			else if (j > nelem)
			    j = j - nelem
			Mems[O_VALP(out)+i-1] = Mems[O_VALP(ap)+j-1]
		    }
		} else
		    O_VALS(out) = (O_VALS(ap))

	    case TY_INT:
		if (nelem > 0) {
		    do i = 1, nelem {
			j = i - shift
			if (j < 1)
			    j = j + nelem
			else if (j > nelem)
			    j = j - nelem
			Memi[O_VALP(out)+i-1] = Memi[O_VALP(ap)+j-1]
		    }
		} else
		    O_VALI(out) = (O_VALI(ap))

	    case TY_LONG:
		if (nelem > 0) {
		    do i = 1, nelem {
			j = i - shift
			if (j < 1)
			    j = j + nelem
			else if (j > nelem)
			    j = j - nelem
			Meml[O_VALP(out)+i-1] = Meml[O_VALP(ap)+j-1]
		    }
		} else
		    O_VALL(out) = (O_VALL(ap))

	    case TY_REAL:
		if (nelem > 0) {
		    do i = 1, nelem {
			j = i - shift
			if (j < 1)
			    j = j + nelem
			else if (j > nelem)
			    j = j - nelem
			Memr[O_VALP(out)+i-1] = Memr[O_VALP(ap)+j-1]
		    }
		} else
		    O_VALR(out) = (O_VALR(ap))

	    case TY_DOUBLE:
		if (nelem > 0) {
		    do i = 1, nelem {
			j = i - shift
			if (j < 1)
			    j = j + nelem
			else if (j > nelem)
			    j = j - nelem
			Memd[O_VALP(out)+i-1] = Memd[O_VALP(ap)+j-1]
		    }
		} else
		    O_VALD(out) = (O_VALD(ap))

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_SORT:
	    # Sort a vector.
	    switch (optype) {

	    case TY_SHORT:
		if (nelem > 0)
		    call asrts (Mems[O_VALP(ap)], Mems[O_VALP(out)], nelem)
		else
		    O_VALS(out) = (O_VALS(ap))

	    case TY_INT:
		if (nelem > 0)
		    call asrti (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)
		else
		    O_VALI(out) = (O_VALI(ap))

	    case TY_LONG:
		if (nelem > 0)
		    call asrtl (Meml[O_VALP(ap)], Meml[O_VALP(out)], nelem)
		else
		    O_VALL(out) = (O_VALL(ap))

	    case TY_REAL:
		if (nelem > 0)
		    call asrtr (Memr[O_VALP(ap)], Memr[O_VALP(out)], nelem)
		else
		    O_VALR(out) = (O_VALR(ap))

	    case TY_DOUBLE:
		if (nelem > 0)
		    call asrtd (Memd[O_VALP(ap)], Memd[O_VALP(out)], nelem)
		else
		    O_VALD(out) = (O_VALD(ap))

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_ATAN, F_ATAN2:

	    if (optype == TY_REAL) {
		if (nargs == 1) {
		    if (O_LEN(ap) > 0) {
			do i = 1, O_LEN(ap)
			    Memr[O_VALP(out)+i-1] =
				atan (Memr[O_VALP(ap)+i-1])
		    } else
			O_VALR(out) = atan (O_VALR(ap))
		} else {
		    if (O_LEN(ap) > 0) {
			do i = 1, O_LEN(ap)
			    Memr[O_VALP(out)+i-1] =
				atan2 (Memr[O_VALP(args[1])+i-1],
				       Memr[O_VALP(args[2])+i-1])
		    } else
			O_VALR(out) = atan2(O_VALR(args[1]), O_VALR(args[2]))
		}
	    }

	    if (optype == TY_DOUBLE) {
		if (nargs == 1) {
		    if (O_LEN(ap) > 0) {
			do i = 1, O_LEN(ap)
			    Memd[O_VALP(out)+i-1] =
				atan (Memd[O_VALP(ap)+i-1])
		    } else
			O_VALD(out) = atan (O_VALD(ap))
		} else {
		    if (O_LEN(ap) > 0) {
			do i = 1, O_LEN(ap)
			    Memd[O_VALP(out)+i-1] =
				atan2 (Memd[O_VALP(args[1])+i-1],
				       Memd[O_VALP(args[2])+i-1])
		    } else
			O_VALD(out) = atan2(O_VALD(args[1]), O_VALD(args[2]))
		}
	    }


	case F_MOD:
	    in1 = args[1]
	    in2 = args[2]

	    switch (optype) {

	    case TY_SHORT:
		if (O_LEN(in1) <= 0) {
		    O_VALS(out) = mod (O_VALS(in1), O_VALS(in2))
		} else if (O_LEN(in2) <= 0) {
		    call amodks (Mems[O_VALP(in1)], O_VALS(in2),
			Mems[O_VALP(out)], nelem)
		} else {
		    call amods (Mems[O_VALP(in1)], Mems[O_VALP(in2)],
			Mems[O_VALP(out)], nelem)
		}

	    case TY_INT:
		if (O_LEN(in1) <= 0) {
		    O_VALI(out) = mod (O_VALI(in1), O_VALI(in2))
		} else if (O_LEN(in2) <= 0) {
		    call amodki (Memi[O_VALP(in1)], O_VALI(in2),
			Memi[O_VALP(out)], nelem)
		} else {
		    call amodi (Memi[O_VALP(in1)], Memi[O_VALP(in2)],
			Memi[O_VALP(out)], nelem)
		}

	    case TY_LONG:
		if (O_LEN(in1) <= 0) {
		    O_VALL(out) = mod (O_VALL(in1), O_VALL(in2))
		} else if (O_LEN(in2) <= 0) {
		    call amodkl (Meml[O_VALP(in1)], O_VALL(in2),
			Meml[O_VALP(out)], nelem)
		} else {
		    call amodl (Meml[O_VALP(in1)], Meml[O_VALP(in2)],
			Meml[O_VALP(out)], nelem)
		}

	    case TY_REAL:
		if (O_LEN(in1) <= 0) {
		    O_VALR(out) = mod (O_VALR(in1), O_VALR(in2))
		} else if (O_LEN(in2) <= 0) {
		    call amodkr (Memr[O_VALP(in1)], O_VALR(in2),
			Memr[O_VALP(out)], nelem)
		} else {
		    call amodr (Memr[O_VALP(in1)], Memr[O_VALP(in2)],
			Memr[O_VALP(out)], nelem)
		}

	    case TY_DOUBLE:
		if (O_LEN(in1) <= 0) {
		    O_VALD(out) = mod (O_VALD(in1), O_VALD(in2))
		} else if (O_LEN(in2) <= 0) {
		    call amodkd (Memd[O_VALP(in1)], O_VALD(in2),
			Memd[O_VALP(out)], nelem)
		} else {
		    call amodd (Memd[O_VALP(in1)], Memd[O_VALP(in2)],
			Memd[O_VALP(out)], nelem)
		}

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_MAX:
	    switch (optype) {

	    case TY_SHORT:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovks (O_VALS(ap), Mems[O_VALP(out)], nelem)
		    else
			O_VALS(out) = O_VALS(ap)
		} else
		    call amovs (Mems[O_VALP(ap)], Mems[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALS(out) = max (O_VALS(ap), O_VALS(out))
			else {
			    call amaxks (Mems[O_VALP(out)], O_VALS(ap),
				Mems[O_VALP(out)], nelem)
			}
		    } else {
			call amaxs (Mems[O_VALP(out)], Mems[O_VALP(ap)],
			    Mems[O_VALP(out)], nelem)
		    }
		}

	    case TY_INT:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovki (O_VALI(ap), Memi[O_VALP(out)], nelem)
		    else
			O_VALI(out) = O_VALI(ap)
		} else
		    call amovi (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALI(out) = max (O_VALI(ap), O_VALI(out))
			else {
			    call amaxki (Memi[O_VALP(out)], O_VALI(ap),
				Memi[O_VALP(out)], nelem)
			}
		    } else {
			call amaxi (Memi[O_VALP(out)], Memi[O_VALP(ap)],
			    Memi[O_VALP(out)], nelem)
		    }
		}

	    case TY_LONG:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovkl (O_VALL(ap), Meml[O_VALP(out)], nelem)
		    else
			O_VALL(out) = O_VALL(ap)
		} else
		    call amovl (Meml[O_VALP(ap)], Meml[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALL(out) = max (O_VALL(ap), O_VALL(out))
			else {
			    call amaxkl (Meml[O_VALP(out)], O_VALL(ap),
				Meml[O_VALP(out)], nelem)
			}
		    } else {
			call amaxl (Meml[O_VALP(out)], Meml[O_VALP(ap)],
			    Meml[O_VALP(out)], nelem)
		    }
		}

	    case TY_REAL:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovkr (O_VALR(ap), Memr[O_VALP(out)], nelem)
		    else
			O_VALR(out) = O_VALR(ap)
		} else
		    call amovr (Memr[O_VALP(ap)], Memr[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALR(out) = max (O_VALR(ap), O_VALR(out))
			else {
			    call amaxkr (Memr[O_VALP(out)], O_VALR(ap),
				Memr[O_VALP(out)], nelem)
			}
		    } else {
			call amaxr (Memr[O_VALP(out)], Memr[O_VALP(ap)],
			    Memr[O_VALP(out)], nelem)
		    }
		}

	    case TY_DOUBLE:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovkd (O_VALD(ap), Memd[O_VALP(out)], nelem)
		    else
			O_VALD(out) = O_VALD(ap)
		} else
		    call amovd (Memd[O_VALP(ap)], Memd[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALD(out) = max (O_VALD(ap), O_VALD(out))
			else {
			    call amaxkd (Memd[O_VALP(out)], O_VALD(ap),
				Memd[O_VALP(out)], nelem)
			}
		    } else {
			call amaxd (Memd[O_VALP(out)], Memd[O_VALP(ap)],
			    Memd[O_VALP(out)], nelem)
		    }
		}

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_MIN:
	    switch (optype) {

	    case TY_SHORT:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovks (O_VALS(ap), Mems[O_VALP(out)], nelem)
		    else
			O_VALS(out) = O_VALS(ap)
		} else
		    call amovs (Mems[O_VALP(ap)], Mems[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALS(out) = min (O_VALS(ap), O_VALS(out))
			else {
			    call aminks (Mems[O_VALP(out)], O_VALS(ap),
				Mems[O_VALP(out)], nelem)
			}
		    } else {
			call amins (Mems[O_VALP(out)], Mems[O_VALP(ap)],
			    Mems[O_VALP(out)], nelem)
		    }
		}

	    case TY_INT:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovki (O_VALI(ap), Memi[O_VALP(out)], nelem)
		    else
			O_VALI(out) = O_VALI(ap)
		} else
		    call amovi (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALI(out) = min (O_VALI(ap), O_VALI(out))
			else {
			    call aminki (Memi[O_VALP(out)], O_VALI(ap),
				Memi[O_VALP(out)], nelem)
			}
		    } else {
			call amini (Memi[O_VALP(out)], Memi[O_VALP(ap)],
			    Memi[O_VALP(out)], nelem)
		    }
		}

	    case TY_LONG:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovkl (O_VALL(ap), Meml[O_VALP(out)], nelem)
		    else
			O_VALL(out) = O_VALL(ap)
		} else
		    call amovl (Meml[O_VALP(ap)], Meml[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALL(out) = min (O_VALL(ap), O_VALL(out))
			else {
			    call aminkl (Meml[O_VALP(out)], O_VALL(ap),
				Meml[O_VALP(out)], nelem)
			}
		    } else {
			call aminl (Meml[O_VALP(out)], Meml[O_VALP(ap)],
			    Meml[O_VALP(out)], nelem)
		    }
		}

	    case TY_REAL:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovkr (O_VALR(ap), Memr[O_VALP(out)], nelem)
		    else
			O_VALR(out) = O_VALR(ap)
		} else
		    call amovr (Memr[O_VALP(ap)], Memr[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALR(out) = min (O_VALR(ap), O_VALR(out))
			else {
			    call aminkr (Memr[O_VALP(out)], O_VALR(ap),
				Memr[O_VALP(out)], nelem)
			}
		    } else {
			call aminr (Memr[O_VALP(out)], Memr[O_VALP(ap)],
			    Memr[O_VALP(out)], nelem)
		    }
		}

	    case TY_DOUBLE:
		# Copy the first argument.
		ap = args[1]
		if (O_LEN(ap) <= 0) {
		    if (O_LEN(out) > 0)
			call amovkd (O_VALD(ap), Memd[O_VALP(out)], nelem)
		    else
			O_VALD(out) = O_VALD(ap)
		} else
		    call amovd (Memd[O_VALP(ap)], Memd[O_VALP(out)], nelem)

		# Process the second and remaining arguments.
		do i = 2, nargs {
		    ap = args[i]
		    if (O_LEN(ap) <= 0) {
			if (O_LEN(out) <= 0)
			    O_VALD(out) = min (O_VALD(ap), O_VALD(out))
			else {
			    call aminkd (Memd[O_VALP(out)], O_VALD(ap),
				Memd[O_VALP(out)], nelem)
			}
		    } else {
			call amind (Memd[O_VALP(out)], Memd[O_VALP(ap)],
			    Memd[O_VALP(out)], nelem)
		    }
		}

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_BOOL:
	    nelem = 0
	    if (O_LEN(ap) > 0 && O_TYPE(ap) != TY_CHAR)
		nelem = O_LEN(ap)
	    call xvv_initop (out, nelem, TY_BOOL)

	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALI(ap)
		else
		    call amovi (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_CHAR:
		ch = O_VALC(ap)
		O_VALI(out) = btoi (ch == 'y' || ch == 'Y')


	    case TY_SHORT:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = btoi (O_VALS(ap) != 0)
		else {
		    v_s = 0
		    call abneks (Mems[O_VALP(ap)], v_s, Memi[O_VALP(out)],
			nelem)
		}

	    case TY_INT:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = btoi (O_VALI(ap) != 0)
		else {
		    v_i = 0
		    call abneki (Memi[O_VALP(ap)], v_i, Memi[O_VALP(out)],
			nelem)
		}

	    case TY_LONG:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = btoi (O_VALL(ap) != 0)
		else {
		    v_l = 0
		    call abnekl (Meml[O_VALP(ap)], v_l, Memi[O_VALP(out)],
			nelem)
		}

	    case TY_REAL:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = btoi (O_VALR(ap) != 0.0)
		else {
		    v_r = 0.0
		    call abnekr (Memr[O_VALP(ap)], v_r, Memi[O_VALP(out)],
			nelem)
		}

	    case TY_DOUBLE:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = btoi (O_VALD(ap) != 0.0D0)
		else {
		    v_d = 0.0D0
		    call abnekd (Memd[O_VALP(ap)], v_d, Memi[O_VALP(out)],
			nelem)
		}


	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_SHORT:
	    nelem = 0
	    if (O_LEN(ap) > 0 && O_TYPE(ap) != TY_CHAR)
		nelem = O_LEN(ap)
	    call xvv_initop (out, nelem, TY_SHORT)

	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		if (O_LEN(ap) <= 0)
		    O_VALS(out) = O_VALI(ap)
		else
		    call achtis (Memi[O_VALP(ap)], Mems[O_VALP(out)], nelem)

	    case TY_CHAR:
		ip = O_VALP(ap)
		if (gctod (Memc, ip, v_d) <= 0)
		    O_VALS(out) = 0
		else
		    O_VALS(out) = v_d


	    case TY_SHORT:
		if (O_LEN(ap) <= 0)
		    O_VALS(out) = O_VALS(ap)
		else
		    call achtss (Mems[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_INT:
		if (O_LEN(ap) <= 0)
		    O_VALS(out) = O_VALI(ap)
		else
		    call achtis (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_LONG:
		if (O_LEN(ap) <= 0)
		    O_VALS(out) = O_VALL(ap)
		else
		    call achtls (Meml[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_REAL:
		if (O_LEN(ap) <= 0)
		    O_VALS(out) = O_VALR(ap)
		else
		    call achtrs (Memr[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_DOUBLE:
		if (O_LEN(ap) <= 0)
		    O_VALS(out) = O_VALD(ap)
		else
		    call achtds (Memd[O_VALP(ap)], Memi[O_VALP(out)], nelem)


	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_INT:
	    nelem = 0
	    if (O_LEN(ap) > 0 && O_TYPE(ap) != TY_CHAR)
		nelem = O_LEN(ap)
	    call xvv_initop (out, nelem, TY_INT)

	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALI(ap)
		else
		    call amovi (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_CHAR:
		ip = O_VALP(ap)
		if (gctod (Memc, ip, v_d) <= 0)
		    O_VALI(out) = 0
		else
		    O_VALI(out) = v_d


	    case TY_SHORT:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALS(ap)
		else
		    call achtsi (Mems[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_INT:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALI(ap)
		else
		    call achtii (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_LONG:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALL(ap)
		else
		    call achtli (Meml[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_REAL:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALR(ap)
		else
		    call achtri (Memr[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_DOUBLE:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALD(ap)
		else
		    call achtdi (Memd[O_VALP(ap)], Memi[O_VALP(out)], nelem)


	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_LONG:
	    nelem = 0
	    if (O_LEN(ap) > 0 && O_TYPE(ap) != TY_CHAR)
		nelem = O_LEN(ap)
	    call xvv_initop (out, nelem, TY_LONG)

	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		if (O_LEN(ap) <= 0)
		    O_VALL(out) = O_VALI(ap)
		else
		    call amovi (Memi[O_VALP(ap)], Meml[O_VALP(out)], nelem)

	    case TY_CHAR:
		ip = O_VALP(ap)
		if (gctod (Memc, ip, v_d) <= 0)
		    O_VALL(out) = 0
		else
		    O_VALL(out) = v_d


	    case TY_SHORT:
		if (O_LEN(ap) <= 0)
		    O_VALL(out) = O_VALS(ap)
		else
		    call achtsl (Mems[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_INT:
		if (O_LEN(ap) <= 0)
		    O_VALL(out) = O_VALI(ap)
		else
		    call achtil (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_LONG:
		if (O_LEN(ap) <= 0)
		    O_VALL(out) = O_VALL(ap)
		else
		    call achtll (Meml[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_REAL:
		if (O_LEN(ap) <= 0)
		    O_VALL(out) = O_VALR(ap)
		else
		    call achtrl (Memr[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_DOUBLE:
		if (O_LEN(ap) <= 0)
		    O_VALL(out) = O_VALD(ap)
		else
		    call achtdl (Memd[O_VALP(ap)], Memi[O_VALP(out)], nelem)


	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_NINT:
	    nelem = 0
	    if (O_LEN(ap) > 0 && O_TYPE(ap) != TY_CHAR)
		nelem = O_LEN(ap)
	    call xvv_initop (out, nelem, TY_INT)

	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALI(ap)
		else
		    call amovi (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_CHAR:
		ip = O_VALP(ap)
		if (gctod (Memc, ip, v_d) <= 0)
		    O_VALI(out) = 0
		else
		    O_VALI(out) = nint (v_d)


	    case TY_SHORT:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALS(ap)
		else
		    call achtsi (Mems[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_INT:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALI(ap)
		else
		    call achtii (Memi[O_VALP(ap)], Memi[O_VALP(out)], nelem)

	    case TY_LONG:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = O_VALL(ap)
		else
		    call achtli (Meml[O_VALP(ap)], Memi[O_VALP(out)], nelem)



	    case TY_REAL:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = nint (O_VALR(ap))
		else {
		    do i = 1, nelem
			Memi[O_VALP(out)+i-1] = nint (Memr[O_VALP(ap)+i-1])
		}

	    case TY_DOUBLE:
		if (O_LEN(ap) <= 0)
		    O_VALI(out) = nint (O_VALD(ap))
		else {
		    do i = 1, nelem
			Memi[O_VALP(out)+i-1] = nint (Memd[O_VALP(ap)+i-1])
		}


	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_REAL:
	    nelem = 0
	    if (O_LEN(ap) > 0 && O_TYPE(ap) != TY_CHAR)
		nelem = O_LEN(ap)
	    call xvv_initop (out, nelem, TY_REAL)

	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		if (O_LEN(ap) <= 0)
		    O_VALR(out) = O_VALI(ap)
		else
		    call achtir (Memi[O_VALP(ap)], Memr[O_VALP(out)], nelem)

	    case TY_CHAR:
		ip = O_VALP(ap)
		if (gctod (Memc, ip, v_d) <= 0)
		    O_VALR(out) = 0
		else
		    O_VALR(out) = v_d


	    case TY_SHORT:
		if (O_LEN(ap) <= 0)
		    O_VALR(out) = O_VALS(ap)
		else
		    call achtsr (Mems[O_VALP(ap)], Memr[O_VALP(out)], nelem)

	    case TY_INT:
		if (O_LEN(ap) <= 0)
		    O_VALR(out) = O_VALI(ap)
		else
		    call achtir (Memi[O_VALP(ap)], Memr[O_VALP(out)], nelem)

	    case TY_LONG:
		if (O_LEN(ap) <= 0)
		    O_VALR(out) = O_VALL(ap)
		else
		    call achtlr (Meml[O_VALP(ap)], Memr[O_VALP(out)], nelem)

	    case TY_REAL:
		if (O_LEN(ap) <= 0)
		    O_VALR(out) = O_VALR(ap)
		else
		    call achtrr (Memr[O_VALP(ap)], Memr[O_VALP(out)], nelem)

	    case TY_DOUBLE:
		if (O_LEN(ap) <= 0)
		    O_VALR(out) = O_VALD(ap)
		else
		    call achtdr (Memd[O_VALP(ap)], Memr[O_VALP(out)], nelem)


	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_DOUBLE:
	    nelem = 0
	    if (O_LEN(ap) > 0 && O_TYPE(ap) != TY_CHAR)
		nelem = O_LEN(ap)
	    call xvv_initop (out, nelem, TY_DOUBLE)

	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		if (O_LEN(ap) <= 0)
		    O_VALD(out) = O_VALI(ap)
		else
		    call achtid (Memi[O_VALP(ap)], Memd[O_VALP(out)], nelem)

	    case TY_CHAR:
		ip = O_VALP(ap)
		if (gctod (Memc, ip, v_d) <= 0)
		    O_VALD(out) = 0
		else
		    O_VALD(out) = v_d


	    case TY_SHORT:
		if (O_LEN(ap) <= 0)
		    O_VALD(out) = O_VALS(ap)
		else
		    call achtsd (Mems[O_VALP(ap)], Memd[O_VALP(out)], nelem)

	    case TY_INT:
		if (O_LEN(ap) <= 0)
		    O_VALD(out) = O_VALI(ap)
		else
		    call achtid (Memi[O_VALP(ap)], Memd[O_VALP(out)], nelem)

	    case TY_LONG:
		if (O_LEN(ap) <= 0)
		    O_VALD(out) = O_VALL(ap)
		else
		    call achtld (Meml[O_VALP(ap)], Memd[O_VALP(out)], nelem)

	    case TY_REAL:
		if (O_LEN(ap) <= 0)
		    O_VALD(out) = O_VALR(ap)
		else
		    call achtrd (Memr[O_VALP(ap)], Memd[O_VALP(out)], nelem)

	    case TY_DOUBLE:
		if (O_LEN(ap) <= 0)
		    O_VALD(out) = O_VALD(ap)
		else
		    call achtdd (Memd[O_VALP(ap)], Memd[O_VALP(out)], nelem)


	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	case F_STR:
	    optype = TY_CHAR
	    if (O_TYPE(ap) == TY_CHAR)
		nelem = strlen (O_VALC(ap))
	    else
		nelem = MAX_DIGITS
	    call xvv_initop (out, nelem, TY_CHAR)

	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		call sprintf (O_VALC(out), nelem, "%b")
		    call pargi (O_VALI(ap))
	    case TY_CHAR:
		call sprintf (O_VALC(out), nelem, "%s")
		    call pargstr (O_VALC(ap))

	    case TY_SHORT:
		call sprintf (O_VALC(out), nelem, "%d")
		    call pargs (O_VALS(ap))

	    case TY_INT:
		call sprintf (O_VALC(out), nelem, "%d")
		    call pargi (O_VALI(ap))

	    case TY_LONG:
		call sprintf (O_VALC(out), nelem, "%d")
		    call pargl (O_VALL(ap))


	    case TY_REAL:
		call sprintf (O_VALC(out), nelem, "%g")
		    call pargr (O_VALR(ap))

	    case TY_DOUBLE:
		call sprintf (O_VALC(out), nelem, "%g")
		    call pargd (O_VALD(ap))

	    default:
		call xvv_error1 (s_badtype, fcn)
	    }

	default:
	    call xvv_error ("callfcn: unknown function type")
	}

free_
	# Free any storage used by the argument list operands.
	do i = 1, nargs
	    call xvv_freeop (args[i])

	call sfree (sp)
end


# XVV_STARTARGLIST -- Allocate an argument list descriptor to receive
# arguments as a function call is parsed.  We are called with either
# zero or one arguments.  The argument list descriptor is pointed to by
# a ficticious operand.  The descriptor itself contains a count of the
# number of arguments, an array of pointers to the operand structures,
# as well as storage for the operand structures.  The operands must be
# stored locally since the parser will discard its copy of the operand
# structure for each argument as the associated grammar rule is reduced.

procedure xvv_startarglist (arg, out)

pointer	arg			#I pointer to first argument, or NULL
pointer	out			#I output operand pointing to arg descriptor

pointer	ap
errchk	xvv_initop

begin
	call xvv_initop (out, LEN_ARGSTRUCT, TY_STRUCT)
	ap = O_VALP(out)

	if (arg == NULL)
	    A_NARGS(ap) = 0
	else {
	    A_NARGS(ap) = 1
	    A_ARGP(ap,1) = A_OPS(ap)
	    YYMOVE (arg, A_OPS(ap))
	}
end


# XVV_ADDARG -- Add an argument to the argument list for a function call.

procedure xvv_addarg (arg, arglist, out)

pointer	arg			#I pointer to argument to be added
pointer	arglist			#I pointer to operand pointing to arglist
pointer	out			#I output operand pointing to arg descriptor

pointer	ap, o
int	nargs

begin
	ap = O_VALP(arglist)

	nargs = A_NARGS(ap) + 1
	A_NARGS(ap) = nargs
	if (nargs > MAX_ARGS)
	    call xvv_error ("too many function arguments")

	o = A_OPS(ap) + ((nargs - 1) * LEN_OPERAND)
	A_ARGP(ap,nargs) = o
	YYMOVE (arg, o)

	YYMOVE (arglist, out)
end


# XVV_ERROR1 -- Take an error action, formatting an error message with one
# format string plus one string argument.

procedure xvv_error1 (fmt, arg)

char	fmt[ARB]		#I printf format string
char	arg[ARB]		#I string argument

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sprintf (Memc[buf], SZ_LINE, fmt)
	    call pargstr (arg)

	call xvv_error (Memc[buf])
	call sfree (sp)
end


# XVV_ERROR2 -- Take an error action, formatting an error message with one
# format string plus one string argument and one integer argument.

procedure xvv_error2 (fmt, arg1, arg2)

char	fmt[ARB]		#I printf format string
char	arg1[ARB]		#I string argument
int	arg2			#I integer argument

pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sprintf (Memc[buf], SZ_LINE, fmt)
	    call pargstr (arg1)
	    call pargi (arg2)

	call xvv_error (Memc[buf])
	call sfree (sp)
end


# XVV_ERROR -- Take an error action, given an error message string as the
# sole argument.

procedure xvv_error (errmsg)

char	errmsg[ARB]			#I error message

begin
	call error (1, errmsg)
end


# XVV_CHTYPE -- Change the datatype of an operand.  The input and output
# operands may be the same.

procedure xvv_chtype (o1, o2, dtype)

pointer	o1		#I input operand
pointer	o2		#I output operand
int	dtype		#I new datatype

short	v_s
int	v_i
long	v_l
real	v_r
double	v_d
pointer	vp, ip, op
bool	float, freeval
int	old_type, nelem, ch

pointer	coerce()
int	sizeof(), btoi(), gctod()
string	s_badtype "chtype: invalid operand type"

begin
	old_type = O_TYPE(o1)
	nelem = O_LEN(o1)

	# No type conversion needed?
	if (old_type == dtype) {
	    if (o1 != o2) {
		if (nelem <= 0)
		    YYMOVE (o1, o2)
		else {
		    call xvv_initop (o2, nelem, old_type)
		    call amovc (O_VALC(o1), O_VALC(o2), nelem * sizeof(dtype))
		}
	    }
	    return
	}

	if (nelem <= 0) {
	    # Scalar input operand.

	    O_TYPE(o2) = dtype
	    O_LEN(o2) = 0
	    float = false

	    # Read the old value into a local variable of type long or double.
	    switch (old_type) {
	    case TY_BOOL:
		v_l = O_VALI(o1)
	    case TY_CHAR:
		v_l = 0  # null string?

	    case TY_SHORT:
		v_l = O_VALS(o1)

	    case TY_INT:
		v_l = O_VALI(o1)

	    case TY_LONG:
		v_l = O_VALL(o1)


	    case TY_REAL:
		v_d = O_VALR(o1)
		float = true

	    case TY_DOUBLE:
		v_d = O_VALD(o1)
		float = true

	    default:
		call xvv_error (s_badtype)
	    }

	    # Set the value of the output operand.
	    switch (dtype) {
	    case TY_BOOL:
		if (float)
		    O_VALI(o2) = btoi (v_d != 0)
		else
		    O_VALI(o2) = btoi (v_l != 0)
	    case TY_CHAR:
		call xvv_initop (o2, MAX_DIGITS, TY_CHAR)
		if (float) {
		    call sprintf (O_VALC(o2), MAX_DIGITS, "%g")
			call pargd (v_d)
		} else {
		    call sprintf (O_VALC(o2), MAX_DIGITS, "%d")
			call pargl (v_l)
		}

	    case TY_SHORT:
		if (float)
		    O_VALS(o2) = v_d
		else
		    O_VALS(o2) = v_l

	    case TY_INT:
		if (float)
		    O_VALI(o2) = v_d
		else
		    O_VALI(o2) = v_l

	    case TY_LONG:
		if (float)
		    O_VALL(o2) = v_d
		else
		    O_VALL(o2) = v_l


	    case TY_REAL:
		if (float)
		    O_VALR(o2) = v_d
		else
		    O_VALR(o2) = v_l

	    case TY_DOUBLE:
		if (float)
		    O_VALD(o2) = v_d
		else
		    O_VALD(o2) = v_l

	    default:
		call xvv_error (s_badtype)
	    }

	} else {
	    # Vector input operand.

	    # Save a pointer to the input operand data vector, to avoid it
	    # getting clobbered if O1 and O2 are the same operand.

	    vp = O_VALP(o1)

	    # If we have a char string input operand the output numeric
	    # operand can only be a scalar.  If we have a char string output
	    # operand nelem is the length of the string.

	    if (old_type == TY_CHAR)
		nelem = 0
	    else if (dtype == TY_CHAR)
		nelem = MAX_DIGITS

	    # Initialize the output operand O2.  The freeval flag is cleared
	    # cleared to keep the initop from freeing the input operand array,
	    # inherited when the input operand is copied (or when the input
	    # and output operands are the same).  We free the old operand
	    # array manually below.

	    if (o1 != o2)
		YYMOVE (o1, o2)
	    freeval = (and (O_FLAGS(o1), O_FREEVAL) != 0)
	    O_FLAGS(o2) = and (O_FLAGS(o2), not(O_FREEVAL))
	    call xvv_initop (o2, nelem, dtype)

	    # Write output value.
	    switch (dtype) {
	    case TY_BOOL:
		if (old_type == TY_CHAR) {
		    ch = Memc[vp]
		    O_VALI(o2) = btoi (ch == 'y' || ch == 'Y')
		} else {
		    switch (old_type) {

		    case TY_SHORT:
			v_s = 0
			call abneks (Mems[vp], v_s, Memi[O_VALP(o2)], nelem)

		    case TY_INT:
			v_i = 0
			call abneki (Memi[vp], v_i, Memi[O_VALP(o2)], nelem)

		    case TY_LONG:
			v_l = 0
			call abnekl (Meml[vp], v_l, Memi[O_VALP(o2)], nelem)

		    case TY_REAL:
			v_r = 0.0
			call abnekr (Memr[vp], v_r, Memi[O_VALP(o2)], nelem)

		    case TY_DOUBLE:
			v_d = 0.0D0
			call abnekd (Memd[vp], v_d, Memi[O_VALP(o2)], nelem)

		    default:
			call xvv_error (s_badtype)
		    }
		}

	    case TY_CHAR:
		call xvv_error (s_badtype)

	    case TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE:
		switch (old_type) {
		case TY_BOOL:
		    op = coerce (O_VALP(o2), O_TYPE(o2), TY_CHAR)
		    call achti (Memi[vp], Memc[op], nelem, dtype)
		case TY_CHAR:
		    ip = vp
		    if (gctod (Memc, ip, v_d) <= 0)
			v_d = 0
		    switch (dtype) {

		    case TY_SHORT:
			O_VALS(o2) = v_d

		    case TY_INT:
			O_VALI(o2) = v_d

		    case TY_LONG:
			O_VALL(o2) = v_d

		    case TY_REAL:
			O_VALR(o2) = v_d

		    case TY_DOUBLE:
			O_VALD(o2) = v_d

		    }

		case TY_SHORT:
		    op = coerce (O_VALP(o2), O_TYPE(o2), TY_CHAR)
		    call achts (Mems[vp], Memc[op], nelem, dtype)

		case TY_INT:
		    op = coerce (O_VALP(o2), O_TYPE(o2), TY_CHAR)
		    call achti (Memi[vp], Memc[op], nelem, dtype)

		case TY_LONG:
		    op = coerce (O_VALP(o2), O_TYPE(o2), TY_CHAR)
		    call achtl (Meml[vp], Memc[op], nelem, dtype)

		case TY_REAL:
		    op = coerce (O_VALP(o2), O_TYPE(o2), TY_CHAR)
		    call achtr (Memr[vp], Memc[op], nelem, dtype)

		case TY_DOUBLE:
		    op = coerce (O_VALP(o2), O_TYPE(o2), TY_CHAR)
		    call achtd (Memd[vp], Memc[op], nelem, dtype)

		default:
		    call xvv_error (s_badtype)
		}
	    default:
		call xvv_error (s_badtype)
	    }

	    # Free old operand value.
	    if (freeval)
		call mfree (vp, old_type)
	}
end


# XVV_INITOP -- Initialize an operand, providing storage for an operand value
# of the given size and type.

procedure xvv_initop (o, o_len, o_type)

pointer	o		#I pointer to operand structure
int	o_len		#I length of operand (zero if scalar)
int	o_type		#I datatype of operand

begin
	O_LEN(o) = 0
	call xvv_makeop (o, o_len, o_type)
end


# XVV_MAKEOP -- Set up the operand structure.  If the operand structure has
# already been initialized and array storage allocated, free the old array.

procedure xvv_makeop (o, o_len, o_type)

pointer	o		#I pointer to operand structure
int	o_len		#I length of operand (zero if scalar)
int	o_type		#I datatype of operand

errchk	malloc

begin
	# Free old array storage if any.
	if (O_TYPE(o) != 0 && O_LEN(o) > 0)
	    if (and (O_FLAGS(o), O_FREEVAL) != 0) {
		if (O_TYPE(o) == TY_BOOL)
		    call mfree (O_VALP(o), TY_INT)
		else
		    call mfree (O_VALP(o), O_TYPE(o))
		O_LEN(o) = 0
	    }

	# Set new operand type.
	O_TYPE(o) = o_type

	# Allocate array storage if nonscalar operand.
	if (o_len > 0) {
	    if (o_type == TY_BOOL)
		call malloc (O_VALP(o), o_len, TY_INT)
	    else
		call malloc (O_VALP(o), o_len, o_type)
	    O_LEN(o) = o_len
	}

	O_FLAGS(o) = O_FREEVAL
end


# XVV_FREEOP -- Reinitialize an operand structure, i.e., free any associated
# array storage and clear the operand datatype field, but do not free the
# operand structure itself (which may be only a segment of an array and not
# a separately allocated structure).

procedure xvv_freeop (o)

pointer	o		#I pointer to operand structure

begin
	# Free old array storage if any.
	if (O_TYPE(o) != 0 && O_LEN(o) > 0)
	    if (and (O_FLAGS(o), O_FREEVAL) != 0) {
		if (O_TYPE(o) == TY_BOOL)
		    call mfree (O_VALP(o), TY_INT)
		else
		    call mfree (O_VALP(o), O_TYPE(o))
		O_LEN(o) = 0
	    }

	# Either free operand struct or clear the operand type to mark
	# operand invalid.

	if (and (O_FLAGS(o), O_FREEOP) != 0)
	    call mfree (o, TY_STRUCT)
	else
	    O_TYPE(o) = 0
end


# XVV_LOADSYMBOLS -- Load a list of symbol names into a symbol table.  Each
# symbol is tagged with an integer code corresponding to its sequence number
# in the symbol list.

pointer procedure xvv_loadsymbols (s)

char	s[ARB]			#I symbol list "|sym1|sym2|...|"

int	delim, symnum, ip
pointer	sp, symname, st, sym, op
pointer	stopen(), stenter()

begin
	call smark (sp)
	call salloc (symname, SZ_FNAME, TY_CHAR)

	st = stopen ("evvexpr", LEN_INDEX, LEN_STAB, LEN_SBUF)
	delim = s[1]
	symnum = 0

	for (ip=2;  s[ip] != EOS;  ip=ip+1) {
	    op = symname
	    while (s[ip] != delim && s[ip] != EOS) {
		Memc[op] = s[ip]
		op = op + 1
		ip = ip + 1
	    }
	    Memc[op] = EOS
	    symnum = symnum + 1

	    if (op > symname && IS_ALPHA(Memc[symname])) {
		sym = stenter (st, Memc[symname], LEN_SYM)
		SYM_CODE(sym) = symnum
	    }
	}

	call sfree (sp)
	return (st)
end


# XVV_NULL -- Return a null value to be used when a computation cannot be
# performed and range checking is enabled.  Perhaps we should permit a user
# specified value here, however this doesn't really work in an expression
# evaluator since the value generated may be used in subsequent calculations
# and hence may change.  If more careful treatment of out of range values
# is needed a conditional expression can be used in which case the value
# we return here is ignored (but still needed to avoid a hardware exception
# when computing a vector).


short procedure xvv_nulls (ignore)
short	ignore			#I ignored
begin
	return (0)
end

int procedure xvv_nulli (ignore)
int	ignore			#I ignored
begin
	return (0)
end

long procedure xvv_nulll (ignore)
long	ignore			#I ignored
begin
	return (0)
end

real procedure xvv_nullr (ignore)
real	ignore			#I ignored
begin
	return (0.0)
end

double procedure xvv_nulld (ignore)
double	ignore			#I ignored
begin
	return (0.0D0)
end

