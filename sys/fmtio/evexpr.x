
# line 2 "evexpr.y"
include	<lexnum.h>
include	<ctype.h>
include	<mach.h>
include	<evexpr.h>

define	YYMAXDEPTH	64		# parser stack length
define	MAX_ARGS	16		# max args in a function call
define	yyparse		xev_parse

define	DTOR		(($1)/57.2957795)
define	RTOD		(($1)*57.2957795)

# Arglist structure.
define	LEN_ARGSTRUCT	(1+MAX_ARGS+(MAX_ARGS*LEN_OPERAND))
define	A_NARGS		Memi[$1]	# number of arguments
define	A_ARGP		Memi[$1+$2]	# array of pointers to operand structs
define	A_OPS		($1+MAX_ARGS+1)	# offset to operand storage area

# Intrinsic functions.

define	KEYWORDS	"|abs|acos|asin|atan|atan2|bool|cos|exp|int|log|log10|\
			 |max|min|mod|nint|real|sin|sqrt|str|tan|"

define	F_ABS		01		# function codes
define	F_ACOS		02
define	F_ASIN		03
define	F_ATAN		04
define	F_ATAN2		05
define	F_BOOL		06
define	F_COS		07
define	F_EXP		08
define	F_INT		09
define	F_LOG		10
define	F_LOG10		11
	# newline	12
define	F_MAX		13
define	F_MIN		14
define	F_MOD		15
define	F_NINT		16
define	F_REAL		17
define	F_SIN		18
define	F_SQRT		19
define	F_STR		20
define	F_TAN 		21


# EVEXPR -- Evaluate an expression.  This is the top level procedure, and the
# only externally callable entry point.  Input consists of the expression to
# be evaluated (a string) and, optionally, user procedures for fetching
# external operands and executing external functions.  Output is a pointer to
# an operand structure containing the computed value of the expression.
# The output operand structure is dynamically allocated by EVEXPR and must be
# freed by the user.
#
# N.B.: this is not intended to be an especially efficient procedure.  Rather,
# this is a high level, easy to use procedure, intended to provide greater
# flexibility in the parameterization of applications programs.

pointer procedure evexpr (expr, getop_epa, ufcn_epa)

char	expr[ARB]		# expression to be evaluated
int	getop_epa		# user supplied get operand procedure
int	ufcn_epa		# user supplied function call procedure

int	junk
bool	debug
pointer	sp, ip
extern	xev_gettok()
int	strlen(), xev_parse()

errchk	xev_parse, calloc
include	"evexpr.com"
data	debug /false/

begin
	call smark (sp)

	# Set user function entry point addresses.
	ev_getop = getop_epa
	ev_ufcn  = ufcn_epa

	# Allocate an operand struct for the expression value.
	call calloc (ev_oval, LEN_OPERAND, TY_STRUCT)

	# Make a local copy of the input string.
	call salloc (ip, strlen(expr), TY_CHAR)
	call strcpy (expr, Memc[ip], ARB)

	# Evaluate the expression.  The expression value is copied into the
	# output operand structure by XEV_PARSE, given the operand pointer
	# passed in common.  A common must be used since the standard parser
	# subroutine has a fixed calling sequence.

	junk = xev_parse (ip, debug, xev_gettok)

	call sfree (sp)
	return (ev_oval)
end

define	CONSTANT		257
define	IDENTIFIER		258
define	NEWLINE		259
define	YYEOS		260
define	PLUS		261
define	MINUS		262
define	STAR		263
define	SLASH		264
define	EXPON		265
define	CONCAT		266
define	QUEST		267
define	COLON		268
define	LT		269
define	GT		270
define	LE		271
define	EQ		272
define	NE		273
define	SE		274
define	AND		275
define	OR		276
define	NOT		277
define	AT		278
define	GE		279
define	UMINUS		280
define	yyclearin	yychar = -1
define	yyerrok		yyerrflag = 0
define	YYMOVE		call amovi (Memi[$1], Memi[$2], YYOPLEN)
define	YYERRCODE	256



# XEV_UNOP -- Unary operation.  Perform the indicated unary operation on the
# input operand, returning the result as the output operand.

procedure xev_unop (opcode, in, out)

int	opcode			# operation to be performed
pointer	in			# input operand
pointer	out			# output operand

errchk	xev_error
define	badsw_ 91

begin
	call xev_initop (out, 0, O_TYPE(in))

	switch (opcode) {
	case MINUS:
	    # Unary negation.
	    switch (O_TYPE(in)) {
	    case TY_BOOL, TY_CHAR:
		call xev_error ("negation of a nonarithmetic operand")
	    case TY_INT:
		O_VALI(out) = -O_VALI(in)
	    case TY_REAL:
		O_VALR(out) = -O_VALR(in)
	    default:
		goto badsw_
	    }

	case NOT:
	    switch (O_TYPE(in)) {
	    case TY_BOOL:
		O_VALB(out) = !O_VALB(in)
	    case TY_CHAR, TY_INT, TY_REAL:
		call xev_error ("not of a nonlogical")
	    default:
		goto badsw_
	    }

	default:
badsw_	    call xev_error ("bad switch in unop")
	}
end


# XEV_BINOP -- Binary operation.  Perform the indicated arithmetic binary
# operation on the two input operands, returning the result as the output
# operand.

procedure xev_binop (opcode, in1, in2, out)

int	opcode			# operation to be performed
pointer	in1, in2		# input operands
pointer	out			# output operand

real	r1, r2
int	i1, i2, dtype, nchars
int	xev_newtype(), strlen()
errchk	xev_newtype

begin
	# Set the datatype of the output operand, taking an error action if
	# the operands have incompatible datatypes.

	dtype = xev_newtype (O_TYPE(in1), O_TYPE(in2))
	call xev_initop (out, 0, dtype)

	switch (dtype) {
	case TY_BOOL:
	    call xev_error ("operation illegal for boolean operands")
	case TY_CHAR:
	    if (opcode != CONCAT)
		call xev_error ("operation illegal for string operands")
	case TY_INT:
	    i1 = O_VALI(in1)
	    i2 = O_VALI(in2)
	case TY_REAL:
	    if (O_TYPE(in1) == TY_INT)
		r1 = O_VALI(in1)
	    else
		r1 = O_VALR(in1)
	    if (O_TYPE(in2) == TY_INT)
		r2 = O_VALI(in2)
	    else
		r2 = O_VALR(in2)
	default:
	    call xev_error ("unknown datatype code in binop")
	}

	# Perform the operation.
	switch (opcode) {
	case PLUS:
	    if (dtype == TY_INT)
		O_VALI(out) = i1 + i2
	    else
		O_VALR(out) = r1 + r2

	case MINUS:
	    if (dtype == TY_INT)
		O_VALI(out) = i1 - i2
	    else
		O_VALR(out) = r1 - r2

	case STAR:
	    if (dtype == TY_INT)
		O_VALI(out) = i1 * i2
	    else
		O_VALR(out) = r1 * r2

	case SLASH:
	    if (dtype == TY_INT)
		O_VALI(out) = i1 / i2
	    else
		O_VALR(out) = r1 / r2

	case EXPON:
	    if (dtype == TY_INT)
		O_VALI(out) = i1 ** i2
	    else if (O_TYPE(in1) == TY_REAL && O_TYPE(in2) == TY_INT)
		O_VALR(out) = r1 ** (O_VALI(in2))
	    else
		O_VALR(out) = r1 ** r2

	case CONCAT:
	    if (dtype != TY_CHAR)
		call xev_error ("concatenation of a nonstring operand")
	    nchars = strlen (O_VALC(in1)) + strlen (O_VALC(in2))
	    call xev_makeop (out, nchars, TY_CHAR)
	    call strcpy (O_VALC(in1), O_VALC(out), ARB)
	    call strcat (O_VALC(in2), O_VALC(out), ARB)
	    call xev_freeop (in1)
	    call xev_freeop (in2)

	default:
	    call xev_error ("bad switch in binop")
	}
end


# XEV_BOOLOP -- Boolean binary operations.  Perform the indicated boolean binary
# operation on the two input operands, returning the result as the output
# operand.

procedure xev_boolop (opcode, in1, in2, out)

int	opcode			# operation to be performed
pointer	in1, in2		# input operands
pointer	out			# output operand

bool	result
real	r1, r2
int	i1, i2, dtype
int	xev_newtype(), xev_patmatch(), strncmp()
errchk	xev_newtype, xev_error
define	badsw_ 91

begin
	# Set the datatype of the output operand, taking an error action if
	# the operands have incompatible datatypes.

	dtype = xev_newtype (O_TYPE(in1), O_TYPE(in2))
	call xev_initop (out, 0, dtype)

	switch (opcode) {
	case AND, OR:
	    if (dtype != TY_BOOL)
		call xev_error ("AND or OR of nonlogical")
	case LT, GT, LE, GE:
	    if (dtype == TY_BOOL)
		call xev_error ("order comparison of a boolean operand")
	}

	if (dtype == TY_INT) {
	    i1 = O_VALI(in1)
	    i2 = O_VALI(in2)
	} else if (dtype == TY_REAL) {
	    if (O_TYPE(in1) == TY_INT) {
		i1 = O_VALI(in1)
		r1 = i1
	    } else
		r1 = O_VALR(in1)
	    if (O_TYPE(in2) == TY_INT) {
		i2 = O_VALI(in2)
		r2 = i2
	    } else
		r2 = O_VALR(in2)
	}

	# Perform the operation.
	switch (opcode) {
	case AND:
	    result = O_VALB(in1) && O_VALB(in2)
	case OR:
	    result = O_VALB(in1) || O_VALB(in2)

	case LT, GE:
	    if (dtype == TY_INT)
		result = i1 < i2
	    else if (dtype == TY_REAL)
		result = r1 < r2
	    else
		result = strncmp (O_VALC(in1), O_VALC(in2), ARB) < 0
	    if (opcode == GE)
		result = !result

	case GT, LE:
	    if (dtype == TY_INT)
		result = i1 > i2
	    else if (dtype == TY_REAL)
		result = r1 > r2
	    else
		result = strncmp (O_VALC(in1), O_VALC(in2), ARB) > 0
	    if (opcode == LE)
		result = !result

	case EQ, SE, NE:
	    switch (dtype) {
	    case TY_BOOL:
		if (O_VALB(in1))
		    result =  O_VALB(in2)
		else
		    result = !O_VALB(in2)
	    case TY_CHAR:
		if (opcode == SE)
		    result = xev_patmatch (O_VALC(in1), O_VALC(in2)) > 0
		else
		    result = strncmp (O_VALC(in1), O_VALC(in2), ARB) == 0
	    case TY_INT:
		result = i1 == i2
	    case TY_REAL:
		result = r1 == r2
	    default:
		goto badsw_
	    }
	    if (opcode == NE)
		result = !result

	default:
badsw_	    call xev_error ("bad switch in boolop")
	}

	call xev_makeop (out, 0, TY_BOOL)
	O_VALB(out) = result

	# Free storage if there were any string type input operands.
	call xev_freeop (in1)
	call xev_freeop (in2)
end


# XEV_PATMATCH -- Match a string against a pattern, returning the patmatch
# index if the string matches.  The pattern may contain any of the conventional
# pattern matching metacharacters.  Closure (i.e., "*") is mapped to "?*".

int procedure xev_patmatch (str, pat)

char	str[ARB]		# operand string
char	pat[ARB]		# pattern

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


# XEV_NEWTYPE -- Get the datatype of a binary operation, given the datatypes
# of the two input operands.  An error action is taken if the datatypes are
# incompatible, e.g., boolean and anything else or string and anything else.

int procedure xev_newtype (type1, type2)

int	type1, type2
int	newtype, p, q, i
int	tyindex[NTYPES], ttbl[NTYPES*NTYPES]
data	tyindex	/TY_BOOL, TY_CHAR,  TY_INT,  TY_REAL/
data	(ttbl(i),i=1,4)		/TY_BOOL,       0,       0,        0/
data	(ttbl(i),i=5,8)		/      0, TY_CHAR,       0,        0/
data	(ttbl(i),i=9,12)	/      0,       0,  TY_INT,  TY_REAL/
data	(ttbl(i),i=13,16)	/      0,       0, TY_REAL,  TY_REAL/

begin
	do i = 1, NTYPES {
	    if (tyindex[i] == type1)
		p = i
	    if (tyindex[i] == type2)
		q = i
	}

	newtype = ttbl[(p-1)*NTYPES+q]
	if (newtype == 0)
	    call xev_error ("operands have incompatible types")
	else
	    return (newtype)
end


# XEV_QUEST -- Conditional expression.  If the condition operand is true
# return the first (true) operand, else return the second (false) operand.

procedure xev_quest (cond, trueop, falseop, out)

pointer	cond			# pointer to condition operand
pointer	trueop, falseop		# pointer to true,false operands
pointer	out			# pointer to output operand
errchk	xev_error

begin
	if (O_TYPE(cond) != TY_BOOL)
	    call xev_error ("nonboolean condition operand")

	if (O_VALB(cond)) {
	    YYMOVE (trueop, out)
	    call xev_freeop (falseop)
	} else {
	    YYMOVE (falseop, out)
	    call xev_freeop (trueop)
	}
end


# XEV_CALLFCN -- Call an intrinsic function.  If the function named is not
# one of the standard intrinsic functions, call an external user function
# if a function call procedure was supplied.

procedure xev_callfcn (fcn, args, nargs, out)

char	fcn[ARB]		# function to be called
pointer	args[ARB]		# pointer to arglist descriptor
int	nargs			# number of arguments
pointer	out			# output operand (function value)

real	rresult, rval[2], rtemp
int	iresult, ival[2], type[2], optype, oplen, itemp
int	opcode, v_nargs, i
pointer	sp, buf, ap
include	"evexpr.com"

bool	strne()
int	strdic(), strlen()
errchk	zcall4, xev_error1, xev_error2, malloc
string	keywords KEYWORDS
define	badtype_ 91
define	free_ 92

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	oplen = 0

	# Lookup the function name in the dictionary.  An exact match is
	# required (strdic permits abbreviations).

	opcode = strdic (fcn, Memc[buf], SZ_FNAME, keywords)
	if (opcode > 0 && strne(fcn,Memc[buf]))
	    opcode = 0

	# If the function named is not a standard one and the user has supplied
	# the entry point of an external function evaluation procedure, call
	# the user procedure to evaluate the function, otherwise abort.

	if (opcode <= 0)
	    if (ev_ufcn != NULL) {
		call zcall4 (ev_ufcn, fcn, args, nargs, out)
		goto free_
	    } else
		call xev_error1 ("unknown function `%s' called", fcn)

	# Verify correct number of arguments.
	switch (opcode) {
	case F_MOD:
	    v_nargs = 2
	case F_MAX, F_MIN, F_ATAN, F_ATAN2:
	    v_nargs = -1
	default:
	    v_nargs = 1
	}

	if (v_nargs > 0 && nargs != v_nargs)
	    call xev_error2 ("function `%s' requires %d arguments",
		fcn, v_nargs)
	else if (v_nargs < 0 && nargs < abs(v_nargs))
	    call xev_error2 ("function `%s' requires at least %d arguments",
		fcn, abs(v_nargs))

	# Verify datatypes.
	if (opcode != F_STR && opcode != F_BOOL) {
	    optype = TY_REAL
	    do i = 1, min(2,nargs) {
		switch (O_TYPE(args[i])) {
		case TY_INT:
		    ival[i] = O_VALI(args[i])
		    rval[i] = ival[i]
		    type[i] = TY_INT
		case TY_REAL:
		    rval[i] = O_VALR(args[i])
		    ival[i] = nint (rval[i])
		    type[i] = TY_REAL
		default:
		    goto badtype_
		}
	    }
	}

	# Evaluate the function.

	ap = args[1]

	switch (opcode) {
	case F_ABS:
	    if (type[1] == TY_INT) {
		iresult = abs (ival[1])
		optype = TY_INT
	    } else
		rresult = abs (rval[1])

	case F_ACOS:
	    rresult = RTOD (acos (rval[1]))
	case F_ASIN:
	    rresult = RTOD (asin (rval[1]))
	case F_COS:
	    rresult =   cos (DTOR (rval[1]))
	case F_EXP:
	    rresult =   exp (rval[1])
	case F_LOG:
	    rresult =   log (rval[1])
	case F_LOG10:
	    rresult = log10 (rval[1])
	case F_SIN:
	    rresult =   sin (DTOR (rval[1]))
	case F_SQRT:
	    rresult =  sqrt (rval[1])
	case F_TAN:
	    rresult =   tan (DTOR (rval[1]))

	case F_ATAN, F_ATAN2:
	    if (nargs == 1)
		rresult = RTOD (atan (rval[1]))
	    else
		rresult = RTOD (atan2 (rval[1], rval[2]))

	case F_MOD:
	    if (type[1] == TY_REAL || type[2] == TY_REAL)
		rresult = mod (rval[1], rval[2])
	    else {
		iresult = mod (ival[1], ival[2])
		optype = TY_INT
	    }
		
	case F_NINT:
	    iresult = nint (rval[1])
	    optype = TY_INT

	case F_MAX, F_MIN:
	    # Determine datatype of result.
	    optype = TY_INT
	    do i = 1, nargs
		if (O_TYPE(args[i]) == TY_REAL)
		    optype = TY_REAL
		else if (O_TYPE(args[i]) != TY_INT)
		    goto badtype_

	    # Compute result.
	    if (optype == TY_INT) {
		iresult = O_VALI(ap)
		do i = 2, nargs {
		    itemp = O_VALI(args[i])
		    if (opcode == F_MAX)
			iresult = max (iresult, itemp)
		    else
			iresult = min (iresult, itemp)
		}

	    } else {
		if (O_TYPE(ap) == TY_INT)
		    rresult = O_VALI(ap)
		else
		    rresult = O_VALR(ap)

		do i = 2, nargs {
		    if (O_TYPE(args[i]) == TY_INT)
			rtemp = O_VALI(args[i])
		    else
			rtemp = O_VALR(args[i])
		    if (opcode == F_MAX)
			rresult = max (rresult, rtemp)
		    else
			rresult = min (rresult, rtemp)
		}
	    }

	case F_BOOL:
	    optype = TY_BOOL
	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		if (O_VALB(ap))
		    iresult = 1
		else
		    iresult = 0
	    case TY_CHAR:
		iresult = strlen (O_VALC(ap))
	    case TY_INT:
		iresult = O_VALI(ap)
	    case TY_REAL:
		if (abs(rval[1]) > .001)
		    iresult = 1
		else
		    iresult = 0
	    default:
		goto badtype_
	    }

	case F_INT:
	    optype = TY_INT
	    if (type[1] == TY_INT)
		iresult = ival[1]
	    else
		iresult = rval[1]
		
	case F_REAL:
	    rresult = rval[1]

	case F_STR:
	    # Convert operand to operand of type string.

	    optype = TY_CHAR
	    switch (O_TYPE(ap)) {
	    case TY_BOOL:
		call malloc (iresult, 3, TY_CHAR)
		oplen = 3
		if (O_VALB(ap))
		    call strcpy ("yes", Memc[iresult], 3)
		else
		    call strcpy ("no",  Memc[iresult], 3)
	    case TY_CHAR:
		oplen = strlen (O_VALC(ap))
		call malloc (iresult, oplen, TY_CHAR)
		call strcpy (O_VALC(ap), Memc[iresult], ARB)
	    case TY_INT:
		oplen = MAX_DIGITS
		call malloc (iresult, oplen, TY_CHAR)
		call sprintf (Memc[iresult], SZ_FNAME, "%d")
		    call pargi (O_VALI(ap))
	    case TY_REAL:
		oplen = MAX_DIGITS
		call malloc (iresult, oplen, TY_CHAR)
		call sprintf (Memc[iresult], SZ_FNAME, "%g")
		    call pargr (O_VALR(ap))
	    default:
		goto badtype_
	    }

	default:
	    call xev_error ("bad switch in callfcn")
	}

	# Write the result to the output operand.  Bool results are stored in
	# iresult as an integer value, string results are stored in iresult as
	# a pointer to the output string, and integer and real results are
	# stored in iresult and rresult without any tricks.

	call xev_initop (out, oplen, optype)

	switch (optype) {
	case TY_BOOL:
	    O_VALB(out) = (iresult != 0)
	case TY_CHAR:
	    O_VALP(out) = iresult
	case TY_INT:
	    O_VALI(out) = iresult
	case TY_REAL:
	    O_VALR(out) = rresult
	}

free_
	# Free any storage used by the argument list operands.
	do i = 1, nargs
	    call xev_freeop (args[i])

	call sfree (sp)
	return

badtype_
	call xev_error1 ("bad argument to function `%s'", fcn)
	call sfree (sp)
	return
end


# XEV_STARTARGLIST -- Allocate an argument list descriptor to receive
# arguments as a function call is parsed.  We are called with either
# zero or one arguments.  The argument list descriptor is pointed to by
# a ficticious operand.  The descriptor itself contains a count of the
# number of arguments, an array of pointers to the operand structures,
# as well as storage for the operand structures.  The operands must be
# stored locally since the parser will discard its copy of the operand
# structure for each argument as the associated grammar rule is reduced.

procedure xev_startarglist (arg, out)

pointer	arg			# pointer to first argument, or NULL
pointer	out			# output operand pointing to arg descriptor
pointer	ap

errchk	malloc

begin
	call xev_initop (out, 0, TY_POINTER)
	call malloc (ap, LEN_ARGSTRUCT, TY_STRUCT)
	O_VALP(out) = ap

	if (arg == NULL)
	    A_NARGS(ap) = 0
	else {
	    A_NARGS(ap) = 1
	    A_ARGP(ap,1) = A_OPS(ap)
	    YYMOVE (arg, A_OPS(ap))
	}
end


# XEV_ADDARG -- Add an argument to the argument list for a function call.

procedure xev_addarg (arg, arglist, out)

pointer	arg			# pointer to argument to be added
pointer	arglist			# pointer to operand pointing to arglist
pointer	out			# output operand pointing to arg descriptor

pointer	ap, o
int	nargs

begin
	ap = O_VALP(arglist)

	nargs = A_NARGS(ap) + 1
	A_NARGS(ap) = nargs
	if (nargs > MAX_ARGS)
	    call xev_error ("too many function arguments")

	o = A_OPS(ap) + ((nargs - 1) * LEN_OPERAND)
	A_ARGP(ap,nargs) = o
	YYMOVE (arg, o)

	YYMOVE (arglist, out)
end


# XEV_ERROR1 -- Take an error action, formatting an error message with one
# format string plus one string argument.

procedure xev_error1 (fmt, arg)

char	fmt[ARB]		# printf format string
char	arg[ARB]		# string argument
pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sprintf (Memc[buf], SZ_LINE, fmt)
	    call pargstr (arg)

	call xev_error (Memc[buf])
	call sfree (sp)
end


# XEV_ERROR2 -- Take an error action, formatting an error message with one
# format string plus one string argument and one integer argument.

procedure xev_error2 (fmt, arg1, arg2)

char	fmt[ARB]		# printf format string
char	arg1[ARB]		# string argument
int	arg2			# integer argument
pointer	sp, buf

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call sprintf (Memc[buf], SZ_LINE, fmt)
	    call pargstr (arg1)
	    call pargi (arg2)

	call xev_error (Memc[buf])
	call sfree (sp)
end


# XEV_ERROR -- Take an error action, given an error message string as the
# sole argument.

procedure xev_error (errmsg)

char	errmsg[ARB]			# error message

begin
	call error (1, errmsg)
end


# XEV_INITOP -- Set up an unintialized operand structure.

procedure xev_initop (o, o_len, o_type)

pointer	o		# pointer to operand structure
int	o_len		# length of operand (zero if scalar)
int	o_type		# datatype of operand

begin
	O_LEN(o) = 0
	call xev_makeop (o, o_len, o_type)
end


# XEV_MAKEOP -- Set up the operand structure.  If the operand structure has
# already been initialized and array storage allocated, free the old array.

procedure xev_makeop (o, o_len, o_type)

pointer	o		# pointer to operand structure
int	o_len		# length of operand (zero if scalar)
int	o_type		# datatype of operand

errchk	malloc

begin
	# Free old array storage if any.
	if (O_TYPE(o) != 0 && O_LEN(o) > 1) {
	    call mfree (O_VALP(o), O_TYPE(o))
	    O_LEN(o) = 0
	}

	# Set new operand type.
	O_TYPE(o) = o_type

	# Allocate array storage if nonscalar operand.
	if (o_len > 0) {
	    call malloc (O_VALP(o), o_len, o_type)
	    O_LEN(o) = o_len
	}
end


# XEV_FREEOP -- Reinitialize an operand structure, i.e., free any associated
# array storage and clear the operand datatype field, but do not free the
# operand structure itself (which may be only a segment of an array and not
# a separately allocated structure).

procedure xev_freeop (o)

pointer	o		# pointer to operand structure

begin
	# Free old array storage if any.
	if (O_TYPE(o) != 0 && O_LEN(o) > 1) {
	    call mfree (O_VALP(o), O_TYPE(o))
	    O_LEN(o) = 0
	}

	# Clear the operand type to mark operand invalid.
	O_TYPE(o) = 0
end
define	YYNPROD		33
define	YYLAST		303
# line	1 "/iraf/iraf/lib/yaccpar.x"
# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# Parser for yacc output, translated to the IRAF SPP language.  The contents
# of this file form the bulk of the source of the parser produced by Yacc.
# Yacc recognizes several macros in the yaccpar input source and replaces
# them as follows:
#	A	user suppled "global" definitions and declarations
# 	B	parser tables
# 	C	user supplied actions (reductions)
# The remainder of the yaccpar code is not changed.

define	yystack_	10		# statement labels for gotos
define	yynewstate_	20
define	yydefault_	30
define	yyerrlab_	40
define	yyabort_	50

define	YYFLAG		(-1000)		# defs used in user actions
define	YYERROR		goto yyerrlab_
define	YYACCEPT	return (OK)
define	YYABORT		return (ERR)


# YYPARSE -- Parse the input stream, returning OK if the source is
# syntactically acceptable (i.e., if compilation is successful),
# otherwise ERR.  The parameters YYMAXDEPTH and YYOPLEN must be
# supplied by the caller in the %{ ... %} section of the Yacc source.
# The token value stack is a dynamically allocated array of operand
# structures, with the length and makeup of the operand structure being
# application dependent.

int procedure yyparse (fd, yydebug, yylex)

int	fd			# stream to be parsed
bool	yydebug			# print debugging information?
int	yylex()			# user-supplied lexical input function
extern	yylex()

short	yys[YYMAXDEPTH]		# parser stack -- stacks tokens
pointer	yyv			# pointer to token value stack
pointer	yyval			# value returned by action
pointer	yylval			# value of token
int	yyps			# token stack pointer
pointer	yypv			# value stack pointer
int	yychar			# current input token number
int	yyerrflag		# error recovery flag
int	yynerrs			# number of errors

short	yyj, yym		# internal variables
pointer	yysp, yypvt
short	yystate, yyn
int	yyxi, i
errchk	salloc, yylex


# XEV_PARSE -- SPP/Yacc parser for the evaluation of an expression passed as
# a text string.  Expression evaluation is carried out as the expression is
# parsed, rather than being broken into separate compile and execute stages.
# There is only one statement in this grammar, the expression.  Our function
# is to reduce an expression to a single value of type bool, string, int,
# or real.

pointer	ap
bool	streq()
errchk	zcall2, xev_error1, xev_unop, xev_binop, xev_boolop
errchk	xev_quest, xev_callfcn, xev_addarg
include	"evexpr.com"

short	yyexca[96]
data	(yyexca(i),i=  1,  8)	/  -1,   1,   0,  -1,  -2,   0,  -1,   4/
data	(yyexca(i),i=  9, 16)	/  40,  27,  -2,   3,  -1,   5,  40,  26/
data	(yyexca(i),i= 17, 24)	/  -2,   4,  -1,  61, 269,   0, 270,   0/
data	(yyexca(i),i= 25, 32)	/ 271,   0, 279,   0,  -2,  16,  -1,  62/
data	(yyexca(i),i= 33, 40)	/ 269,   0, 270,   0, 271,   0, 279,   0/
data	(yyexca(i),i= 41, 48)	/  -2,  17,  -1,  63, 269,   0, 270,   0/
data	(yyexca(i),i= 49, 56)	/ 271,   0, 279,   0,  -2,  18,  -1,  64/
data	(yyexca(i),i= 57, 64)	/ 269,   0, 270,   0, 271,   0, 279,   0/
data	(yyexca(i),i= 65, 72)	/  -2,  19,  -1,  65, 272,   0, 273,   0/
data	(yyexca(i),i= 73, 80)	/ 274,   0,  -2,  20,  -1,  66, 272,   0/
data	(yyexca(i),i= 81, 88)	/ 273,   0, 274,   0,  -2,  21,  -1,  67/
data	(yyexca(i),i= 89, 96)	/ 272,   0, 273,   0, 274,   0,  -2,  22/
short	yyact[303]
data	(yyact(i),i=  1,  8)	/  12,  13,  14,  15,  16,  17,  27,  71/
data	(yyact(i),i=  9, 16)	/  20,  21,  22,  24,  26,  25,  18,  19/
data	(yyact(i),i= 17, 24)	/  51,  16,  23,  11,  12,  13,  14,  15/
data	(yyact(i),i= 25, 32)	/  16,  17,  27,  28,  20,  21,  22,  24/
data	(yyact(i),i= 33, 40)	/  26,  25,  18,  19,  31,  49,  23,  12/
data	(yyact(i),i= 41, 48)	/  13,  14,  15,  16,  17,  27,  10,  20/
data	(yyact(i),i= 49, 56)	/  21,  22,  24,  26,  25,  18,  19,  10/
data	(yyact(i),i= 57, 64)	/   9,  23,  12,  13,  14,  15,  16,  17/
data	(yyact(i),i= 65, 72)	/  10,   1,  20,  21,  22,  24,  26,  25/
data	(yyact(i),i= 73, 80)	/  18,  14,  15,  16,  23,  12,  13,  14/
data	(yyact(i),i= 81, 88)	/  15,  16,  17,   0,   0,  20,  21,  22/
data	(yyact(i),i= 89, 96)	/  24,  26,  25,  69,   0,   0,  70,  23/
data	(yyact(i),i= 97,104)	/  12,  13,  14,  15,  16,  17,   0,   0/
data	(yyact(i),i=105,112)	/  20,  21,  22,  12,  13,  14,  15,  16/
data	(yyact(i),i=113,120)	/  17,   2,  23,  12,  13,  14,  15,  16/
data	(yyact(i),i=121,128)	/   0,  29,  30,   0,  32,   0,   0,   0/
data	(yyact(i),i=129,136)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=137,144)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=145,152)	/   0,  50,   0,  52,  54,  55,  56,  57/
data	(yyact(i),i=153,160)	/  58,  59,  60,  61,  62,  63,  64,  65/
data	(yyact(i),i=161,168)	/  66,  67,  68,   0,   0,   0,   0,   0/
data	(yyact(i),i=169,176)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=177,184)	/   0,   0,   0,   0,  33,   0,   0,   0/
data	(yyact(i),i=185,192)	/  72,   0,   0,  74,   0,   0,   0,   0/
data	(yyact(i),i=193,200)	/   0,   0,  34,  35,  36,  37,  38,  39/
data	(yyact(i),i=201,208)	/  40,  41,  42,  43,  44,  45,  46,  47/
data	(yyact(i),i=209,216)	/  48,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=217,224)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=225,232)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=233,240)	/   0,   0,   0,   0,  12,  13,  14,  15/
data	(yyact(i),i=241,248)	/  16,  17,  27,   0,  20,  21,  22,  24/
data	(yyact(i),i=249,256)	/  26,  25,  18,  19,  73,   0,  23,   0/
data	(yyact(i),i=257,264)	/   0,   0,   0,   0,   0,   0,   0,   4/
data	(yyact(i),i=265,272)	/   5,  53,   0,   0,   7,   0,   0,   3/
data	(yyact(i),i=273,280)	/   4,   5,   0,   0,   0,   7,   0,   0/
data	(yyact(i),i=281,288)	/   0,   4,   5,   8,   6,   0,   7,   0/
data	(yyact(i),i=289,296)	/   0,   0,   0,   0,   8,   6,   0,   0/
data	(yyact(i),i=297,303)	/   0,   0,   0,   0,   0,   8,   6/
short	yypact[75]
data	(yypact(i),i=  1,  8)	/  15,-1000,-241,-1000,-1000,-1000,-230,  24/
data	(yypact(i),i=  9, 16)	/  24,  -4,  24,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i= 17, 24)	/-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i= 25, 32)	/-1000,-1000,-1000,-1000,-1000,-1000,-1000,  24/
data	(yypact(i),i= 33, 40)	/ -25,   6,   6,   6,   6,   6,   6,   6/
data	(yypact(i),i= 41, 48)	/   6,   6,   6,   6,   6,   6,   6,   6/
data	(yypact(i),i= 49, 56)	/   6,  50,-222,-1000,-190,-1000,-190,-248/
data	(yypact(i),i= 57, 64)	/-248,-1000,-146,-184,-203,-154,-154,-154/
data	(yypact(i),i= 65, 72)	/-154,-165,-165,-165,-261,-1000,  24,-1000/
data	(yypact(i),i= 73, 75)	/-222,   6,-222/
short	yypgo[6]
data	(yypgo(i),i=  1,  6)	/   0,  65, 113, 180,  56,  37/
short	yyr1[33]
data	(yyr1(i),i=  1,  8)	/   0,   1,   1,   2,   2,   2,   2,   2/
data	(yyr1(i),i=  9, 16)	/   2,   2,   2,   2,   2,   2,   2,   2/
data	(yyr1(i),i= 17, 24)	/   2,   2,   2,   2,   2,   2,   2,   2/
data	(yyr1(i),i= 25, 32)	/   2,   2,   4,   4,   5,   5,   5,   3/
data	(yyr1(i),i= 33, 33)	/   3/
short	yyr2[33]
data	(yyr2(i),i=  1,  8)	/   0,   2,   1,   1,   1,   2,   2,   2/
data	(yyr2(i),i=  9, 16)	/   4,   4,   4,   4,   4,   4,   4,   4/
data	(yyr2(i),i= 17, 24)	/   4,   4,   4,   4,   4,   4,   4,   7/
data	(yyr2(i),i= 25, 32)	/   4,   3,   1,   1,   0,   1,   3,   0/
data	(yyr2(i),i= 33, 33)	/   2/
short	yychk[75]
data	(yychk(i),i=  1,  8)	/-1000,  -1,  -2, 256, 257, 258, 278, 262/
data	(yychk(i),i=  9, 16)	/ 277,  -4,  40, 260, 261, 262, 263, 264/
data	(yychk(i),i= 17, 24)	/ 265, 266, 275, 276, 269, 270, 271, 279/
data	(yychk(i),i= 25, 32)	/ 272, 274, 273, 267, 257,  -2,  -2,  40/
data	(yychk(i),i= 33, 40)	/  -2,  -3,  -3,  -3,  -3,  -3,  -3,  -3/
data	(yychk(i),i= 41, 48)	/  -3,  -3,  -3,  -3,  -3,  -3,  -3,  -3/
data	(yychk(i),i= 49, 56)	/  -3,  -5,  -2,  41,  -2, 259,  -2,  -2/
data	(yychk(i),i= 57, 64)	/  -2,  -2,  -2,  -2,  -2,  -2,  -2,  -2/
data	(yychk(i),i= 65, 72)	/  -2,  -2,  -2,  -2,  -2,  41,  44, 268/
data	(yychk(i),i= 73, 75)	/  -2,  -3,  -2/
short	yydef[75]
data	(yydef(i),i=  1,  8)	/   0,  -2,   0,   2,  -2,  -2,   0,   0/
data	(yydef(i),i=  9, 16)	/   0,   0,   0,   1,  31,  31,  31,  31/
data	(yydef(i),i= 17, 24)	/  31,  31,  31,  31,  31,  31,  31,  31/
data	(yydef(i),i= 25, 32)	/  31,  31,  31,  31,   5,   6,   7,  28/
data	(yydef(i),i= 33, 40)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yydef(i),i= 41, 48)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yydef(i),i= 49, 56)	/   0,   0,  29,  25,   8,  32,   9,  10/
data	(yydef(i),i= 57, 64)	/  11,  12,  13,  14,  15,  -2,  -2,  -2/
data	(yydef(i),i= 65, 72)	/  -2,  -2,  -2,  -2,   0,  24,   0,  31/
data	(yydef(i),i= 73, 75)	/  30,   0,  23/

begin
	call smark (yysp)
	call salloc (yyv, (YYMAXDEPTH+2) * YYOPLEN, TY_STRUCT)

	# Initialization.  The first element of the dynamically allocated
	# token value stack (yyv) is used for yyval, the second for yylval,
	# and the actual stack starts with the third element.

	yystate = 0
	yychar = -1
	yynerrs = 0
	yyerrflag = 0
	yyps = 0
	yyval = yyv
	yylval = yyv + YYOPLEN
	yypv = yylval

yystack_
	# SHIFT -- Put a state and value onto the stack.  The token and
	# value stacks are logically the same stack, implemented as two
	# separate arrays.

	if (yydebug) {
	    call printf ("state %d, char 0%o\n")
		call pargs (yystate)
		call pargi (yychar)
	}
	yyps = yyps + 1
	yypv = yypv + YYOPLEN
	if (yyps > YYMAXDEPTH) {
	    call sfree (yysp)
	    call eprintf ("yacc stack overflow\n")
	    return (ERR)
	}
	yys[yyps] = yystate
	YYMOVE (yyval, yypv)

yynewstate_
	# Process the new state.
	yyn = yypact[yystate+1]

	if (yyn <= YYFLAG)
	    goto yydefault_			# simple state

	# The variable "yychar" is the lookahead token.
	if (yychar < 0) {
	    yychar = yylex (fd, yylval)
	    if (yychar < 0)
		yychar = 0
	}
	yyn = yyn + yychar
	if (yyn < 0 || yyn >= YYLAST)
	    goto yydefault_

	yyn = yyact[yyn+1]
	if (yychk[yyn+1] == yychar) {		# valid shift
	    yychar = -1
	    YYMOVE (yylval, yyval)
	    yystate = yyn
	    if (yyerrflag > 0)
		yyerrflag = yyerrflag - 1
	    goto yystack_
	}

yydefault_
	# Default state action.

	yyn = yydef[yystate+1]
	if (yyn == -2) {
	    if (yychar < 0) {
		yychar = yylex (fd, yylval)
		if (yychar < 0)
		    yychar = 0
	    }

	    # Look through exception table.
	    yyxi = 1
	    while ((yyexca[yyxi] != (-1)) || (yyexca[yyxi+1] != yystate))
		yyxi = yyxi + 2
	    for (yyxi=yyxi+2;  yyexca[yyxi] >= 0;  yyxi=yyxi+2) {
		if (yyexca[yyxi] == yychar)
		    break
	    }

	    yyn = yyexca[yyxi+1]
	    if (yyn < 0) {
		call sfree (yysp)
		return (OK)			# ACCEPT -- all done
	    }
	}


	# SYNTAX ERROR -- resume parsing if possible.

	if (yyn == 0) {
	    switch (yyerrflag) {
	    case 0, 1, 2:
		if (yyerrflag == 0) {		# brand new error
		    call eprintf ("syntax error\n")
yyerrlab_
		    yynerrs = yynerrs + 1
		    # fall through...
		}

	    # case 1:
	    # case 2: incompletely recovered error ... try again
		yyerrflag = 3

		# Find a state where "error" is a legal shift action.
		while (yyps >= 1) {
		    yyn = yypact[yys[yyps]+1] + YYERRCODE
		    if ((yyn >= 0) && (yyn < YYLAST) &&
			(yychk[yyact[yyn+1]+1] == YYERRCODE)) {
			    # Simulate a shift of "error".
			    yystate = yyact[yyn+1]
			    goto yystack_
		    }
		    yyn = yypact[yys[yyps]+1]

		    # The current yyps has no shift on "error", pop stack.
		    if (yydebug) {
			call printf ("error recovery pops state %d, ")
			    call pargs (yys[yyps])
			call printf ("uncovers %d\n")
			    call pargs (yys[yyps-1])
		    }
		    yyps = yyps - 1
		    yypv = yypv - YYOPLEN
		}

		# ABORT -- There is no state on the stack with an error shift.
yyabort_
		call sfree (yysp)
		return (ERR)


	    case 3: # No shift yet; clobber input char.

		if (yydebug) {
		    call printf ("error recovery discards char %d\n")
			call pargi (yychar)
		}

		if (yychar == 0)
		    goto yyabort_		# don't discard EOF, quit
		yychar = -1
		goto yynewstate_		# try again in the same state
	    }
	}


	# REDUCE -- Reduction by production yyn.

	if (yydebug) {
	    call printf ("reduce %d\n")
		call pargs (yyn)
	}
	yyps  = yyps - yyr2[yyn+1]
	yypvt = yypv
	yypv  = yypv - yyr2[yyn+1] * YYOPLEN
	YYMOVE (yypv + YYOPLEN, yyval)
	yym   = yyn

	# Consult goto table to find next state.
	yyn = yyr1[yyn+1]
	yyj = yypgo[yyn+1] + yys[yyps] + 1
	if (yyj >= YYLAST)
	    yystate = yyact[yypgo[yyn+1]+1]
	else {
	    yystate = yyact[yyj+1]
	    if (yychk[yystate+1] != -yyn)
		yystate = yyact[yypgo[yyn+1]+1]
	}

	# Perform action associated with the grammar rule, if any.
	switch (yym) {
	    
case 1:
# line 135 "evexpr.y"
{
			# Normal exit.  Move the final expression value operand
			# into the operand structure pointed to by the global
			# variable ev_oval.

			YYMOVE (yypvt-YYOPLEN, ev_oval)
			return (OK)
		}
case 2:
# line 143 "evexpr.y"
{
			call error (1, "syntax error")
		}
case 3:
# line 149 "evexpr.y"
{
			# Numeric constant.
			YYMOVE (yypvt, yyval)
		    }
case 4:
# line 153 "evexpr.y"
{
			# The boolean constants "yes" and "no" are implemented
			# as reserved operands.

			call xev_initop (yyval, 0, TY_BOOL)
			if (streq (O_VALC(yypvt), "yes"))
			    O_VALB(yyval) = true
			else if (streq (O_VALC(yypvt), "no"))
			    O_VALB(yyval) = false
			else if (ev_getop != NULL)
			    call zcall2 (ev_getop, O_VALC(yypvt), yyval)
			else
			    call xev_error1 ("illegal operand `%s'", O_VALC(yypvt))
			call xev_freeop (yypvt)
		    }
case 5:
# line 168 "evexpr.y"
{
			# e.g., @"param"
			if (ev_getop != NULL)
			    call zcall2 (ev_getop, O_VALC(yypvt), yyval)
			else
			    call xev_error1 ("illegal operand `%s'", O_VALC(yypvt))
			call xev_freeop (yypvt)
		    }
case 6:
# line 176 "evexpr.y"
{
			# Unary arithmetic minus.
			call xev_unop (MINUS, yypvt, yyval)
		    }
case 7:
# line 180 "evexpr.y"
{
			# Boolean not.
			call xev_unop (NOT, yypvt, yyval)
		    }
case 8:
# line 184 "evexpr.y"
{
			# Addition.
			call xev_binop (PLUS, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 9:
# line 188 "evexpr.y"
{
			# Subtraction.
			call xev_binop (MINUS, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 10:
# line 192 "evexpr.y"
{
			# Multiplication.
			call xev_binop (STAR, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 11:
# line 196 "evexpr.y"
{
			# Division.
			call xev_binop (SLASH, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 12:
# line 200 "evexpr.y"
{
			# Exponentiation.
			call xev_binop (EXPON, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 13:
# line 204 "evexpr.y"
{
			# String concatenation.
			call xev_binop (CONCAT, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 14:
# line 208 "evexpr.y"
{
			# Boolean and.
			call xev_boolop (AND, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 15:
# line 212 "evexpr.y"
{
			# Boolean or.
			call xev_boolop (OR, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 16:
# line 216 "evexpr.y"
{
			# Boolean less than.
			call xev_boolop (LT, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 17:
# line 220 "evexpr.y"
{
			# Boolean greater than.
			call xev_boolop (GT, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 18:
# line 224 "evexpr.y"
{
			# Boolean less than or equal.
			call xev_boolop (LE, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 19:
# line 228 "evexpr.y"
{
			# Boolean greater than or equal.
			call xev_boolop (GE, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 20:
# line 232 "evexpr.y"
{
			# Boolean equal.
			call xev_boolop (EQ, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 21:
# line 236 "evexpr.y"
{
			# String pattern-equal.
			call xev_boolop (SE, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 22:
# line 240 "evexpr.y"
{
			# Boolean not equal.
			call xev_boolop (NE, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 23:
# line 244 "evexpr.y"
{
			# Conditional expression.
			call xev_quest (yypvt-6*YYOPLEN, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 24:
# line 248 "evexpr.y"
{
			# Call an intrinsic or external function.
			ap = O_VALP(yypvt-YYOPLEN)
			call xev_callfcn (O_VALC(yypvt-3*YYOPLEN),
			    A_ARGP(ap,1), A_NARGS(ap), yyval)
			call mfree (ap, TY_STRUCT)
			call xev_freeop (yypvt-3*YYOPLEN)
		    }
case 25:
# line 256 "evexpr.y"
{
			YYMOVE (yypvt-YYOPLEN, yyval)
		    }
case 26:
# line 262 "evexpr.y"
{
			YYMOVE (yypvt, yyval)
		    }
case 27:
# line 265 "evexpr.y"
{
			if (O_TYPE(yypvt) != TY_CHAR)
			    call error (1, "illegal function name")
			YYMOVE (yypvt, yyval)
		    }
case 28:
# line 273 "evexpr.y"
{
			# Empty.
			call xev_startarglist (NULL, yyval)
		    }
case 29:
# line 277 "evexpr.y"
{
			# First arg; start a nonnull list.
			call xev_startarglist (yypvt, yyval)
		    }
case 30:
# line 281 "evexpr.y"
{
			# Add an argument to an existing list.
			call xev_addarg (yypvt, yypvt-2*YYOPLEN, yyval)
		    }	}

	goto yystack_				# stack new state and value
end
