include	"vex.h"

# VEX_EVAL -- Evaluate the pseudocode
#
# This procedure evaluates the pseudocode produced by vex_compile(). Evaluation
# is performed on an entire vector at a time. The calling program must
# supply a subroutine which returns the vector associated with a variable name.
# The procedure is called as follows: call getvar(stack, name), where
# stack is a pointer to the stack structure and name is the variable name.
# This procedure should call stk_alloc(stack, len, type) passing it the stack
# pointer, the length of the new array, and the type of the new array. The 
# pointer to the new array is returned as the function value. The procedure
# should then fill in the values in the array. Code is the pointer returned 
# by vex_compile(). Nil is a value substituted for the result of an illegal 
# operation, such as division by zero. Type is the data type of expression.
# To retrieve the results of the expression, call vex_copy[dir], which
# retrieves the result as a double, integer, or real array and clears the
# stack for the next call of vex_eval.
#
# B.Simon	21-May-90	Original
# B.Simon	24-Apr-91	Revised to handle multiple types
# B.Simon	15-Oct-98	Embed strings in pseudocode

procedure vex_eval (code, getvar, nil, type)

pointer	code		# i: Pointer to pseudocode structure
extern	getvar		# i: Function which returns a vector given a name
double	nil		# i: Nil value used to replace illegal ops
int	type		# o: Data type of expression
#--
double	junk
int	len, toktype
pointer	sp, token, errmsg, stack, op, var

string	fn1tok  FN1STR
string	fn2tok  FN2STR

int	word_match
double	vex_nilf()
errchk	vex_push

string	badcode  "vex_eval : Illegal opcode (%d)"
string	badfunc  "vex_eval : Illegal function name (%s)"

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (token, MAX_TOKEN, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Initialize the undefined operation function

	junk = vex_nilf (nil)

	# Execute each code token until stop token found

	stack = VEX_STACK(code)
	for (op = VEX_CODE(code); Memi[op] != Y_DONE; op = op + 1) {
	    call pargi (Memi[op])

	    # Perform the indicated operation on the stack variables

	    switch (Memi[op]) {
	    case Y_VAR, Y_INT, Y_REAL, Y_DOUBLE:
		toktype = Memi[op]
		call vex_getstr (op, Memc[token], MAX_TOKEN)
		call vex_push (stack, getvar, toktype, Memc[token])

	    case Y_FN1:
		call vex_getstr (op, Memc[token], MAX_TOKEN)

		switch (word_match (Memc[token], fn1tok)) {
		case FN1_ABS:
		    call vex_abs (stack)

		case FN1_ACOS:
		    call vex_acos (stack)

		case FN1_ASIN:
		    call vex_asin (stack)

		case FN1_ATAN:
		    call vex_atan (stack)

		case FN1_COS:
		    call vex_cos (stack)

		case FN1_COSH:
		    call vex_cosh (stack)

		case FN1_CUBE:
		    call vex_cube (stack)

		case FN1_DOUBLE:
		    call vex_double (stack)

		case FN1_EXP:
		    call vex_exp (stack)

		case FN1_INT:
		    call vex_int (stack)

		case FN1_LOG:
		    call vex_log (stack)

		case FN1_LOG10:
		    call vex_log10 (stack)

		case FN1_NINT:
		    call vex_nint (stack)

		case FN1_REAL:
		    call vex_real (stack)

		case FN1_SIN:
		    call vex_sin (stack)

		case FN1_SINH:
		    call vex_sinh (stack)

		case FN1_SQR:
		    call vex_sqr (stack)

		case FN1_SQRT:
		    call vex_sqrt (stack)

		case FN1_TAN:
		    call vex_tan (stack)

		case FN1_TANH:
		    call vex_tanh (stack)

		default:
		    call sprintf (Memc[errmsg], SZ_LINE, badfunc)
		    call pargstr (Memc[token])
		    call error (1, Memc[errmsg])
		}

	    case Y_FN2:
		call vex_getstr (op, Memc[token], MAX_TOKEN)

		switch (word_match (Memc[token], fn2tok)) {
		case FN2_ATAN2:
		    call vex_atan2 (stack)

		case FN2_DIM:
		    call vex_dim (stack)

		case FN2_MAX:
		    call vex_max (stack)

		case FN2_MIN:
		    call vex_min (stack)

		case FN2_MOD:
		    call vex_mod (stack)

		case FN2_SIGN:
		    call vex_sig (stack)

		default:
		    call sprintf (Memc[errmsg], SZ_LINE, badfunc)
		    call pargstr (Memc[token])
		    call error (1, Memc[errmsg])
		}

	    case Y_IF:
		call vex_if (stack)
 
 	    case Y_OR:
		call vex_or (stack)

 	    case Y_AND:
		call vex_and (stack)

	    case Y_NOT:
		call vex_not (stack)

 	    case Y_EQ:
		call vex_eq (stack)

 	    case Y_NE:
		call vex_ne (stack)

 	    case Y_LT:
		call vex_lt (stack)

 	    case Y_GT:
		call vex_gt (stack)

 	    case Y_LE:
		call vex_le (stack)

 	    case Y_GE:
		call vex_ge (stack)

	    case Y_ADD:
		call vex_add (stack)

	    case Y_SUB:
		call vex_sub (stack)

 	    case Y_MUL:
		call vex_mul (stack)

	    case Y_DIV:
		call vex_div (stack)

	    case Y_NEG:
		call vex_neg (stack)

	    case Y_POW:
		call vex_pow (stack)

	    default:
		call sprintf (Memc[errmsg], SZ_LINE, badcode)
		    call pargs (Memi[op])
		call error (1, Memc[errmsg])
	    }
	}

	# Retrieve the result of the expression,
	# but only return the type to the user

	call stk_fetch (stack, 1, var, len, type)
	call sfree (sp)
end
