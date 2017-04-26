include <tbset.h>
include	"lexoper.h"

define	MAXSTACK	100
define	SZ_VALSTACK	3*SZ_LINE

# PARSER -- Parse a rule base file
#
# This procedure uses a simple operator precedence parser. Every token
# retrieved from the file is either an identifier or an operator.
# Identifiers are pushed onto an identifier stack. Operators are
# pushed onto a separate operator stack. When an operator is read
# whose priority is less than that of the operator on top of the
# operator stack, the operator on the stack is popped and passed to
# a procedure which performs the appropriate action, using the
# identifiers on the identifier stack. This continues until all
# operators of higher priority have been processed, or the stack is
# empty. Syntax checking is done by checking that the identifier
# stack contains the correct number and type of identifier and that
# identifiers and operators alternate in the input. The priority of 
# each operator is implicit in the integer which is used to represent 
# it. For more information on operator precedence parsers, see "Writing 
# Interactive Compilers and Interpreters" by P.J Brown, pp. 149-151.
#
# B.Simon	25-Apr-88	Original
# B.Simon	15-Jan-99	Skip rules with columns not in table

procedure parser (rbase, itp, dbg, target, action)

char	rbase[ARB]	# i: Rule base name
pointer	itp		# i: Input table descriptor
int	dbg		# i: Debug file descriptor
pointer	target		# o: Target table descriptor
pointer	action		# o: Action table descriptor
#--
include "parser.com"

bool	done, expect_id
int	idtop, optop, oper, tabtop, missing
int	opstack[MAXSTACK]
pointer	sp, rb, work, value, valstack, nxtval, colname, colval
pointer	idstack[MAXSTACK]

string	find_error  "Column name or type mismatch"
string	stack_error "Stack overflow"
string	oper_error  "Operator expected"
string	ident_error "Identifier expected"

string	wrkname     "The parser changed the work table to the following:"

int	gstrcpy(), putstackt(), numstack()
pointer	initstack()

errchk	initstack, lexinit, lexer, lexclose, syntax, typecheck, mkrules

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (value, SZ_LINE, TY_CHAR)
	call salloc (valstack, SZ_VALSTACK, TY_CHAR)

	# Initialize the lexical analyzer

	call lexinit (rbase, rb)

	# Create tables used by parser

	target = initstack (itp, "_FIRST,_LAST,_USED")
	action = initstack (itp, "")
	work =  initstack (itp, "")

	# Save copy of table pointers in common block

	ptgt = target
	pact = action
	pwrk = work

	# Initialize stack pointers

	idtop = 0
	optop = 0
	nxtval = valstack

	missing = NO
	done = false
	expect_id = true

	repeat {

	    # Get next operator from rule base

	    call lexer (rb, oper, Memc[value], SZ_LINE)

	    # First case: operator is identifier, push on id stack

	    if (oper == IDOPR) {
		if (expect_id) {
		    idtop = idtop + 1
		    if (idtop > MAXSTACK)
			call syntax (rb, stack_error)

		    idstack[idtop] = nxtval
		    nxtval = gstrcpy (Memc[value], Memc[nxtval], SZ_LINE) +
			     nxtval + 1
		} else {
		    call syntax (rb, oper_error)
		}

	    # Second case: operator is not identifier

	    } else {
		if (oper != ENDOPR && expect_id)
		    call syntax (rb, ident_error)

		# Process all operators whose priorities are >=
		# the operator just read from the rule base

		repeat {
		    if (optop == 0)
			break
		    if (oper > opstack[optop])
			break

		    # Perform semantic actions associated with operators

		    switch (opstack[optop]) {
		    case ENDOPR:
			call typecheck (rb, idstack, idtop, NO_IDENT, NO_IDENT)
			done = true
		    case SEPOPR:
			call typecheck (rb, idstack, idtop, NO_IDENT, NO_IDENT)
		    case IMPOPR:
			call typecheck (rb, idstack, idtop, PHRASE, CLAUSE)
			if (missing == NO) {
			    call mkrules (work, target, action)
			} else {
			    missing = NO
			    tabtop = numstack (work)
			    call tbrdel (work, 1, tabtop)
			}
			idtop = idtop - 2
		    case OROPR:
			call typecheck (rb, idstack, idtop, CLAUSE, CLAUSE)
			idtop = idtop - 1
			idstack[idtop] = CLAUSE
		    case ANDOPR:
			call typecheck (rb, idstack, idtop, PHRASE, PHRASE)
			if (missing == NO)
			    call andstack (work)

			idtop = idtop - 1
			idstack[idtop] = PHRASE
		    case EQOPR:
			call typecheck (rb, idstack, idtop, NAME, NAME)
			colval = idstack[idtop]
			colname = idstack[idtop-1]
			nxtval = colname
			call pushstack (work)
			if (putstackt (work, Memc[colname], Memc[colval])== NO)
			    missing = YES

			idtop = idtop - 1
			idstack[idtop] = PHRASE			
		    }

		    optop = optop - 1

		    # Debug prints

		    tabtop = numstack (work)
		    call dbg_rules (work, wrkname, 1, tabtop, dbg)

		} until (done)

		# Push the operator just read on the operator stack

		optop = optop + 1
		if (optop > MAXSTACK)
		    call syntax (rb, stack_error)
		opstack[optop] = oper
	    }

	    # Operators and identifiers should alternate in the input

	    expect_id = ! expect_id

	} until (done)

	call freestack (work)
	call lexclose (rb)
	call sfree (sp)
end

# TYPECHECK -- Check the number and type of identifiers on the stack

procedure typecheck (rb, idstack, idtop, type1, type2)

pointer	rb		# i: Rule base descriptor
pointer	idstack[ARB]	# i: Identifier stack
int	idtop		# i: Top of identifier stack
pointer	type1		# i: Type expected for one below stack top
pointer	type2		# i: Type expected for stack top
#--
int	itype
pointer	id, type[2]

string	bad_type "Operator out of order"
string	too_few  "Missing identifier"
string	too_many "Unexpected end of rule"

begin
	type[1] = type1
	type[2] = type2

	do itype = 1, 2 {
	    switch (type[itype]) {
	    case CLAUSE:

		if (idtop < itype)
		    call syntax (rb, too_few)
		id = idstack[idtop+itype-2]

		# a phrase is also a clause

		if (!(id == PHRASE || id == CLAUSE))
		    call syntax (rb, bad_type)

	    case PHRASE:

		if (idtop < itype)
		    call syntax (rb, too_few)
		id = idstack[idtop+itype-2]
		if (id != PHRASE)
		    call syntax (rb, bad_type)

	    case NO_IDENT:

		if (idtop >= itype)
		    call syntax (rb, too_many)

	    case NAME:

		if (idtop < itype)
		    call syntax (rb, too_few)
		id = idstack[idtop+itype-2]
		if (id <= 0)
		    call syntax (rb, bad_type)

	    }
	}

end

# SYNTAX -- Print a syntax error message

procedure syntax (rb, errmsg)

pointer	rb		# i: Rule base descriptor
char	errmsg[ARB]	# i: Error message
#--
include "parser.com"

begin
	# Remove temporary tables

	call freestack (ptgt)
	call freestack (pact)
	call freestack (pwrk)

	# Print the line where the error was detected

	call eprintf ("Syntax error on line %d\n%s%*t^\n")
	    call pargi (RB_NLINE(rb))
	    call pargstr (RB_LINE(rb))
	    call pargi (RB_INDEX(rb))

	# Close the rules file and send the error message

	call lexclose (rb)
	call error (ERR, errmsg)

end
