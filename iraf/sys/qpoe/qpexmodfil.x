# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "qpoe.h"
include "qpex.h"

# QPEX_MODFILTER -- Compile an event attribute expression to be used for event
# attribute filtering, modifying the the current EAF as directed by the expr.
# An event attribute expression consists of a sequence of independent terms
# of the form "attribute = expr", e.g.,
#
#       pha=%104B, e=100:, t=(:9,11:29,33,42:65,67:99,!(82,87),103), ...
#
# 
# Variants on "attr = expr" are "attr := expr" and "attr += expr".  In the
# case of :=, any expression terms already entered for the named attribute
# will be REPLACED by the new expression.  In the case of +=, the given
# expression denotes an additional condition which the attribute must satisfy
# to pass the filter, i.e., a new term is added to the existing filter.  The
# case = is the same as +=, i.e., the default action is to modify rather than
# replace any existing filter.
# 
#
# Our function is to extract each attribute=expr term and compile it into a
# series of instructions to be repeatedly executed (interpreted) at runtime
# to evaluate the expression for a particular event structure.  Terms are
# compiled and evaluated in the order in which they appear in the expression
# list (except for replacement terms), allowing the user to manually optimize
# the filter by giving terms which are most likely to fail first.
#
# The expression list may contain references to predefined global or local
# (datafile) macros, external macro files, or back-quoted CL commands for
# which the output is to be substituted as for a macro.  In all cases, macro
# substitution is handled at a lower level in the gettok routine.  In
# particular, the logical names of the fields of the event structure are
# implemented as predefined datafile-local macros, hence we are concerned only
# with physical field names here.  The form of a physical field name is
# a datatype code [SIRD] followed by the decimal zero-indexed byte offset
# of the field in the event structure, e.g., S0, S2, R4, etc. (short integer
# field at offset 0, same at offset 2, Real*4 field at offset 4, etc.).
#
# The function value is OK if the expression list compiles without any errors,
# or ERR if some compilation error occurs.  Compilation errors cause an error
# message to be output to STDERR and the affected terms to be skipped, but are
# otherwise ignored.

int procedure qpex_modfilter (ex, exprlist)

pointer ex                      #I qpex descriptor
char    exprlist[ARB]           #I list of attribute=expr expressions

bool	replace
int	boffset, offset, max_offset, dtype
int	status, sz_expr, token, parenlevel, nchars, buflen
pointer	sp, atname, assignop, tokbuf, expr, qp, ip, op, in, et_tail

pointer	qp_opentext()
int	qpex_codegeni(), qpex_codegenr(), qpex_codegend()
int	qp_gettok(), strlen(), gstrcpy(), ctoi(), sizeof()
errchk	malloc, qp_opentext, qp_gettok, realloc, qpex_delete

string	qpexwarn "QPEX Warning"
define	eatup_ 91
define	badatt_ 92

begin
	call smark (sp)
	call salloc (atname, SZ_TOKBUF, TY_CHAR)
	call salloc (assignop, SZ_TOKBUF, TY_CHAR)
	call salloc (tokbuf, SZ_TOKBUF, TY_CHAR)

	status = OK
	sz_expr = DEF_SZEXPRBUF
	et_tail = EX_ETTAIL(ex)
	qp = EX_QP(ex)

	# Allocate a variable size expression buffer.
	call malloc (expr, sz_expr, TY_CHAR)

	# Open the expression list for token input with macro expansion.
	in = qp_opentext (qp, exprlist)

	# Accumulate and compile successive attribute=expr terms of the
	# expression list.

	repeat {
	    # Get attribute name.
	    switch (qp_gettok (in, Memc[atname], SZ_TOKBUF)) {
	    case EOF:
		break			# input exhausted
	    case ',', ';':
		next			# null statement
	    case TOK_IDENTIFIER:
		;			# got one
	    default:
		call eprintf ("%s: unexpected token `%s'\n")
		    call pargstr (qpexwarn)
		    call pargstr (Memc[atname])
		goto eatup_
	    }

	    # Get operator.
	    switch (qp_gettok (in, Memc[assignop], SZ_TOKBUF)) {
	    case TOK_PLUSEQUALS, '=':
		replace = false
	    case TOK_COLONEQUALS:
		replace = true

	    default:
		call eprintf ("%s: missing assignment token (`%s')\n")
		    call pargstr (qpexwarn)
		    call pargstr (Memc[atname])
eatup_
		# A half-hearted attempt to ignore the offending statement...
		while (qp_gettok (in, Memc[expr], sz_expr) != EOF)
		    if (Memc[expr] == ',')
			break

		# The default is to add to any existing filter.
		replace = false
	    }

	    parenlevel = 0
	    token = NULL

	    # Accumulate expression.
	    for (op=expr;  token != EOF;  ) {
		# Get next token from input stream.
		token = qp_gettok (in, Memc[tokbuf], SZ_TOKBUF)

		# Process any special tokens.
		switch (token) {
		case EOF:
		    break
		case '(':
		    parenlevel = parenlevel + 1
		case ')':
		    parenlevel = parenlevel - 1
		    if (parenlevel < 0) {
			call eprintf ("%s: missing left parenthesis\n")
			    call pargstr (qpexwarn)
			parenlevel = 0
			status = ERR
			next
		    }
		case ',', ';':
		    # An unparenthesized comma terminates the expression.
		    if (parenlevel <= 0)
			break
		}

		# Allocate more storage if expr buf fills.
		nchars = strlen (Memc[tokbuf])
		buflen = op - expr
		if (buflen + nchars > sz_expr) {
		    sz_expr = sz_expr + INC_SZEXPRBUF
		    call realloc (expr, sz_expr, TY_CHAR)
		    op = expr + buflen
		}

		# Concatenate token string to expr.
		op = op + gstrcpy (Memc[tokbuf], Memc[op], SZ_TOKBUF)
	    }

	    Memc[op] = EOS
	    if (parenlevel > 0) {
		call eprintf ("%s: missing right parenthesis in expression\n")
		    call pargstr (qpexwarn)
		status = ERR
	    }

	    # Parse the attribute name to determine the datatype and offset.

	    # Get byte offset of field.
	    ip = atname + 1
	    if (ctoi (Memc, ip, boffset) <= 0)
		goto badatt_

	    # Get datatype and scaled offset; check field alignment.
	    switch (Memc[atname]) {
	    case 'S', 's':
		dtype = TY_SHORT
		offset = boffset / (SZ_SHORT * SZB_CHAR)
		if (offset * SZ_SHORT * SZB_CHAR != boffset)
		    goto badatt_
	    case 'I', 'i':
		dtype = TY_INT
		offset = boffset / (SZ_INT * SZB_CHAR)
		if (offset * SZ_INT * SZB_CHAR != boffset)
		    goto badatt_
	    case 'R', 'r':
		dtype = TY_REAL
		offset = boffset / (SZ_REAL * SZB_CHAR)
		if (offset * SZ_REAL * SZB_CHAR != boffset)
		    goto badatt_
	    case 'D', 'd':
		dtype = TY_DOUBLE
		offset = boffset / (SZ_DOUBLE * SZB_CHAR)
		if (offset * SZ_DOUBLE * SZB_CHAR != boffset)
		    goto badatt_
	    default:
		goto badatt_
	    }
	    
	    # Verify that the field is in range in the event struct.
	    # (Actually, we don't know the event struct at compile time...)

	    max_offset = (boffset / SZB_CHAR) + sizeof(dtype) - 1
	    if (boffset < 0 || max_offset > ARB) {
badatt_		call eprintf ("%s: bad attribute name `%s'\n")
		    call pargstr (qpexwarn)
		    call pargstr (Memc[atname])
		status = ERR
		next
	    }

	    # Clobber any old expression for the given attribute if replace
	    # mode is in effect.  Only previous expression terms are affected,
	    # hence in single expressions like "pha=x,pha=y", the second entry
	    # does not clobber the first.

	    if (replace)
		call qpex_delete (ex, et_tail, offset, dtype)

	    # Compile the expression.
	    switch (dtype) {
	    case TY_SHORT, TY_INT:
		if (qpex_codegeni (ex, Memc[atname], Memc[assignop],
		    Memc[expr], offset, dtype) == ERR)
		    status = ERR
	    case TY_REAL:
		if (qpex_codegenr (ex, Memc[atname], Memc[assignop],
		    Memc[expr], offset, dtype) == ERR)
		    status = ERR
	    case TY_DOUBLE:
		if (qpex_codegend (ex, Memc[atname], Memc[assignop],
		    Memc[expr], offset, dtype) == ERR)
		    status = ERR
	    }
	}

	call qp_closetext (in)
	call mfree (expr, TY_CHAR)
	call sfree (sp)

	return (status)
end
