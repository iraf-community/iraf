# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<mach.h>
include	"qpoe.h"
include	"qpex.h"
include	"qpio.h"

# QPIO_PARSE -- Parse the QPIO expression operand input to qpio_open or
# qpio_setfilter.  This consists of a comma delimited list of keyword=value
# terms.  We factor out those which are QPIO related and deal with these
# directly, concatenating the remaining terms to be passed on to QPEX.
# The output filter buffer is resized as needed to hold the filter expr.
# ERR is returned as the function value if an error occurs while compiling
# the expression.

int procedure qpio_parse (io, expr, filter, sz_filter, mask, sz_mask)

pointer	io			#I QPIO descriptor
char	expr[ARB]		#I expression to be parsed
pointer	filter			#U filter buffer
int	sz_filter		#U allocated buffer size
char	mask[sz_mask]		#O new mask name (not reallocatable)
int	sz_mask			#I max chars out

real	rval
pointer	qp, sp, keyword, vp, in
int	assignop, byte_offset, sz_field
int	level, zlevel, status, start, value, token, op, kw, tokno

pointer	qp_opentext()
int	qp_gettok(), gstrcpy(), strlen(), strdic(), ctoi(), ctor()
errchk	qp_opentext, malloc, realloc, qp_gettok, qp_ungettok, syserrs

define	F Memc[filter+($1)-1]
define	noval_  91
define	badval_ 92
define	badkey_ 93

begin
	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)

	qp = IO_QP(io)

	# Open the input expression for macro expanded token input.
	in = qp_opentext (qp, expr)

	# Extract and process a series of "param[=expr]" terms, where
	# the expr may be any series of tokens, delimited by an
	# unparenthesized comma.

	op = 1
	tokno = 0
	F(op) = EOS
	mask[1] = EOS
	status = OK
	level = 0

	repeat {
	    start = op

	    # Advance to the next keyword.
	    token = qp_gettok (in, F(op), SZ_TOKBUF)
	    tokno = tokno + 1

	    switch (token) {
	    case EOF:
		break
	    case '(', '[', '{':
		level = level + 1
		next
	    case ')', ']', '}':
		level = level - 1
		next
	    case '!':
		if (tokno <= 2) {
		    IO_NODEFFILT(io) = YES
		    IO_NODEFMASK(io) = YES
		    tokno = 1
		}
		next
	    case TOK_IDENTIFIER:
		op = op + strlen (F(op))
		if (op + SZ_TOKBUF > sz_filter) {
		    sz_filter = sz_filter + INC_SZEXPRBUF
		    call realloc (filter, sz_filter, TY_CHAR)
		}
		call strcpy (F(start), Memc[keyword], SZ_FNAME)
		call strlwr (Memc[keyword])
	    default:
		if (token != ',') {
		    call eprintf ("QPIO: unexpected token `%s'\n")
			call pargstr (F(op))
		    status = ERR
		}
		next
	    }
	   
	    value = NULL
	    token = qp_gettok (in, F(op), SZ_TOKBUF)

	    if (token == '=' ||
		token == TOK_PLUSEQUALS || token == TOK_COLONEQUALS) {

		# Accumulate the expression.
		zlevel = level
		assignop = token
		op = op + strlen (F(op))
		value = op

		repeat {
		    # Peek at the next token to see if it terminates the
		    # expression.  An unparenthesized comma or unmatched
		    # right brace, bracket, or parenthesis is part of the
		    # next statement and terminates the expression.

		    token = qp_gettok (in, F(op), SZ_TOKBUF)
		    switch (token) {
		    case EOF:
			break
		    case '(', '[', '{':
			level = level + 1
		    case ')', ']', '}':
			if (level <= zlevel) {
			    call qp_ungettok (in, F(op))
			    F(op) = EOS
			    break
			} else
			    level = level - 1
		    case ',':
			if (level <= zlevel) {
			    call qp_ungettok (in, F(op))
			    F(op) = EOS
			    break
			}
		    }

		    # Accept token as data.
		    op = op + strlen (F(op))
		    if (op + SZ_TOKBUF + 1 > sz_filter) {
			sz_filter = sz_filter + INC_SZEXPRBUF
			call realloc (filter, sz_filter, TY_CHAR)
		    }

		    F(op) = ' ';  op = op + 1
		    F(op) = EOS
		}
	    }

	    # Process the keywords known to QPIO and pass anything else on
	    # to the output filter buffer.

	    kw = strdic (Memc[keyword], Memc[keyword], SZ_FNAME, KEYWORDS)
	    vp = filter + value - 1

	    switch (kw) {
	    case KW_BLOCK:
		# Set the XY blocking factor for pixelation.
		if (value == NULL)
		    goto noval_
		else if (ctor (Memc, vp, rval) <= 0)
		    goto badval_
		IO_XBLOCK(io) = rval
		IO_YBLOCK(io) = rval
		op = start

	    case KW_XBLOCK:
		# Set the X blocking factor for pixelation.
		if (value == NULL)
		    goto noval_
		else if (ctor (Memc, vp, rval) <= 0)
		    goto badval_
		IO_XBLOCK(io) = rval
		op = start

	    case KW_YBLOCK:
		# Set the Y blocking factor for pixelation.
		if (value == NULL)
		    goto noval_
		else if (ctor (Memc, vp, rval) <= 0)
		    goto badval_
		IO_YBLOCK(io) = rval
		op = start

	    case KW_DEBUG:
		# Set the debug level, default 1 if no argument.
		if (value == NULL)
		    IO_DEBUG(io) = 1
		else if (ctoi (Memc, vp, IO_DEBUG(io)) <= 0) {
		    IO_DEBUG(io) = QP_DEBUG(qp)
badval_		    call eprintf ("QPIO: cannot convert `%s' to integer\n")
			call pargstr (Memc[vp])
		}
		op = start

	    case KW_FILTER:
		# A term such as "filter=(...)".  Keep the (...).
		if (value == NULL)
		    goto noval_
		else {
		    # Accumulate expression term.
		    op = start + gstrcpy (Memc[vp], F(start), ARB)
		    F(op) = ',';  op = op + 1
		    F(op) = EOS
		}

	    case KW_KEY:
		# Set the offsets of the event attribute fields to be used
		# for the event coordinates during extraction.  The typical
		# syntax of the key value is, e.g.,  key=(s10,s8).  Fields
		# used for event coordinate keys must be a numeric type.

		call strlwr (Memc[vp])
		while (Memc[vp] == ' ' || Memc[vp] == '(')
		    vp = vp + 1

		# Get the X field offset and type.
		switch (Memc[vp]) {
		case 's':
		    IO_EVXTYPE(io) = TY_SHORT
		    sz_field = SZ_SHORT
		case 'i':
		    IO_EVXTYPE(io) = TY_INT
		    sz_field = SZ_INT
		case 'l':
		    IO_EVXTYPE(io) = TY_LONG
		    sz_field = SZ_LONG
		case 'r':
		    IO_EVXTYPE(io) = TY_REAL
		    sz_field = SZ_REAL
		case 'd':
		    IO_EVXTYPE(io) = TY_DOUBLE
		    sz_field = SZ_DOUBLE
		default:
		    goto badkey_
		}

		vp = vp + 1
		if (ctoi (Memc, vp, byte_offset) <= 0)
		    goto badkey_
		else
		    IO_EVXOFF(io) = byte_offset / (sz_field * SZB_CHAR)

		while (Memc[vp] == ' ' || Memc[vp] == ',')
		    vp = vp + 1

		# Get the Y field offset.
		switch (Memc[vp]) {
		case 's':
		    IO_EVYTYPE(io) = TY_SHORT
		    sz_field = SZ_SHORT
		case 'i':
		    IO_EVYTYPE(io) = TY_INT
		    sz_field = SZ_INT
		case 'l':
		    IO_EVYTYPE(io) = TY_LONG
		    sz_field = SZ_LONG
		case 'r':
		    IO_EVYTYPE(io) = TY_REAL
		    sz_field = SZ_REAL
		case 'd':
		    IO_EVYTYPE(io) = TY_DOUBLE
		    sz_field = SZ_DOUBLE
		default:
		    goto badkey_
		}

		vp = vp + 1
		if (ctoi (Memc, vp, byte_offset) <= 0) {
badkey_			call eprintf ("QPIO: bad key value `%s'\n")
			call pargstr (F(value))
		    status = ERR
		} else
		    IO_EVYOFF(io) = byte_offset / (sz_field * SZB_CHAR)

		op = start

	    case KW_NOINDEX:
		# Disable use of the index for extraction (for testing).
		IO_NOINDEX(io) = YES
		op = start

	    case KW_PARAM, KW_MASK:
		# Set a string valued option.

		if (value == NULL) {
noval_		    call eprintf ("QPIO: kewyord `%s' requires an argument\n")
			call pargstr (Memc[keyword])
		    status = ERR

		} else {
		    # Kill space added at end of token.
		    op = op - 1
		    F(op) = EOS

		    # Output the string.
		    if (kw == KW_PARAM) {
			# Set the name of the event list parameter.
			call strcpy (Memc[vp], Memc[IO_PARAM(io)], SZ_FNAME)
		    } else {
			# Set the name of the region mask.
			call strcpy (Memc[vp], mask, sz_mask)
			if (assignop == TOK_COLONEQUALS)
			    IO_NODEFMASK(io) = YES
		    }
		}
		op = start

	    case KW_RECT:
		# Set the source rect or "bounding box" for i/o.  The syntax
		# is somewhat flexible, i.e., "*", ":N", "N:", "M:N" are
		# all accepted ways of expressing the range for an axis.

		IO_VSDEF(io,1) = 1;		IO_VSDEF(io,2) = 1
		IO_VEDEF(io,1) = IO_NCOLS(io);	IO_VEDEF(io,2) = IO_NLINES(io)

		if (Memc[vp] == '[' || Memc[vp] == '(')		# ])
		    vp = vp + 1
		while (Memc[vp] == ' ')
		    vp = vp + 1

		# Get range in X.
		if (Memc[vp] == '*')
		    vp = vp + 1
		else {
		    if (ctoi (Memc, vp, IO_VSDEF(io,1)) <= 0)
			IO_VSDEF(io,1) = 1
		    while (IS_WHITE(Memc[vp]) || Memc[vp] == ':')
			vp = vp + 1
		    if (ctoi (Memc, vp, IO_VEDEF(io,1)) <= 0)
			IO_VEDEF(io,1) = IO_NCOLS(io)
		}

		while (IS_WHITE(Memc[vp]) || Memc[vp] == ',')
		    vp = vp + 1

		# Get range in Y.
		if (Memc[vp] == '*')
		    vp = vp + 1
		else {
		    if (ctoi (Memc, vp, IO_VSDEF(io,2)) <= 0)
			IO_VSDEF(io,2) = 1
		    while (IS_WHITE(Memc[vp]) || Memc[vp] == ':')
			vp = vp + 1
		    if (ctoi (Memc, vp, IO_VEDEF(io,2)) <= 0)
			IO_VEDEF(io,2) = IO_NLINES(io)
		}

		IO_BBUSED(io) = YES
		op = start

	    default:
		# Accumulate EAF expression term.
		F(op) = ',';  op = op + 1
		F(op) = ' ';  op = op + 1
		F(op) = EOS
	    }
	}

	# Verify that the parens etc. match.
	if (level != 0)
	    call syserrs (SYS_QPIOSYN, QP_DFNAME(qp))

	F(op) = EOS
	sz_filter = op
	call realloc (filter, sz_filter, TY_CHAR)

	call qp_closetext (in)
	call sfree (sp)

	return (status)
end
