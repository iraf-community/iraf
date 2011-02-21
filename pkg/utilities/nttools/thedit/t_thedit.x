include	<error.h>
include	<evexpr.h>
include <ctype.h>
include	<lexnum.h>
include	<tbset.h>

define	SZ_TABLENAME	(SZ_FNAME)	# max size of a table name
define	SZ_KEYWORDNAME	31		# max size of a keyword name

define	OP_EDIT		1		# hedit opcodes
define	OP_DELETE	2
define	OP_PRINT	3


# thedit -- Edit or view selected keywords of a table header or headers.  This
# editor performs a single edit operation upon a relation, e.g., upon a set
# of keywords of a set of tables.  Templates and expressions may be used to
# automatically select the tables and keywords to be edited, and to compute
# the new value of each keyword.
#
# Phil Hodge, 10-May-2000  Task created, based on hedit.
# Phil Hodge, 26-May-2000  When adding a keyword, check for invalid characters.
# Phil Hodge, 31-May-2000  Add "keywords" i_nrows, etc.
# Phil Hodge, 19-Jul-2000  In he_getop, call tkw_special before tbhgtt,
#		rather than explicitly checking for $I.
# Phil Hodge,  8-Sep-2000  Require value = "\." or "\," in order to actually
#		set a keyword value to "." or ",".  ("\," is for protection
#		against accidentally typing "," instead of ".".)
#		In he_add_keyword, include the new value in the message
#		(if show=yes).  In he_put_keyword, include both the old and
#		new values in the message.
# Phil Hodge,  4-Mar-2002  Call xev_freeop to free memory allocated by evexpr.
# Phil Hodge,  1-Apr-2003  Fix incorrect calling sequence for tkw_open
#		in he_delete.

procedure t_thedit()

pointer	keywords		# template listing keywords to be processed
pointer	valexpr			# the value expression (if op=edit|add)

pointer tnt
pointer	sp, s_valexpr, table, template, buf
pointer tp			# pointer to table struct
pointer kw			# pointer to table keyword struct
pointer vip			# for deleting whitespace in valexpr
pointer newval			# valexpr after evaluation
int	operation, show
int	ip, ctowrd()
int	nkw, tkw_len()		# number of keywords that match the template
int	dtype			# data type of expression

pointer	tbtopn()
pointer tkw_open()
bool	clgetb(), streq()
bool	tbhisc()
int	btoi(), tbnopenp(), tbnget()
int	i, strlen()
errchk	he_print, he_delete, he_add_keyword, he_put_keyword, he_evaluate

begin
	call smark (sp)
	call salloc (buf,       SZ_FNAME, TY_CHAR)
	call salloc (table,     SZ_FNAME, TY_CHAR)
	call salloc (keywords,  SZ_LINE,  TY_CHAR)
	call salloc (template,  SZ_FNAME, TY_CHAR)
	call salloc (s_valexpr, SZ_LINE,  TY_CHAR)
	call salloc (newval,    SZ_LINE,  TY_CHAR)

	# Get the list of table names.
	tnt = tbnopenp ("table")

	# Determine type of operation to be performed.  The default operation
	# is edit.

	operation = OP_EDIT
	if (clgetb ("delete"))
	    operation = OP_DELETE

	# Get list of keywords to be edited, added, or deleted.
	call clgstr ("keywords", Memc[keywords], SZ_LINE)
	do i = 1, strlen (Memc[keywords]) {
	    if (Memc[keywords+i-1] == ',')
		Memc[keywords+i-1] = ' '	# replace comma with blank
	}

	# The value expression parameter is not used for the delete operation.
	if (operation != OP_DELETE) {
	    call clgstr ("value", Memc[s_valexpr], SZ_LINE)
	    for (vip=s_valexpr;  IS_WHITE (Memc[vip]);  vip=vip+1)
		;
	    valexpr = vip
	    while (Memc[vip] != EOS)
		vip = vip + 1
	    while (vip > valexpr && IS_WHITE (Memc[vip-1]))
		vip = vip - 1
	    Memc[vip] = EOS
	} else {
	    Memc[s_valexpr] = EOS
	    valexpr = s_valexpr
	}
	# Check for value = ",", which could be a typo.
	if (streq (Memc[valexpr], ",")) {
	    call error (1,
	"In order to set a keyword value to ',' you must use value='\,'")
	} else if (streq (Memc[valexpr], "\,")) {
	    call strcpy (",", Memc[valexpr], SZ_LINE)
	}

	# Get switches.  If the expression value is ".", meaning print value
	# rather than edit, then we do not use the switches.

	if (streq (Memc[valexpr], ".")) {
	    operation = OP_PRINT
	    show = NO
	} else {
	    show = btoi (clgetb ("show"))
	}

	# In order to set the keyword value to ".", specify value="\.".
	if (streq (Memc[valexpr], "\."))
	    call strcpy (".", Memc[valexpr], SZ_LINE)

	# Main processing loop.  A table is processed in each pass through
	# the loop.

	while (tbnget (tnt, Memc[table], SZ_FNAME) != EOF) {

	    # Open the current table.
	    iferr {
		if (operation == OP_PRINT)
		    tp = tbtopn (Memc[table], READ_ONLY,  NULL)
		else
		    tp = tbtopn (Memc[table], READ_WRITE, NULL)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    # Get a list of all the keywords in the header.
	    kw = tkw_open (tp)

	    # for each keyword or template in blank-separated list ...
	    ip = 1
	    while (ctowrd (Memc[keywords], ip, Memc[template], SZ_FNAME) > 0) {

		# Find all keywords that match the current keyword template.
		call tkw_find (tp, kw, Memc[template])
		nkw = tkw_len (kw)

		if (operation == OP_PRINT) {

		    call he_print (tp, kw, Memc[table], Memc[template])

		} else if (operation == OP_DELETE) {

		    call he_delete (tp, kw, Memc[table], Memc[template], show)

		} else {

		    # interpret the value string
		    call he_getopsettable (tp, Memc[table], Memc[template])
		    call he_evaluate (Memc[valexpr],
				Memc[newval], SZ_LINE, dtype)

		    # No keywords match the template, or the keyword is
		    # history or comment?
		    if (nkw == 0 || tbhisc (Memc[template])) {

			# Add a new keyword.
			call he_add_keyword (tp, Memc[table], Memc[template],
				Memc[newval], dtype, show)

		    } else {

			call he_put_keyword (tp, kw, Memc[table],
				Memc[template], Memc[newval], dtype, show)
		    }
		}
	    }

	    # Close the keyword list and the table.
	    call tkw_close (kw)
	    call tbtclo (tp)

	    call flush (STDOUT)
	}

	call tbnclose (tnt)
	call sfree (sp)
end


# This routine prints the value of the keyword or keywords that match
# the template.

procedure he_print (tp, kw, table, template)

pointer tp		# i: pointer to table struct
pointer kw		# i: pointer to keyword struct
char	table[ARB]	# i: table name
char	template[ARB]	# i: keyword name or template (for warning message)
#--
pointer sp
pointer value, comment
char	keyword[SZ_KEYWORD]	# keyword name
int	nkw		# number of keywords
int	k
int	tkw_len()

begin
	nkw = tkw_len (kw)

	if (nkw == 0) {
	    call eprintf ("Warning:  keyword(s) `%s' not found.\n")
		call pargstr (template)
	} else {
	    call smark (sp)
	    call salloc (value, SZ_FNAME, TY_CHAR)
	    call salloc (comment, SZ_FNAME, TY_CHAR)
	    do k = 1, nkw {
		call he_gval (tp, kw, k,
			keyword, Memc[value], Memc[comment], SZ_FNAME)
		call printf ("%s,%s = %s")
		    call pargstr (table)
		    call pargstr (keyword)
		    call he_pargstr (Memc[value])
		if (Memc[comment] != EOS) {
		    call printf (" / %s")
			call pargstr (Memc[comment])
		}
		call printf ("\n")
	    }
	    call sfree (sp)
	}
end

procedure he_gval (tp, kw, k, keyword, value, comment, maxch)

pointer tp			# i: pointer to table struct
pointer kw			# i: pointer to keyword struct
int	k			# i: index in list of matched keywords
char	keyword[SZ_KEYWORD]	# o: keyword name
char	value[ARB]		# o: value of keyword
char	comment[ARB]		# o: comment, or null
int	maxch			# i: size of value and comment strings
#--
pointer sp
pointer sval
int	i
int	keynum			# index in list of all keywords in header
int	dtype			# data type of keyword
bool	tbhisc()
errchk	tbhgnp, tbhgcm, tkw_special

begin
	call smark (sp)
	call salloc (sval, SZ_FNAME, TY_CHAR)

	call tkw_getkw (kw, k, keynum, keyword, SZ_KEYWORD)

	if (keynum > 0) {

	    call tbhgnp (tp, keynum, keyword, dtype, Memc[sval])

	    # Delete leading whitespace.
	    do i = 0, SZ_FNAME-1 {
		if (Memc[sval+i] == EOS)
		    break
		if (!IS_WHITE(Memc[sval+i]))
		    break
	    }
	    call strcpy (Memc[sval+i], value, maxch)

	    if (tbhisc (keyword))
		comment[1] = EOS
	    else
		call tbhgcm (tp, keyword, comment, maxch)

	} else {

	    call tkw_special (tp, keyword, value, maxch)
	    comment[1] = EOS
	}

	call sfree (sp)
end

# This routine deletes one or more keywords from the header.
# The list of all keywords in the header and the list of keywords that
# match the template will be reassigned after deleting.

procedure he_delete (tp, kw, table, template, show)

pointer tp		# i: pointer to table struct
pointer kw		# io: pointer to keyword struct
char	table[ARB]	# i: table name
char	template[ARB]	# i: keyword name or template
int	show		# i: print info?
#--
char	keyword[SZ_KEYWORD]	# keyword name
int	nkw		# number of keywords
int	keynum		# index in list of all keywords in header
int	k
pointer tkw_open()
int	tkw_len()
errchk	tbhdel

begin
	nkw = tkw_len (kw)

	if (nkw == 0) {
	    call eprintf ("Warning:  keyword(s) `%s' not found.\n")
		call pargstr (template)
	} else {

	    do k = nkw, 1, -1 {
		call tkw_getkw (kw, k, keynum, keyword, SZ_KEYWORD)
		if (keynum <= 0) {
		    call eprintf (
			"Warning:  can't delete special keyword %s.\n")
			call pargstr (keyword)
		    next
		}
		call tbhdel (tp, keynum)
		if (show == YES) {
		    call printf ("%s,%s deleted\n")
			call pargstr (table)
			call pargstr (keyword)
		}
	    }

	    # Update the list of the current keywords, since we've deleted some.
	    call tkw_close (kw)
	    kw = tkw_open (tp)
	}
end

# This routine adds a new keyword to the header.

procedure he_add_keyword (tp, table, keyword, newval, dtype, show)

pointer tp		# i: pointer to table struct
char	table[ARB]	# i: table name
char	keyword[ARB]	# i: keyword name or template
char	newval[ARB]	# i: value to assign to keyword
int	dtype		# i: data type of newval
int	show		# i: print info?
#--
int	i
bool	bval
int	ival
real	rval
double	dval
int	nscan()
errchk	tbhadd, tbhadr, tbhadi, tbhadb, tbhadt

begin
	# Check that the keyword name is valid.
	do i = 1, SZ_KEYWORD {

	    if (keyword[i] == EOS)
		break

	    if (keyword[i] == '*' || keyword[i] == '?') {
		call eprintf (
	"Warning:  keyword `%s' doesn't match any keyword in the header;\n")
		    call pargstr (keyword)
		call eprintf ("  this keyword template will be ignored.\n")
		return
	    }

	    # All the following are OK:
	    if (IS_UPPER(keyword[i]))
		next
	    if (IS_LOWER(keyword[i]))
		next
	    if (IS_DIGIT(keyword[i]))
		next
	    if (keyword[i] == '_' || keyword[i] == '-')
		next

	    # If we get here, the character is invalid.
	    call eprintf ("Warning:  invalid character `%c' in keyword `%s';\n")
		call pargc (keyword[i])
		call pargstr (keyword)
	    call eprintf ("  this keyword will not be added to the header.\n")
	    return
	}

	switch (dtype) {
	case TY_DOUBLE:
	    call sscan (newval)
		call gargd (dval)
	    if (nscan() < 1) {
		call eprintf ("can't interpret %s as a floating point value\n")
		    call pargstr (newval)
		call error (1, "")
	    }
	    call tbhadd (tp, keyword, dval)

	case TY_REAL:
	    call sscan (newval)
		call gargr (rval)
	    if (nscan() < 1) {
		call eprintf ("can't interpret %s as a floating point value\n")
		    call pargstr (newval)
		call error (1, "")
	    }
	    call tbhadr (tp, keyword, rval)

	case TY_INT:
	    call sscan (newval)
		call gargi (ival)
	    if (nscan() < 1) {
		call eprintf ("can't interpret %s as an integer\n")
		    call pargstr (newval)
		call error (1, "")
	    }
	    call tbhadi (tp, keyword, ival)

	case TY_BOOL:
	    call sscan (newval)
		call gargb (bval)
	    if (nscan() < 1) {		# shouldn't happen
		call eprintf ("can't interpret %s as a boolean value\n")
		    call pargstr (newval)
		call error (1, "")
	    }
	    call tbhadb (tp, keyword, bval)

	default:
	    call tbhadt (tp, keyword, newval)
	}

	if (show == YES) {
	    call printf ("add %s,%s = %s\n")
		call pargstr (table)
		call pargstr (keyword)
		call he_pargstr (newval)
	}
end

procedure he_put_keyword (tp, kw, table, template, newval, dtype, show)

pointer tp		# i: pointer to table struct
pointer kw		# i: pointer to keyword struct
char	table[ARB]	# i: table name
char	template[ARB]	# i: keyword name or template
char	newval[ARB]	# i: value to assign to keyword
int	dtype		# i: data type of newval
int	show		# i: print info?
#--
bool	bval
int	ival
real	rval
double	dval
char	oldval[SZ_FNAME]	# current value of keyword (if show is YES)
char	keyword[SZ_KEYWORD]	# name of current keyword
int	keynum		# index in list of all keywords in header
int	k
int	nkw, tkw_len()
int	nscan()
errchk	tbhptd, tbhptr, tbhpti, tbhptb, tbhptt

begin
	nkw = tkw_len (kw)

	# for each keyword that matches the template ...
	do k = 1, nkw {

	    call tkw_getkw (kw, k, keynum, keyword, SZ_KEYWORD)
	    if (keynum <= 0) {
		call eprintf ("Warning:  can't modify special keyword %s.\n")
		    call pargstr (keyword)
		next
	    }

	    if (show == YES) {		# get the current value
		call tbhgtt (tp, keyword, oldval, SZ_FNAME)
	    }

	    switch (dtype) {
	    case TY_DOUBLE:
		call sscan (newval)
		    call gargd (dval)
		if (nscan() < 1) {
		    call eprintf (
			"can't interpret %s as a floating point value\n")
			call pargstr (newval)
		    call error (1, "")
		}
		call tbhptd (tp, keyword, dval)

	    case TY_REAL:
		call sscan (newval)
		    call gargr (rval)
		if (nscan() < 1) {
		    call eprintf (
			"can't interpret %s as a floating point value\n")
			call pargstr (newval)
		    call error (1, "")
		}
		call tbhptr (tp, keyword, rval)

	    case TY_INT:
		call sscan (newval)
		    call gargi (ival)
		if (nscan() < 1) {
		    call eprintf ("can't interpret %s as an integer\n")
			call pargstr (newval)
		    call error (1, "")
		}
		call tbhadi (tp, keyword, ival)

	    case TY_BOOL:
		call sscan (newval)
		    call gargb (bval)
		if (nscan() < 1) {		# shouldn't happen
		    call eprintf ("can't interpret %s as a boolean value\n")
			call pargstr (newval)
		    call error (1, "")
		}
		call tbhadb (tp, keyword, bval)

	    default:
		call tbhptt (tp, keyword, newval)
	    }

	    if (show == YES) {
		call printf ("%s,%s updated:  %s -> %s\n")
		    call pargstr (table)
		    call pargstr (keyword)
		    call he_pargstr (oldval)
		    call he_pargstr (newval)
	    }
	}
end

# This routine copies the value from valexpr to newval and interprets
# the data type of the result.  If valexpr begins with "(", it will be
# passed to evexpr to evaluate it, and the resulting string will be
# returned as newval.

procedure he_evaluate (valexpr, newval, maxch, dtype)

char	valexpr[ARB]		# i: value expression
char	newval[ARB]		# o: value
int	maxch			# i: size of newval
int	dtype			# o: data type of expression
#--
pointer o			# evexpr pointer
pointer evexpr()
int	locpr()
bool	streq()
int	he_dtype()
extern	he_getop()

begin
	if (streq (valexpr, ".")) {

	    call strcpy (valexpr, newval, maxch)
	    dtype = TY_CHAR		# irrelevant

	} else if (valexpr[1] == '(') {

	    # Evaluate the expression given in parentheses.
	    o = evexpr (valexpr, locpr (he_getop), 0)

	    switch (O_TYPE(o)) {	# evexpr only supports these data types
	    case TY_BOOL:
		dtype = TY_BOOL
		call sprintf (newval, maxch, "%b")
		    call pargb (O_VALB(o))
	    case TY_CHAR:
		dtype = TY_CHAR
		call sprintf (newval, maxch, "%s")
		    call pargstr (O_VALC(o))
	    case TY_INT:
		dtype = TY_INT
		call sprintf (newval, maxch, "%d")
		    call pargi (O_VALI(o))
	    case TY_REAL:
		dtype = TY_REAL
		call sprintf (newval, maxch, "%g")
		    call pargr (O_VALR(o))
	    default:
		call error (1, "unknown expression datatype")
	    }
	    call xev_freeop (o)
	    call mfree (o, TY_STRUCT)

	} else {

	    # Interpret the data type, and copy the string from valexpr to
	    # newval.
	    dtype = he_dtype (valexpr, newval, maxch)
	}
end

# This function returns the data type of value, and it copies value to
# newval.  If the data type is boolean, don't complain if the user gave
# the value in a nonstandard form, such as "T" or "F", but then assign
# the standard "yes" or "no" to newval (that's the reason for copying
# value to newval).

int procedure he_dtype (value, newval, maxch)

char	value[ARB]	# i: the value encoded as a string
char	newval[ARB]	# o: same as lower case value, unless type is boolean
int	maxch		# i: max size of newval
#--
int	dtype		# the data type, to be returned
bool	numeric
int	tok_type, ip, numlen
int	lexnum()
int	strlen()
bool	streq()

begin
	# Use newval for scratch, to convert to lower case for the
	# tests on boolean data type.
	call strcpy (value, newval, maxch)
	call strlwr (newval)

	if (streq (newval, "yes") ||
	    streq (newval, "true") ||
	    streq (newval, "t")) {

	    dtype = TY_BOOL

	    call strcpy ("yes", newval, maxch)

	} else if (streq (newval, "no") ||
		   streq (newval, "false") ||
		   streq (newval, "f")) {

	    dtype = TY_BOOL

	    call strcpy ("no", newval, maxch)

	} else {

	    ip = 1
	    tok_type = lexnum (value, ip, numlen)
	    numeric = (tok_type != LEX_NONNUM && numlen == strlen (value))

	    if (numeric) {
		if (tok_type == LEX_OCTAL || tok_type == LEX_DECIMAL ||
		    tok_type == LEX_HEX) {
		    dtype = TY_INT
		} else if (tok_type == LEX_REAL) {
		    dtype = TY_DOUBLE
		} else {
		    dtype = TY_CHAR		# shouldn't happen
		}
	    } else {
		dtype = TY_CHAR
	    }

	    call strcpy (value, newval, maxch)
	}

	return (dtype)
end

# HE_GETOP -- Satisfy an operand request from EVEXPR.  The value of the
# current keyword is gotten from the table header.
#
# Note that HE_GETOPSETTABLE must first have been called to save the
# table pointer and keyword name in the common block.

procedure he_getop (operand, o)

char	operand[ARB]		# operand name
pointer	o			# operand (output)

pointer sp
pointer keyword		# scratch for current keyword name
pointer value		# scratch for value
pointer newvalue	# value in lower case; "yes" or "no" for bool value
int	dtype		# data type of keyword
pointer	h_tp		# getop common
char	h_table[SZ_TABLENAME]
char	h_keyword[SZ_KEYWORDNAME]
common	/hegop2/ h_tp, h_table, h_keyword
int	he_dtype()
bool	streq()
errchk	tbhgtt

begin
	call smark (sp)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (newvalue, SZ_FNAME, TY_CHAR)
	call salloc (keyword, SZ_KEYWORDNAME, TY_CHAR)

	if (streq (operand, "$"))
	    call strcpy (h_keyword, Memc[keyword], SZ_KEYWORDNAME)
	else
	    call strcpy (operand, Memc[keyword], SZ_KEYWORDNAME)

	# Get the value and interpret its data type.
	iferr {
	    call tkw_special (h_tp, Memc[keyword], Memc[value], SZ_FNAME)
	} then {
	    call tbhgtt (h_tp, Memc[keyword], Memc[value], SZ_FNAME)
	}

	dtype = he_dtype (Memc[value], Memc[newvalue], SZ_FNAME)

	switch (dtype) {
	case TY_BOOL:
	    call xev_initop (o, 0, TY_BOOL)
	    O_VALB(o) = (streq (Memc[newvalue], "yes"))

	case TY_SHORT, TY_INT, TY_LONG:
	    call xev_initop (o, 0, TY_INT)
	    call sscan (Memc[value])
		call gargi (O_VALI(o))

	case TY_REAL, TY_DOUBLE, TY_COMPLEX:
	    call xev_initop (o, 0, TY_REAL)
	    call sscan (Memc[value])
		call gargr (O_VALR(o))

	default:
	    call xev_initop (o, SZ_LINE, TY_CHAR)
	    call strcpy (Memc[value], O_VALC(o), SZ_LINE)
	}

	call sfree (sp)
end


# HE_GETOPSETTABLE -- Copy the table pointer, table name, and keyword name
# to a common block in preparation for a getop call by EVEXPR.

procedure he_getopsettable (tp, table, keyword)

pointer	tp			# table descriptor of table to be edited
char	table[ARB]		# name of table to be edited
char	keyword[ARB]		# name of keyword to be edited

pointer	h_tp			# getop common
char	h_table[SZ_TABLENAME]
char	h_keyword[SZ_KEYWORDNAME]
common	/hegop2/ h_tp, h_table, h_keyword

begin
	h_tp = tp
	call strcpy (table, h_table, SZ_TABLENAME)
	call strcpy (keyword, h_keyword, SZ_KEYWORDNAME)
end


# HE_ENCODEOP -- Encode an operand as returned by EVEXPR as a string.  EVEXPR
# operands are restricted to the datatypes bool, int, real, and string.

procedure he_encodeop (o, outstr, maxch)

pointer	o			# operand to be encoded
char	outstr[ARB]		# output string
int	maxch			# max chars in outstr

begin
	switch (O_TYPE(o)) {
	case TY_BOOL:
	    call sprintf (outstr, maxch, "%b")
		call pargb (O_VALB(o))
	case TY_CHAR:
	    call sprintf (outstr, maxch, "%s")
		call pargstr (O_VALC(o))
	case TY_INT:
	    call sprintf (outstr, maxch, "%d")
		call pargi (O_VALI(o))
	case TY_REAL:
	    call sprintf (outstr, maxch, "%g")
		call pargr (O_VALR(o))
	default:
	    call error (1, "unknown expression datatype")
	}
end


# HE_PARGSTR -- Pass a string to a printf statement, enclosing the string
# in quotes if it contains any whitespace.

procedure he_pargstr (str)

char	str[ARB]		# string to be printed
int	ip
bool	quoteit
pointer	sp, op, buf

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	op = buf
	Memc[op] = '"'
	op = op + 1

	# Copy string to scratch buffer, enclosed in quotes.  Check for
	# embedded whitespace.

	quoteit = false
	for (ip=1;  str[ip] != EOS;  ip=ip+1) {
	    if (IS_WHITE(str[ip])) {		# detect whitespace
		quoteit = true
		Memc[op] = str[ip]
	    } else if (str[ip] == '\n') {	# prettyprint newlines
		Memc[op] = '\\'
		op = op + 1
		Memc[op] = 'n'
	    } else				# normal characters
		Memc[op] = str[ip]

	    if (ip < SZ_LINE)
		op = op + 1
	}

	# If whitespace was seen pass the quoted string, otherwise pass the
	# original input string.

	if (quoteit) {
	    Memc[op] = '"'
	    op = op + 1
	    Memc[op] = EOS
	    call pargstr (Memc[buf])
	} else
	    call pargstr (str)

	call sfree (sp)
end
