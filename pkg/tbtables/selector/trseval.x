include	"trs.h"

#* HISTORY *
#* B.Simon	04-Nov-94	original
#* B.Simon	29-Dec-97	revised to use row set

.help trseval 
.nf______________________________________________________________________

This is one of a set of three procedures to select rows of a table
according to a qpoe filter. This procedure evaluates the filter, i.e.,
determines whether it is true or false for a specified row of the
table.  The other two procedures are trsopen(), which compiles the
qpoe filter into the pseudocode used by trseval() and trsclose() which
frees the memory held by the pseudocode arrays. Here is an typical
example of the use of these three routines:

	tp = tbtopn (table, READ_ONLY, NULL)
	numrow = tbpsta (tp, TBL_NROWS)
	pcode = trsopen (tp, filter)
	do irow = 1, numrow {
	    if (trseval (tp, irow, pcode)) {
		# Do something neat here
	    }
	}
	call trsclose (pcode)
	call tbtclo (tp)

For sake of an example, suppose we have a star catalog with the
columns Name, Ra, Dec, V, B-V, and U-B. The simplest sort of filter is
the equality test. The name of the column appears on the left of an
equals sign and the column value appears on the right. For example,
[name=eta_uma]. (The brackets in this and the following example are
not actually part of the filter.)  Column numbers can be used in place
of the column name.  This is especially useful for ascii
tables. Values can be either numbers or strings. It is usually not
necessary to place strings in quotes.  However, any string (including
a column name) contains embedded blanks or characters significant to
the qpoe filter, such a equal signs, commas, or colons, it should be
placed in quotes.

Ranges of values can be specified by giving the endpoints of the
ranges separated by a colon. For example, [v=10:15] selects all rows
with visual magnitude between 10 and 15.  Ranges include their
endpoints. Ranges can also be used with strings as well as
numbers. Ranges can also be one sided. The filter [dec=80:] selects
all rows with declination greater than or equal to eighty degress and
the filter [dec=:-40] selects all declinations less than or equal to
forty degrees south. A filter can contain a list of single values and
ranges. The values in the list should be enclosed in parentheses. For
example, [name=(eta_uma,alpha_lyr)] or [b-v=(-1:0,0.5:1)]. 

Individual values or ranges can be negated by placing a ! in front of
them. For example, [name=!eta_uma] selects every row except the star
named eta_uma and [ra=!0:6] selects all rows except those with right
ascension between zero and six hours. An entire list can be negated by
placing a ! in front of the column name or the parentheses enclosing
the list. The filters [!name=(eta_uma,alpha_lyr)] and
[name=!(eta_uma,alpha_lyr)] and [name=(!eta_uma,!alpha_lyr)] are all
equivalent.

Filters can test more than one column in a table. The individual tests
are separated by commas or semicolons. All tests in the filter must
succeed for the filter to be accepted. For example,
[ra=1.3:1.4,dec=40:42] selects a rectangular region in the catalog. A
range of row numbers can also be selected by placing the word row on
the left side of the equals sign. For example, [row=10:20] selects
rows from ten to twenty inclusive and [row=50:] selects all rows from
fifty on. Row selection can be combined with any other test in a
filter.  A filter, can also be placed in an include file, for example
[@filter.lis]. Include files can be a part of a larger expression 
and include files can contain other files, up to seven levels deep.

.endhelp _________________________________________________________________

# TRSEVAL -- Evaluate a table row selector on a row of a table

bool procedure trseval (tp, irow, pcode)

pointer	tp		# i: table descriptor
int	irow		# i: table row number
pointer	pcode		# i: pseudocode
#--
string	notcode "trseval: not pointer to code"

bool	rst_inset(), trscalc()
errchk	trscalc

begin
	# Make sure this is a valid trs descriptor

	if (TRS_IDENT(pcode) != TRS_MAGIC)
	    call error (1, notcode)

	# Check to see if the row is in the set first
	# if it is, calculate the result of the pseudocode

	if (rst_inset (TRS_ROWS(pcode),irow))
	    if (trscalc (tp,irow, TRS_CODE(pcode)))
		return (true)

	return (false)
end

# TRSCALC -- Calculate the result of the pseudocode embedded in the descriptor

bool procedure trscalc (tp, irow, codebuf)

pointer	tp		# i: table descriptor
int	irow		# i: table row number
pointer	codebuf		# i: pseudocode

#--
bool	jump, stack[MAXSTACK]
double	val
int	itop, icode,junk, mask1, mask2
pointer	sp, str

string	ovflow  "trscalc: stack overflow"
string	badcode "trscalc: bad instruction"

errchk	trsgetd, trsgett
bool	streq(), strle(), strge()
int	trstrim()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	itop = 0
	icode = 0
	jump = false

	repeat {
	    if (itop == MAXSTACK)
		call error (1, ovflow)

	    switch (CODE(codebuf+icode)) {
	    case YDONE:	# end instruction
		break

	    case YRANGE: # range instruction, no-op
		;

	    case YAND: # logical and
		if (! jump) {
		    stack[itop-1] = stack[itop-1] && stack[itop]
		    itop = itop - 1
		}

	    case YOR: # logical or
		if (! jump) {
		    stack[itop-1] = stack[itop-1] || stack[itop]
		    itop = itop - 1
		}

	    case YNOT: # logical not
		stack[itop] = ! stack[itop]

	    case YEQN: # numeric equality test
		call trsgetd (tp, COLUMN(codebuf+icode), irow, val)
		itop = itop + 1
		stack[itop] = val ==  Memd[LOVAL(codebuf+icode)]

	    case YEQS: # string equality check
		call trsgett (tp, COLUMN(codebuf+icode), irow, 
			      Memc[str], SZ_LINE)
		junk = trstrim (Memc[str])

		itop = itop + 1
		stack[itop] = streq (Memc[str], Memc[LOVAL(codebuf+icode)])

	    case YLEN: # numeric less than or equal check
		call trsgetd (tp, COLUMN(codebuf+icode), irow, val)
		itop = itop + 1
		stack[itop] = val <= Memd[LOVAL(codebuf+icode)]

	    case YLES: # string less than or equal check
		call trsgett (tp, COLUMN(codebuf+icode), irow, 
			      Memc[str], SZ_LINE)
		junk = trstrim (Memc[str])

		itop = itop + 1
		stack[itop] = strle (Memc[str], Memc[LOVAL(codebuf+icode)])

	    case YINN: # numeric inclusion check
		call trsgetd (tp, COLUMN(codebuf+icode), irow, val)
		itop = itop + 1
		stack[itop] = val >=  Memd[LOVAL(codebuf+icode)] &&
			      val <=  Memd[HIVAL(codebuf+icode)]

	    case YINS: # string inclusion check
		call trsgett (tp, COLUMN(codebuf+icode), irow, 
			      Memc[str], SZ_LINE)
		junk = trstrim (Memc[str])

		itop = itop + 1
		stack[itop] = strge (Memc[str], Memc[LOVAL(codebuf+icode)]) &&
			      strle (Memc[str], Memc[HIVAL(codebuf+icode)])

	    case YGEN: # numeric greater than or equal check
		call trsgetd (tp, COLUMN(codebuf+icode), irow, val)
		itop = itop + 1
		stack[itop] = val >= Memd[LOVAL(codebuf+icode)]

	    case YGES: # string greater than or equal check
		call trsgett (tp, COLUMN(codebuf+icode), irow, 
			      Memc[str], SZ_LINE)
		junk = trstrim (Memc[str])

		itop = itop + 1
		stack[itop] = strge (Memc[str], Memc[LOVAL(codebuf+icode)])

	    case YMSK: # bit mask
		call trsgetd (tp, COLUMN(codebuf+icode), irow, val)
		mask1 = val
		mask2 = Memd[LOVAL(codebuf+icode)]
		itop = itop + 1
		stack[itop] = and (mask1, mask2) == mask2

	    default:
		call error (1, badcode)
	    }

	    # Set instruction pointer. Peform a jump if the jump field
	    # corresponding to the result is not NULL. Otherwise, 
	    # increment the pointer.

	    if (TJUMP(codebuf+icode) != NULL && stack[itop]) {
		jump = true
		icode = TJUMP(codebuf+icode)
	    } else if (FJUMP(codebuf+icode) != NULL && ! stack[itop]) {
		jump = true
		icode = FJUMP(codebuf+icode)
	    } else {
		jump = false
		icode = icode + SZ_INSTR
	    }
	}

	# This handles the case of an empty program

	if (itop == 0)
	    stack[1] = true

	# Return result

	call sfree (sp)
	return (stack[1])
end

# TRSGETD -- Read double value from table

procedure trsgetd (tp, cp, irow, val)

pointer	tp		# i: table descriptor
pointer	cp		# i: column descriptor
int	irow		# i: column number
double	val		# o: value read from table
#--
errchk	tbegtd

begin
	if (cp == NULL) {
	    val = irow
	} else {
	    call tbegtd (tp, cp, irow, val)
	}
end

# TRSGETT -- Read string value from table

procedure trsgett (tp, cp, irow, str, maxch)

pointer	tp		# i: table descriptor
pointer	cp		# i: column descriptor
int	irow		# i: column number
char	str[ARB]	# o: value read from table
int	maxch		# i: maximum string length
#--
int	junk

errchk	itoc, tbgett
int	itoc()

begin
	if (cp == NULL) {
	    junk = itoc (irow, str, maxch)
	} else {
	    call tbegtt (tp, cp, irow, str, maxch)
	}
end
