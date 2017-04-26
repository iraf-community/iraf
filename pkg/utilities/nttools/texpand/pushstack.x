include	<tbset.h>

define	SZ_COLVAL	SZ_LINE

# The following procedures treat a table as if it were a stack, that is,
# all reading and writing is done at the end of the table. The end of the
# table is indicated by TB_NROWS.
#
# B.Simon	25-Apr-88	Original
# B.Simon	27-Jan-98	Drop temporary tables

# PUSHSTACK -- Push a null row on the top of a table stack

procedure pushstack (tp)

pointer	tp		# i: Table descriptor
#--
int	top
int	tbpsta()

begin
	top = tbpsta (tp, TBL_NROWS) + 1
	call tbtwer (tp, top)
end

# POPSTACK -- Pop the top row from a table stack

procedure popstack (tp)

pointer	tp		# i: Table descriptor
#--
int	top
int	tbpsta()

begin
	top = tbpsta (tp, TBL_NROWS)
	if (top > 0)
	    call tbrdel (tp, top, top)
end

# NUMSTACK -- Return the number of rows in a table stack

int procedure numstack (tp)

pointer	tp		# i: Table descriptor
#--
int	tbpsta()

begin
	return (tbpsta (tp, TBL_NROWS))
end

# INITSTACK -- Initialize a table stack and return its descriptor

pointer procedure initstack (tp, extra)

pointer	tp		# i: Table to use as a template for the table stack
char	extra[ARB]	# i: Extra columns to add to the table stack
#--
char	comma
int	ic, jc
pointer	sp, cp, stack, colname, colunits, colfmt, tmproot, tmpfile

int	stridx()
pointer	tbtopn()

errchk	tbtopn, tbtcre

begin
	# Set up arrays in dynamic memory

	call smark (sp)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (colunits, SZ_COLUNITS, TY_CHAR)
	call salloc (colfmt, SZ_COLFMT, TY_CHAR)
	call salloc (tmproot, SZ_FNAME, TY_CHAR)
	call salloc (tmpfile, SZ_FNAME, TY_CHAR)

	# Create the stack table

	call mktemp ("tmp$stk", Memc[tmproot], SZ_FNAME)
	call tbtext (Memc[tmproot], Memc[tmpfile], SZ_FNAME)
	stack = tbtopn (Memc[tmpfile], NEW_COPY, tp)

	# Set up column information that will not vary across columns

	Memc[colunits] = EOS
	Memc[colfmt] = EOS

	# Add column names from the extra string

	ic = 1
	comma = ','
	repeat {

	    # Copy the next comma delimeted column name

	    jc = stridx (comma, extra[ic])
	    if (jc == 0)
		call strcpy (extra[ic], Memc[colname], SZ_COLNAME)
	    else
		call strcpy (extra[ic], Memc[colname], jc-1)
	    ic = ic + jc

	    # Create the new column

	    if (Memc[colname] != EOS)
		call tbcdef (stack, cp, Memc[colname], Memc[colunits], 
			     Memc[colfmt], TY_INT, 1, 1)

	} until (jc == 0)

	# Return the stack table descriptor

	call tbtcre (stack)
	call sfree (sp)

	return (stack)
end

# FREESTACK -- Close and delete a table stack

procedure freestack (tp)

pointer	tp		# i: Table descriptor
#--
pointer	sp, table

begin
	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)

	call tbtnam (tp, Memc[table], SZ_FNAME)
	call tbtclo (tp)

	call delete (Memc[table])
	call sfree (sp)
end

# PUTSTACKT -- Put a text string in the top row of a table stack

int procedure putstackt (tp, colname, colval)

pointer	tp		# i: Table descriptor
char	colname[ARB]	# i: Column name
char	colval[ARB]	# i: Column value
#--
int	top, found
pointer	cp

int	tbpsta()

begin
	top = tbpsta (tp, TBL_NROWS)
	call tbcfnd (tp, colname, cp, 1)

	found = NO
	if (cp != NULL) {
	    ifnoerr {
		call tbrptt (tp, cp, colval, ARB, 1, top)
	    } then {
		found = YES
	    }
	}

	return (found)
end

# PUTSTACKI -- Put an integer  in the top row of a table stack

procedure putstacki (tp, colname, colval)

pointer	tp		# i: Table descriptor
char	colname[ARB]	# i: Column name
int	colval		# i: Column value
#--
int	top
pointer	cp

int	tbpsta()

begin
	top = tbpsta (tp, TBL_NROWS)

	call tbcfnd (tp, colname, cp, 1)
	call tbepti (tp, cp, top, colval)

end

# ANDSTACK -- Combine the top two rows of the table stack

procedure andstack (tp)

pointer	tp		# i: Table descriptor
#--
int	top
int	tbpsta()

begin
	top = tbpsta (tp, TBL_NROWS)

	call movtbrow (tp, top, tp, top-1)
	call tbrdel (tp, top, top)
end

# MOVSTACK -- Move the top row of one table stack to another

procedure movstack (rtp, wtp)

pointer	rtp		# i: Table descriptor of table read from
pointer	wtp		# i: Table descriptor of table written to
#--
int	rtop, wtop

int	tbpsta()

begin
	call pushstack (wtp)

	rtop = tbpsta (rtp, TBL_NROWS)
	wtop = tbpsta (wtp, TBL_NROWS)

	call movtbrow (rtp, rtop, wtp, wtop)
	call tbrdel (rtp, rtop, rtop)

end
