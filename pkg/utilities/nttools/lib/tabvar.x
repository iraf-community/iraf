include	<tbset.h>

# TABVAR -- Retrieve a table column given its name
#
# B.Simon	03-May-91	Original
# B.Simon	23-Jun-97	Peicewise evaluation of column

procedure tabvar (stack, colname)

pointer	stack		# u: Expression stack pointer
char	colname[ARB]	# i: Column name
#--
include "../tabvar.com"

int	i, coltype, nrows
pointer	sp, nullbuf, buffer, errmsg, cp

string	badcolnam  "Column name not found (%s)"

bool	streq()
int	tbcigi()
pointer	stk_alloc()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Get column pointer from name

	call tbcfnd (tabptr, colname, cp, 1)
	if (cp == NULL) {
	    if (streq (colname, "rownum")) {
		call rowvar (stack)
		return
	    } else {
		call sprintf (Memc[errmsg], SZ_LINE, badcolnam)
		call pargstr (colname)
		call error (1, Memc[errmsg])
	    }
	}

	# Get column type

	coltype = tbcigi (cp, TBL_COL_DATATYPE)
	if (coltype == TY_BOOL || coltype == TY_SHORT || coltype == TY_LONG) {
	    coltype = TY_INT
	} else if (coltype < 0) {
	    coltype = TY_DOUBLE
	}

	# Allocate a buffer on the expression evaluator stack

	nrows = (lastrow - firstrow) + 1
	call malloc (nullbuf, nrows, TY_BOOL)
	buffer = stk_alloc (stack, nrows, coltype)

	# Copy the table column into the buffer
	# Substitute the user supplied vales for nulls

	switch (coltype) {
	case TY_SHORT, TY_INT, TY_LONG:
	    call tbcgti (tabptr, cp, Memi[buffer], Memb[nullbuf], 
			 firstrow, lastrow)
	    do i = 0, nrows-1 {
		if (Memb[nullbuf+i])
		    Memi[buffer+i] = nullval
	    }

	case TY_REAL:
	    call tbcgtr (tabptr, cp, Memr[buffer], Memb[nullbuf], 
			 firstrow, lastrow)
	    do i = 0, nrows-1 {
		if (Memb[nullbuf+i])
		    Memr[buffer+i] = nullval
	    }
	case TY_DOUBLE:
	    call tbcgtd (tabptr, cp, Memd[buffer], Memb[nullbuf], 
			 firstrow, lastrow)
	    do i = 0, nrows-1 {
		if (Memb[nullbuf+i])
		    Memd[buffer+i] = nullval
	    }
	}

	# Update the null array
	call stk_ornull (stack, Memb[nullbuf], nrows)

	call mfree (nullbuf, TY_BOOL)
	call sfree (sp)

end

# ROWVAR -- Handle the variable "rownum"

procedure rowvar (stack)

pointer	stack		# u: Expression stack pointer
#--
include "../tabvar.com"

int	irow, nrows
pointer	buffer

pointer	stk_alloc()

begin
	# Allocate a buffer on the expression evaluator stack

	nrows = (lastrow - firstrow) + 1
	buffer = stk_alloc (stack, nrows, TY_INT)

	# Fill the buffer with the row number
	do irow = 0, nrows-1
	    Memi[buffer+irow] = firstrow + irow
end

