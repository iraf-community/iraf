include <tbset.h>

# T_TBKEYCOL -- For all the rows of a list of ST tables, copy the values of
# selected table keywords into new columns of the same name. If the columns
# already exist no action is taken.

procedure t_tbkeycol ()

int	tlist				# the tables list descriptor
int	klist				# the keywords list descriptor

bool	bval
double	dval
int	i, keytype, keyptr, nrows, keylength, ival
pointer	sp, table, keyword, keyvalue, format, tp, colptr
real	rval

bool	itob()
int	clpopnu(), clgfil(), clplen(), tbpsta(), strlen(), access()
int	ctoi(), ctor(), ctod()
pointer	tbtopn()
errchk	tbtopn()

begin
	# Open the lists of tables and keywords.
	tlist = clpopnu ("tables")
	if (clplen (tlist) <= 0)
	    return
	klist = clpopnu ("keywords")
	if (clplen (klist) <= 0)
	    return

	# Allocate working space.
	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)
	call salloc (keyvalue, SZ_PARREC, TY_CHAR)
	call salloc (format, SZ_COLFMT, TY_CHAR)

	# Loop over the list of ST tables.
	while (clgfil (tlist, Memc[table], SZ_FNAME) != EOF) {

	    # If the file is not an ST table go to the next file in the list.
	    if (access(Memc[table], 0, TEXT_FILE) == YES)
		next
	    iferr (tp = tbtopn (Memc[table], READ_WRITE, 0))
		next
	    if (tbpsta (tp, TBL_WHTYPE) == TBL_TYPE_TEXT)
                next


	    # Loop over the keywords.
	    while (clgfil (klist, Memc[keyword], SZ_FNAME) != EOF) {

		# If a column named keyword already exists in the table
		# skip to the next keyword.
		call tbcfnd (tp, Memc[keyword], colptr, 1)
		if (colptr != NULL)
		    next

		# If keyword does not exist in the table skip to the
		# next keyword.
		call tbhfkr (tp, Memc[keyword], keytype, Memc[keyvalue],
		    keyptr)
		if (keyptr == 0)
		    next

		nrows = tbpsta (tp, TBL_NROWS)

		# Decode the header value and copy it into all the rows
		# of the table.
		i = 1
		switch (keytype) {
		case TY_BOOL:
		    call tbcdef (tp, colptr, Memc[keyword], "undefined",
		        "%-3.3b", keytype, 1, 1)
		    if (ctoi (Memc[keyvalue], i, ival) <= 0)
			ival = NO
		    bval = itob (ival)
		    do i = 1, nrows 
			call tbrptb (tp, colptr, bval, 1, i)
		case TY_CHAR:
		    keylength = strlen (Memc[keyvalue])
		    call sprintf (Memc[format], SZ_COLFMT, "%*.*s")
			call pargi (-keylength)
			call pargi (keylength)
		    call tbcdef (tp, colptr, Memc[keyword], "undefined",
		        Memc[format], -keylength, 1, 1)
		    do i = 1, nrows 
			call tbrptt (tp, colptr, Memc[keyvalue], keylength,
			    1, i)
		case TY_INT:
		    keylength  = ctoi (Memc[keyvalue], i, ival)
		    if (keylength <= 0) {
			ival = INDEFI
			keylength = 6
		    }
		    call sprintf (Memc[format], SZ_COLFMT, "%%%d.%dd")
			call pargi (-keylength)
			call pargi (keylength)
		    call tbcdef (tp, colptr, Memc[keyword], "undefined",
		        Memc[format], keytype, 1, 1)
		    do i = 1, nrows 
			call tbrpti (tp, colptr, ival, 1, i)
		case TY_REAL:
		    keylength = ctor (Memc[keyvalue], i, rval)
		    if (keylength <= 0) {
			rval = INDEFR
			keylength = 6
		    }
		    call sprintf (Memc[format], SZ_COLFMT, "%%%dg")
			call pargi (-keylength)
		    call tbcdef (tp, colptr, Memc[keyword], "undefined",
		        Memc[format], keytype, 1, 1)
		    do i = 1, nrows 
			call tbrptr (tp, colptr, rval, 1, i)
		case TY_DOUBLE:
		    keylength = ctod (Memc[keyvalue], i, dval)
		    if (keylength <= 0) {
			dval = INDEFD
			keylength = 6
		    }
		    call sprintf (Memc[format], SZ_COLFMT, "%%%dg")
			call pargi (-keylength)
		    call tbcdef (tp, colptr, Memc[keyword], "undefined",
		        Memc[format], keytype, 1, 1)
		    do i = 1, nrows 
			call tbrptd (tp, colptr, dval, 1, i)
		}

	    }

	    call tbtclo (tp)
	    call clprew (klist)
	}

	call clpcls (klist)
	call clpcls (tlist)
	call sfree (sp)
end
