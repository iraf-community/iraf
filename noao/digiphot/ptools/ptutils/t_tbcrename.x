include <tbset.h>

# T_TBCRENAME -- Rename a list of columns in an ST table. 

procedure t_tbcrename ()

int	tlist			# the tables list descriptor
int	columns			# the input columns list descriptor
int	names			# the output column names list descriptor

pointer	sp, table, incname, outcname, tp, colptr
int	clpopnu(), clplen(), clgfil(), access(), tbpsta()
pointer	tbtopn()

begin
	# Open the lists of tables and keywords.
	tlist = clpopnu ("table")
	if (clplen (tlist) <= 0)
	    return
	columns = clpopnu ("columns")
	names = clpopnu ("names")
	if (clplen (columns) != clplen (names))
	    call error (0,
	        "The number of new names does not equal the number of columns")

	# Allocate working space.
	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (incname, SZ_COLNAME, TY_CHAR)
	call salloc (outcname, SZ_COLNAME, TY_CHAR)

	# Loop over the list of ST tables.
	while (clgfil (tlist, Memc[table], SZ_FNAME) != EOF) {

	    # If the file is not an ST table go to the next file in the list.
	    if (access (Memc[table], 0, TEXT_FILE) == YES)
		next
	    iferr (tp = tbtopn (Memc[table], READ_WRITE, 0))
		next
	    if (tbpsta (tp, TBL_WHTYPE) == TBL_TYPE_TEXT)
		next

	    # Loop over the input column list.
	    while (clgfil (columns, Memc[incname], SZ_COLNAME) != EOF &&
	        clgfil (names, Memc[outcname], SZ_COLNAME) != EOF) {

		# If the output column already exists in the table skip
		# to the next input column.
		call tbcfnd (tp, Memc[outcname], colptr, 1)
		if (colptr != NULL)
		    next

		# If the input column does not exist in the table skip to the
		# next column.

		call tbcfnd (tp, Memc[incname], colptr, 1)
		if (colptr == NULL)
		    next

		# Rename the column.
		call tbcnam (tp, colptr, Memc[outcname])
	    }

	    call tbtclo (tp)
	    call clprew (columns)
	    call clprew (names)
	}

	call clpcls (columns)
	call clpcls (names)
	call clpcls (tlist)
	call sfree (sp)
end
