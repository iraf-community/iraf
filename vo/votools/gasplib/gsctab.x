include	<tbset.h>

define	NUM_COLS	5	# Number of table columns

procedure gsctab (tp, cd, nrows)

pointer	tp			# GSC table descriptor
pointer	cd[NUM_COLS]		# Table column descriptors
int	nrows			# Number of rows in table

pointer	sp, tname
char	colname[SZ_COLNAME,NUM_COLS]
pointer	null

pointer	tbtopn()
int	tbpsta()

begin
	call smark (sp)
	call salloc (tname, SZ_FNAME, TY_CHAR)
	call clgstr ("gsctable", Memc[tname], SZ_FNAME)

	# Open the GSC table
	tp = tbtopn (Memc[tname], READ_WRITE, 0)

	# Number of rows (coordinates) in the table
	nrows = tbpsta (tp, TBL_NROWS)

	call strcpy ("RA_DEG",  colname[1,1], SZ_COLNAME)
	call strcpy ("DEC_DEG", colname[1,2], SZ_COLNAME)
	call strcpy ("x_pix",   colname[1,3], SZ_COLNAME)
	call strcpy ("y_pix",   colname[1,4], SZ_COLNAME)
	call strcpy ("valid",   colname[1,5], SZ_COLNAME)

	# Find the table columns
	call tbcfnd (tp, colname, cd, NUM_COLS)

	if (cd[1] <= 0)
	    call error (0, "No R.A. column found in table")

	if (cd[2] <= 0)
	    call error (0, "No Dec. column found in table")

	if (cd[3] <= 0)
	    # Create the output (pixel coordinate) column
	    call tbcdef (tp, cd[3], colname[1,3], "pixels", 
		"%6.1f", TY_REAL, 0, 1)

	if (cd[4] <= 0)
	    # Create the output (pixel coordinate) column
	    call tbcdef (tp, cd[4], colname[1,4], "pixels", 
		"%6.1f", TY_REAL, 0, 1)

	if (cd[5] <= 0) {
	    # Create the output (valid flag) column
	    call tbcdef (tp, cd[5], colname[1,5], EOS,
		"%1b", TY_INT, 0, 1)

	    # Initialize to false
	    call salloc (null, nrows, TY_INT)
	    call amovki (NO, Memi[null], nrows)
	    call tbcpti (tp, cd[5], Memi[null], 1, nrows)
	}

	call sfree (sp)
end
