include	<tbset.h>

#  TINSERT  --  Perform the actual insertion.
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-97  -  Task created (I.Busko)
#  17-Mar-97  -  Added selrows function (IB)
#   8-Apr-02  -  Remove the call to whatfile (P. Hodge)


procedure tinsert (list, output, otp, cpo, ncpo, row, rflag, verbose, 
                   rowsel, colsel, colname, colunits, colfmt)

pointer	list		# i: input list
char	output[ARB]	# i: output table name
pointer	otp		# i: output table descriptor
pointer	cpo		# i: output column descriptors
int	ncpo		# i: output number of columns
int	row		# i: row where to begin insertion
bool	rflag		# i: read row from header ?
bool	verbose		# i: print info ?
char	rowsel[ARB]	# i: work string for row selector
char	colsel[ARB]	# i: work string for column selector
char	colname[ARB]	# i: work string for column names
char	colunits[ARB]	# i: work string for column units
char	colfmt[ARB]	# i: work string for column formats
#--
pointer	sp, itp, fname, root, cpi
int	i, file, hrow, numrow, numcol, nrows, ncpi

errchk	ticopy

pointer	tbtopn()
int	imtgetim(), imtlen(), tihrow(), tbpsta(), selrows()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (root,  SZ_FNAME,    TY_CHAR)

	# Loop over input list.
	do file = 1, imtlen(list) {

	    # Get input table name.
	    i = imtgetim (list, Memc[fname], SZ_PATHNAME)

	    # Break input file name into bracketed selectors.
	    call rdselect (Memc[fname], Memc[root], rowsel, colsel, SZ_FNAME)

	    # Open input table and get some info.
	    itp = tbtopn (Memc[fname], READ_ONLY, NULL)
	    numrow = tbpsta (itp, TBL_NROWS)
	    numcol = tbpsta (itp, TBL_NCOLS)

	    # See if original row information is stored in header.
	    # If so, and user asked for, use it.
	    hrow = tihrow (itp)
	    if (rflag) {
	        if (hrow > 0)
	            row = hrow
	        else
	            call error (1, "No valid row.")
	    }

	    # Find how many rows were requested by row selector.
	    nrows = selrows (itp, rowsel)

	    # Create array of column pointers from column selector. 
	    call malloc (cpi, numcol, TY_INT)
	    call tcs_open (itp, colsel, Memi[cpi], ncpi, numcol)

	    if (verbose) {
	        call printf ("%s -> %s row=%d  \n")
	        call pargstr (Memc[fname])
	        call pargstr (output)
	        call pargi (row)
	        call flush (STDOUT)
	    }

	    # Copy current input table into current row of output table.
	    call ticopy (itp, cpi, ncpi, otp, cpo, ncpo, rowsel, row, nrows,
                         colname, colunits, colfmt)

	    # Free input table's array of column pointers.
	    call tcs_close (Memi[cpi], ncpi)
	    call mfree (cpi, TY_INT)

	    # Close input table.
	    call tbtclo (itp)

	    # Bump row counter.
	    row = row + 1
	}

	call sfree (sp)
end
