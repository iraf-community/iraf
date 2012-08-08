include	<tbset.h>

#* HISTORY *
#* B.Simon	07-Nov-1994	original
#  Phil Hodge	 8-Apr-1999	call tbfpri

# TCPYONE -- Copy a single table to the output table

procedure tcpyone (input, output)

char	input[ARB]	# i: input table name
char	output[ARB]	# i: output table name
#--
int     numrow, numcol, numptr, type, iptr, irow, jrow
int     colnum, datatype, lendata, lenfmt
int	phu_copied	# returned by tbfpri and ignored
pointer	sp, root, extend, rowselect, colselect, colname, colunits, colfmt
pointer errmsg, icp, ocp, itp, otp, colptr, newcol, pcode

string	nosect  "Sections not permitted on output table name (%s)"
string	nocols  "Column names not found (%s)"

errchk	tbfpri, tbtopn, tctexp, tbracket, trsopen, trseval

bool	trseval(), streq()
int	tbpsta(), tcs_totsize()
pointer	tbtopn(), tcs_column, trsopen()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extend, SZ_FNAME, TY_CHAR)
	call salloc (rowselect, SZ_FNAME, TY_CHAR)
	call salloc (colselect, SZ_FNAME, TY_CHAR)
        call salloc (colname, SZ_COLNAME, TY_CHAR)
        call salloc (colunits, SZ_COLUNITS, TY_CHAR)
        call salloc (colfmt, SZ_COLFMT, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Check output table name for sections

#	call getsects (output, Memc[root], Memc[extend], Memc[rowselect], 
#		       Memc[colselect], SZ_FNAME)

call rdselect (output, Memc[root], Memc[rowselect], Memc[colselect], SZ_FNAME)

	if (Memc[rowselect] != EOS || Memc[colselect] != EOS) {
	    call sprintf (Memc[errmsg], SZ_LINE, nosect)
	    call pargstr (output)
	    call error (1, Memc[errmsg])
	}

	# Break input file names into bracketed sections

#	call getsects (input, Memc[root], Memc[extend], Memc[rowselect], 
#		       Memc[colselect], SZ_FNAME)

call rdselect (input, Memc[root], Memc[rowselect], Memc[colselect], SZ_FNAME)

	if (Memc[rowselect] == EOS && Memc[colselect] == EOS) {
	    # Perform straight file copy if no sections on input name

	    call tbfpri (input, output, phu_copied)
	    call tbtcpy (input, output)

	} else {
            # Open the tables and set output table type

#	    call strcat (Memc[extend], Memc[root], SZ_FNAME)

            itp = tbtopn (Memc[root], READ_ONLY, NULL)
	    call tbfpri (Memc[root], output, phu_copied)
            otp = tbtopn (output, NEW_FILE, NULL)

            type = tbpsta (itp, TBL_WHTYPE)
	    # Support for ASCII output (11/20/96, IB)
	    if (streq (output, "STDOUT"))
	        type = TBL_TYPE_TEXT
            call tbpset (otp, TBL_WHTYPE, type)

            # Create an array of column pointers from the column template

	    numrow = tbpsta (itp, TBL_NROWS)
            numcol = tbpsta (itp, TBL_NCOLS)

            call salloc (colptr, numcol, TY_INT)
	    call salloc (newcol, numcol, TY_INT)

	    call tcs_open (itp, Memc[colselect], Memi[colptr], numptr, numcol)

	    # Take an error exit if no columns were matched

            if (numptr == 0) {
		call sprintf (Memc[errmsg], SZ_LINE, nocols)
		call pargstr (input)
		call error (1, Memc[errmsg])
	    }

	    # Copy column information from the input table to the output table

	    do iptr = 1, numptr {
		icp = tcs_column (Memi[colptr+iptr-1])
		call tbcinf (icp, colnum, Memc[colname], Memc[colunits], 
			     Memc[colfmt], datatype, lendata, lenfmt)

		if (lendata > 1)
		    lendata = tcs_totsize (Memi[colptr+iptr-1])

		call tbcdef (otp, ocp, Memc[colname], Memc[colunits], 
			     Memc[colfmt], datatype, lendata, 1)
		Memi[newcol+iptr-1] = ocp    
	    }

	    # Copy header keywords

	    call tbtcre (otp)
	    call tbhcal (itp, otp)

	    # Copy selected rows from input to output table

	    jrow = 1
	    pcode = trsopen (itp, Memc[rowselect])

	    do irow = 1, numrow {
		if (trseval (itp, irow, pcode)) {
		    call tcpyrow (itp, otp, Memi[colptr], Memi[newcol], 
				  irow, jrow, numptr)
		    jrow = jrow + 1
		}
	    }

	    call trsclose (pcode)
	    call tcs_close (Memi[colptr], numptr)
            call tbtclo (itp)
            call tbtclo (otp)
	}

	call sfree (sp)
end
