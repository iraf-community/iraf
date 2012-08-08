include	<tbset.h>

# WPROJECT -- Copy selected columns and rows to output table
#
# B.Simon	19-Oct-87	First Code
# B.Simon	30-Apr-1999	Replace call to unique with nextuniq

procedure wproject (itp, otp, numptr, colptr, uniq)

pointer	itp		# i: Input table descriptor
pointer	otp		# i: Output table descriptor
int	numptr		# i: Number of column pointers
pointer	colptr[ARB]	# i: Array of column pointers
bool	uniq		# i: Only output unique rows?
#--
int	iptr, irow, jrow, nrow
int	colnum[1], datatype[1], lendata[1], lenfmt[1]
pointer	sp, ocp, newcol, colname, colunits, colfmt

int	tbpsta()

begin
	# Set up arrays in dynamic memory

	call smark (sp)
	call salloc (newcol, numptr, TY_INT)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (colunits, SZ_COLUNITS, TY_CHAR)
	call salloc (colfmt, SZ_COLFMT, TY_CHAR)


	# Copy column information from the input table to the output table

	do iptr = 1, numptr {
    	    call tbcinf (colptr[iptr], colnum, Memc[colname], Memc[colunits],
			 Memc[colfmt], datatype[1], lendata[1], lenfmt[1])
	    call tbcdef (otp, ocp, Memc[colname], Memc[colunits], Memc[colfmt],
			 datatype[1], lendata[1], 1)
	    Memi[newcol+iptr-1] = ocp    
	}

	# Copy the table columns a row at a time

	call tbtcre (otp)
	call tbhcal (itp, otp)

	irow = 1
	jrow = 1
	nrow = tbpsta (itp, TBL_NROWS)

	while (irow <= nrow) {
	    call tbrcsc (itp, otp, colptr, Memi[newcol], irow, jrow, numptr)

	    if (uniq) {
		call nextuniq (itp, numptr, colptr, irow)
	    } else {
		irow = irow + 1
	    }

	    jrow = jrow + 1
	}

	call sfree (sp)
end
