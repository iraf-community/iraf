include	<tbset.h>

# WQUERY -- Copy selected columns and rows to output table
#
# B.Simon	19-Oct-87	First Code

procedure wquery (itp, otp, numcol, colptr, nindex, index)

pointer	itp		# i: Input table descriptor
pointer	otp		# i: Output table descriptor
int	numcol		# i: Number of column pointers
pointer	colptr[ARB]	# i: Array of column pointers
int	nindex		# i: Size of index array
int	index[ARB]	# i: Array of row indices
#--
int	iptr, idx, jdx
int	colnum[1], datatype[1], lendata[1], lenfmt[1]
pointer	sp, ocp, newcol, colname, colunits, colfmt

begin
	# Set up arrays in dynamic memory

	call smark (sp)
	call salloc (newcol, numcol, TY_INT)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (colunits, SZ_COLUNITS, TY_CHAR)
	call salloc (colfmt, SZ_COLFMT, TY_CHAR)


	# Copy column information from the input table to the output table

	do iptr = 1, numcol {
    	    call tbcinf (colptr[iptr], colnum, Memc[colname], Memc[colunits],
			 Memc[colfmt], datatype[1], lendata[1], lenfmt[1])
	    call tbcdef (otp, ocp, Memc[colname], Memc[colunits], Memc[colfmt],
			 datatype[1], lendata[1], 1)
	    Memi[newcol+iptr-1] = ocp    
	}

	# Copy the table columns a row at a time

	call tbtcre (otp)
	call tbhcal (itp, otp)
	do idx = 1, nindex {
	    jdx = index[idx]
	    call tbrcsc (itp, otp, colptr, Memi[newcol], jdx, idx, numcol)
	}

	call sfree (sp)
end
