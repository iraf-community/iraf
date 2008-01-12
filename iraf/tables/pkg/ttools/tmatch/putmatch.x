include	<tbset.h>

#* HISTORY *
# B.Simon	25-Aug-94	Original

# PUTMATCH -- Write matched rows in input as a single row in output table

procedure putmatch (output, incol1, incol2, in1, in2, nclosest, closest)

char	output[ARB]	# i: output table name
char	incol1[ARB]	# i: list of columns to copy from first table
char	incol2[ARB]	# i: list of columns to copy from second table
pointer	in1		# i: first table's descriptor
pointer	in2		# i: second table's descriptor
int	nclosest	# i: length of closest array
int	closest[ARB]	# i: indices of rows in second table closest to first
#--
int	mxcol1, mxcol2, maxcol, ncol1, ncol2, ncol, type1, type2
pointer	colnum, datatype, lendata, lenfmt, icol, irow, jrow
pointer	sp, colname, colunits, colfmt, oldcol, newcol,out

string	nomatch "WARNING: No rows matched between tables, output \
table is empty\n"

int	tbpsta()
pointer	tbtopn()

begin
	# Allocate memory for temporary strings

	call smark (sp)
        call salloc (colname, SZ_COLNAME, TY_CHAR)
        call salloc (colunits, SZ_COLUNITS, TY_CHAR)
        call salloc (colfmt, SZ_COLFMT, TY_CHAR)

	# Get column descriptors from input tables

	mxcol1 = tbpsta (in1, TBL_NCOLS)
	mxcol2 = tbpsta (in2, TBL_NCOLS)
	maxcol = mxcol1 + mxcol2

	call salloc (oldcol, maxcol, TY_INT)
	call salloc (newcol, maxcol, TY_INT)

	call tctexp (in1, incol1, mxcol1, ncol1, Memi[oldcol])
	call tctexp (in2, incol2, mxcol2, ncol2, Memi[oldcol+ncol1])
	ncol = ncol1 + ncol2

	# Create output table

	out = tbtopn (output, NEW_FILE, NULL)

	# Set type (text, row ordered, column ordered)

	type1 = tbpsta (in1, TBL_WHTYPE)
	type2 = tbpsta (in2, TBL_WHTYPE)
	if (type1 == type2)
	    call tbpset (out, TBL_WHTYPE, type1)

	# Create columns in output table

	do icol = 1, ncol {
	    call tbcinf (Memi[oldcol+icol-1], colnum, Memc[colname], 
			 Memc[colunits], Memc[colfmt], datatype, 
			 lendata, lenfmt)

	    call newcolnam (ncol, Memi[oldcol], icol, 
			    Memc[colname], SZ_COLNAME)

	    call tbcdef (out, Memi[newcol+icol-1], Memc[colname], 
			 Memc[colunits], Memc[colfmt], datatype, lendata, 1)
	}

	# Copy header keywords from first input table

	call tbtcre (out)
	call tbhcal (in1, out)

	# Copy rows from input table to output

	jrow = 0
	do irow = 1, nclosest {
	    if (closest[irow] == 0)
		next

	    jrow = jrow + 1
	    call tbrcsc (in1, out, Memi[oldcol], Memi[newcol], 
			 irow, jrow, ncol1)
	    call tbrcsc (in2, out, Memi[oldcol+ncol1], Memi[newcol+ncol1],
			 closest[irow], jrow, ncol2)
	}

	# Write warning message if no rows matched

	if (jrow == 0)
	    call eprintf (nomatch)

	# Clean up

	call tbtclo (out)
	call sfree (sp)
end
