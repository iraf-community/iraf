include	<tbset.h>

# MKJOIN -- Create a table that will hold the join of two other tables
#
# B.Simon	04-Nov-87	First Code
# B.Simon	31-Mar-92	Set output table type from input tables
# B.Simon	14-Apr-99	Extracted code that creates table

pointer procedure mkjoin (tol, tp1, cp1, tp2, cp2, outtable, otp,
			  cpvec1, cpvec2, cpveco, ncol1, ncol2)

double	tol		# i: Tolerance used in testing for equality
pointer	tp1		# i: Table descriptor of first table
pointer	cp1		# i: Descriptor of merged column in first table
pointer	tp2		# i: Table descriptor of second table
pointer	cp2		# i: Descriptor of merged column in second table
char	outtable[ARB]	# i: Name of output table
pointer	otp		# i: Table descriptor of output table
pointer	cpvec1[ARB]	# i: Vector of columns in first input table
pointer	cpvec2[ARB]	# i: Vector of columns in second input table
pointer cpveco[ARB]	# i: Vector of columns in output table
int	ncol1		# i: Number of columns in first input table
int	ncol2		# u: Number of columns in second input table
#--
int	icol, jcol, numcol, type1, type2
int	colnum[1], datatype[1], lendata[1], lenfmt[1]
pointer	sp, icp, ocp, oldcol, newcol
pointer	colname, colunits, colfmt

int	tbpsta(), tbcnum(), tbcigi()
pointer	tbtopn()

begin
	# Set up arrays in dynamic memory

	call smark (sp)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (colunits, SZ_COLUNITS, TY_CHAR)
	call salloc (colfmt, SZ_COLFMT, TY_CHAR)

	# Copy column pointers to old column array. If the tolerance is
	# zero, the join column in the second table is not copied

	numcol = ncol1 + ncol2

	do icol = 1, ncol1
	    cpvec1[icol] = tbcnum (tp1, icol)

	do icol = 1, ncol2
	    cpvec2[icol] = tbcnum (tp2, icol)

	if (tol == 0.0 && cp1 != NULL && cp2 != NULL) {
	    jcol = tbcigi (cp2, TBL_COL_NUMBER)
	    ncol2 = ncol2 - 1
	    numcol = numcol - 1
	    do icol = jcol+1, ncol2
		cpvec2[icol-1] = cpvec2[icol]
	}

	# Set type of output table

 	otp = tbtopn (outtable, NEW_FILE, NULL)

	type1 = tbpsta (tp1, TBL_WHTYPE)
	type2 = tbpsta (tp2, TBL_WHTYPE)
	if (type1 == type2)
	    call tbpset (otp, TBL_WHTYPE, type1)

	# Copy column information from the input tables to the output table

 	do icol = 1, ncol1 {
	    icp = cpvec1[icol]
    	    call tbcinf (icp, colnum, Memc[colname], Memc[colunits],
			 Memc[colfmt], datatype[1], lendata[1], lenfmt[1])

	    call newcolnam (numcol, Memi[oldcol], icol,
			    Memc[colname], SZ_COLNAME)

	    call tbcdef (otp, ocp, Memc[colname], Memc[colunits], Memc[colfmt],
			 datatype[1], lendata[1], 1)
	    cpveco[icol] = ocp    
	}

 	do icol = 1, ncol2 {
	    icp = cpvec2[icol]
    	    call tbcinf (icp, colnum, Memc[colname], Memc[colunits],
			 Memc[colfmt], datatype[1], lendata[1], lenfmt[1])
	    call newcolnam (numcol, Memi[oldcol], icol,
			    Memc[colname], SZ_COLNAME)
	    call tbcdef (otp, ocp, Memc[colname], Memc[colunits], Memc[colfmt],
			 datatype[1], lendata[1], 1)
	    cpveco[ncol1+icol] = ocp    
	}

	# Copy the table columns a row at a time

	call tbtcre (otp)
	call tbhcal (tp2, otp)
	call tbhcal (tp1, otp)

	call mfree (oldcol, TY_INT)
	call mfree (newcol, TY_INT)
	call sfree (sp)

	return (otp)
end
