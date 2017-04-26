include <tbset.h>

# TPRODUCT -- Form the cartesian product of two tables
#
# B.Simon	05-Nov-1987	First Code
# B.Simon	31-Mar-1992	Set output table type from input tables
# Phil Hodge	 8-Apr-1999	Call tbfpri.

procedure t_product()

pointer	intable1		# Names of the first table to be joined
pointer	intable2		# Names of the second table to be joined
pointer	outtable		# Name of output table
#--
int	idx, jdx, kdx, icol, ncol1, ncol2, nrow1, nrow2, numcol, type1, type2
int	phu_copied		# set by tbfpri and ignored
int	colnum[1], datatype[1], lendata[1], lenfmt[1]
pointer	sp, tp1, tp2, otp, icp, ocp, oldcol, newcol
pointer	colname, colunits, colfmt

int	tbpsta(), tbcnum()
pointer	tbtopn()

begin
	# Allocate stack memory for strings

	call smark (sp)
	call salloc (intable1, SZ_FNAME, TY_CHAR)
	call salloc (intable2, SZ_FNAME, TY_CHAR)
	call salloc (outtable, SZ_FNAME, TY_CHAR)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (colunits, SZ_COLUNITS, TY_CHAR)
	call salloc (colfmt, SZ_COLFMT, TY_CHAR)

	# Read the task parameters

	call clgstr ("intable1", Memc[intable1], SZ_FNAME)
	call clgstr ("intable2", Memc[intable2], SZ_FNAME)
	call clgstr ("outtable", Memc[outtable], SZ_FNAME)

	# Open the tables

	tp1 = tbtopn (Memc[intable1], READ_ONLY, NULL)
	tp2 = tbtopn (Memc[intable2], READ_ONLY, NULL)
	call tbfpri (Memc[intable1], Memc[outtable], phu_copied)
 	otp = tbtopn (Memc[outtable], NEW_FILE, NULL)

	# Set type of output table

	type1 = tbpsta (tp1, TBL_WHTYPE)
	type2 = tbpsta (tp2, TBL_WHTYPE)
	if (type1 == type2)
	    call tbpset (otp, TBL_WHTYPE, type1)

	# Get the number of columns and allocate arrays to hold column pointers

	ncol1 = tbpsta (tp1, TBL_NCOLS)
	ncol2 = tbpsta (tp2, TBL_NCOLS)
	nrow1 = tbpsta (tp1, TBL_NROWS)
	nrow2 = tbpsta (tp2, TBL_NROWS)

	numcol = ncol1 + ncol2
 	call malloc (oldcol, numcol, TY_INT)
 	call malloc (newcol, numcol, TY_INT)

	# Copy column pointers to old column array.

	do icol = 1, ncol1
	    Memi[oldcol+icol-1] = tbcnum (tp1, icol)

	do icol = 1, ncol2
	    Memi[oldcol+ncol1+icol-1] = tbcnum (tp2, icol)

	# Copy column information from the input tables to the output table

 	do icol = 1, numcol {
	    icp = Memi[oldcol+icol-1]
    	    call tbcinf (icp, colnum, Memc[colname], Memc[colunits],
			 Memc[colfmt], datatype[1], lendata[1], lenfmt[1])
	    call newcolnam (numcol, Memi[oldcol], icol,
			    Memc[colname], SZ_COLNAME)
	    call tbcdef (otp, ocp, Memc[colname], Memc[colunits], Memc[colfmt],
			 datatype[1], lendata[1], 1)
	    Memi[newcol+icol-1] = ocp    
	}

	# Copy the table columns a row at a time

	call tbtcre (otp)
	call tbhcal (tp2, otp)
	call tbhcal (tp1, otp)

	kdx = 0
	do idx = 1, nrow1 {
	    do jdx = 1, nrow2 {
		kdx = kdx + 1
		call tbrcsc (tp1, otp, Memi[oldcol], Memi[newcol],
			     idx, kdx, ncol1)
		call tbrcsc (tp2, otp, Memi[oldcol+ncol1], Memi[newcol+ncol1],
			     jdx, kdx, ncol2)
	    }
	}

	# Close the tables and free dynamic memory

	call tbtclo (tp1)
	call tbtclo (tp2)
	call tbtclo (otp)

	call mfree (oldcol, TY_INT)
	call mfree (newcol, TY_INT)

end
