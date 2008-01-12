include <tbset.h>

# tu_getput -- do the interpolation
# This routine reads the independent and dependent variable values,
# does the interpolation, and writes the results to the output table.
#
# Phil Hodge, 24-April-2000  subroutine created

procedure tu_getput (itp, otp, iv_icp, iv_ocp,
		icp, ocp, ncols, 
		xout, outrows, iv_step,
		i_func, extrapolate, ext_value, padvalue, array, verbose)

pointer itp, otp		# i: descr of input & output tables
pointer iv_icp			# i: column descr for input indep var column
pointer iv_ocp			# i: column descr for output indep var column
pointer icp[ncols]		# i: column descriptors for input table
pointer ocp[ncols]		# i: column descriptors for output table
int	ncols			# i: number of columns to copy
double	xout[ARB]		# i: output indep var values
int	outrows			# i: size of xout
double	iv_step			# i: increment in independent variable
int	i_func			# i: interpolation function code
bool	extrapolate		# i: true means don't use ext_value
double	ext_value		# i: value to assign to extrapolated points
double	padvalue		# i: value at end of input indep. var. to ignore
bool	array			# i: true if indep var column contains arrays
bool	verbose			# i: print info?
#--
pointer sp
pointer xin		# scratch for input indep var values
pointer yin		# scratch for input data values
pointer y2		# scratch for second derivatives (spline only)
pointer xa		# scratch for non-indef indep var values
pointer ya		# scratch for non-indef dep var values
pointer yout		# array of interpolated values
double	dbuf		# one interpolated value
int	inrows		# number of rows in input table
int	xnelem		# number of elements in xin, INDEF & padvalue trimmed
int	nelem		# allocated number of elements in array
int	nvals		# number of array elements actually gotten
int	n		# size of xa, ya, y2 (counting only non-INDEF values)
int	row, col	# loop indices
int	i
int	tbpsta(), tbcigi(), tbagtd()
errchk	tugcol, tbagtd, tbaptd, tbegtd, tbeptd, tbrcsc

begin
	call smark (sp)

	inrows = tbpsta (itp, TBL_NROWS)

	if (array)
	    nelem = tbcigi (iv_icp, TBL_COL_LENDATA)
	else
	    nelem = inrows
	call salloc (xin, nelem, TY_DOUBLE)
	call salloc (yin, nelem, TY_DOUBLE)
	call salloc (y2, nelem, TY_DOUBLE)
	call salloc (xa, nelem, TY_DOUBLE)
	call salloc (ya, nelem, TY_DOUBLE)

	if (array) {

	    call salloc (yout, outrows, TY_DOUBLE)

	    # for each row in the input table ...
	    do row = 1, inrows {

		# Get input independent variable array from current row.
		call tugcol (itp, iv_icp, row, Memd[xin], xnelem,
			padvalue, array)

		# Put output indep var values into current row.
		call tbaptd (otp, iv_ocp, row, xout, 1, outrows)

		# for each column to be interpolated ...
		do col = 1, ncols {
		    nelem = tbcigi (icp[col], TBL_COL_LENDATA)
		    if (nelem == 1) {
			# just copy scalar column
			call tbrcsc (itp, otp, icp[col], ocp[col], row, row, 1)
		    } else {
			nvals = tbagtd (itp, icp[col], row, Memd[yin], 1, nelem)
			call tuifit (i_func, Memd[xin], Memd[yin], xnelem,
				Memd[xa], Memd[ya], Memd[y2], n)
			if (n > 0) {
			    do i = 1, outrows {
				# interpolate
				call tuival (i_func,
					Memd[xa], Memd[ya], Memd[y2], n,
					extrapolate, ext_value, xout,
					i, outrows, iv_step, Memd[yout+i-1])
			    }
			    # put the result
			    call tbaptd (otp, ocp[col], row,
					Memd[yout], 1, outrows)
			} else if (verbose && row == 1) {
			    call printf ("not enough data for interpolation\n")
			    call flush (STDOUT)
			}
		    }
		}
	    }

	} else {

	    # Get input independent variable column.
	    row = 1
	    call tugcol (itp, iv_icp, row, Memd[xin], xnelem, padvalue, array)

	    # Put output independent variable values into output table column.
	    do row = 1, outrows
		call tbeptd (otp, iv_ocp, row, xout[row])

	    # for each column to be interpolated ...
	    do col = 1, ncols {

		do row = 1, inrows
		    call tbegtd (itp, icp[col], row, Memd[yin+row-1])

		# xnelem will be less than or equal to inrows.
		call tuifit (i_func, Memd[xin], Memd[yin], xnelem,
			Memd[xa], Memd[ya], Memd[y2], n)

		if (n > 0) {
		    do row = 1, outrows {
			# interpolate, and put the result
			call tuival (i_func, Memd[xa], Memd[ya], Memd[y2], n,
				extrapolate, ext_value,
				xout, row, outrows, iv_step, dbuf)
			call tbeptd (otp, ocp[col], row, dbuf)
		    }
		} else if (verbose) {
		    call printf ("not enough data for interpolation\n")
		    call flush (STDOUT)
		}
	    }
	}

	call sfree (sp)
end
