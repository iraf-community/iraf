include <tbset.h>

# tudcol -- get column pointers
# Get pointers to input and output dependent variable columns, define
# columns in output table.
# The arrays icp & ocp of column pointers are of the same length,
# which will be less than the total number of columns because they
# do not include the independent variable column.
#
# Columns of type text or boolean will not be copied to the output table.
# If the independent variable column contains arrays, scalar columns will
# be copied to output without interpolation; array columns must be the same
# length as the independent variable column.
# If the independent variable column contains scalars, array columns will
# not be copied to output.
# If verbose is true, a message will be printed regarding skipped columns.
#
# Phil Hodge, 26-Apr-2000  Subroutine created, based on previous tugcol.

procedure tudcol (itp, otp, iv_colname, outrows,
		iv_icp, iv_ocp, icp, ocp, ncols, array, verbose)

pointer itp			# i: pointer to input table descriptor
pointer otp			# i: pointer to output table descriptor
char	iv_colname[ARB]		# i: name of indep var column
int	outrows			# i: array length for output array columns
pointer iv_icp			# o: ptr to descr for input indep var column
pointer iv_ocp			# o: ptr to descr for output indep var column
pointer icp[ARB]		# o: ptr to column descr for input table
pointer ocp[ARB]		# o: ptr to column descr for output table
int	ncols			# o: number of dependent variable columns
bool	array			# o: true if indep var column contains arrays
bool	verbose			# i: print info?
#--
pointer sp
pointer why			# note regarding why a column is skipped
bool	skip_this		# true if column will not be copied to output
pointer cp			# a column pointer
char	cname[SZ_COLNAME]	# a column name
char	cunits[SZ_COLUNITS]	# units for a column
char	cfmt[SZ_COLFMT]		# print format for a column
int	dtype			# data type of a column
int	xnelem			# number of input elements for indep var column
int	iv_nelem		# number of output elements for indep var column
int	nelem			# number of elements
int	lenfmt			# length of print format
int	incols			# number of columns in input table
int	k			# loop index
int	cnum			# column number (ignored)
pointer tbcnum()
int	tbpsta()

begin
	call smark (sp)
	call salloc (why, SZ_FNAME, TY_CHAR)

	incols = tbpsta (itp, TBL_NCOLS)

	call tbcfnd1 (itp, iv_colname, iv_icp)
	if (iv_icp == NULL)
	    call error (1, "independent variable column not found")

	# Get info about indep var column in input table.
	call tbcinf (iv_icp, cnum, cname, cunits, cfmt, dtype, xnelem, lenfmt)

	# Note that this test is based on the independent variable column.
	array = (xnelem > 1)

	# The indep var column in the output table may contain arrays;
	# iv_nelem will be used in the loop below when defining the output
	# column of independent variable values.
	if (array)
	    iv_nelem = outrows
	else
	    iv_nelem = 1

	if (verbose && array) {
	    call printf ("note:  array columns in each row will be rebinned\n")
	    call flush (STDOUT)
	}

	# Define the columns in the output table.
	ncols = 0
	do k = 1, incols {

	    skip_this = false		# initial value
	    cp = tbcnum (itp, k)

	    call tbcinf (cp, cnum, cname, cunits, cfmt, dtype, nelem, lenfmt)

	    # Indep var column.
	    if (cp == iv_icp) {
		call tbcdef1 (otp, iv_ocp, cname, cunits, cfmt, dtype, iv_nelem)
		next
	    }

	    if (array) {
		if (nelem > 1 && nelem != xnelem) {	# not the same size
		    skip_this = true
		    call strcpy ("array size is not the same",
				Memc[why], SZ_FNAME)
		}
	    } else {
		if (nelem > 1) {			# skip array columns
		    skip_this = true
		    call strcpy ("column contains arrays", Memc[why], SZ_FNAME)
		}
		if (dtype <= 0 || dtype == TY_CHAR || dtype == TY_BOOL) {
		    skip_this = true
		    if (dtype == TY_BOOL) {
			call strcpy ("data type is boolean",
				Memc[why], SZ_FNAME)
		    } else {
			call strcpy ("data type is text string",
				Memc[why], SZ_FNAME)
		    }
		}
	    }

	    if (skip_this) {
		if (verbose) {
		    call printf ("  skipping column `%s' (%s)\n")
			call pargstr (cname)
			call pargstr (Memc[why])
		    call flush (STDOUT)
		}
		next
	    }

	    # Define output column; save pointers for input & output.
	    ncols = ncols + 1
	    if (array && nelem > 1)
		nelem = outrows
	    icp[ncols] = cp
	    call tbcdef1 (otp, ocp[ncols],
				cname, cunits, cfmt, dtype, nelem)
	}

	call sfree (sp)
end
