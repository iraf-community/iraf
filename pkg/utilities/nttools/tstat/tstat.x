include <error.h>
include <fset.h>		# to check whether input or output is redirected
include <tbset.h>

define	MAX_RANGES (SZ_LINE/2)	# max number of ranges of row numbers
define	NUM_COL		8	# number of output columns

# tstat -- get statistics for a table column
# This task gets the mean, standard deviation, median, and minimum &
# maximum values for a table column.
#
# Phil Hodge,  8-Dec-1988  Task created.
# Phil Hodge, 14-Mar-1989  Also compute the median; fix bug in std dev.
# Phil Hodge, 27-May-1992  Print INDEF if no values in range.
# Phil Hodge, 31-Jul-1992  Print column name after table name.
# Phil Hodge, 11-Jan-1992  Use asrtd instead of asokd for median if nr is even.
# Phil Hodge,  3-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge, 25-Apr-1997  Use asrtd instead of asokd regardless of nr.
# Phil Hodge, 26-Mar-1998  Get all elements of array columns; ignore the
#			column parameter if an input table has only one column.
# Phil Hodge,  8-Jun-1999  Set input/output to STDIN/STDOUT if redirected.
# Phil Hodge,  2-Jan-2001  If the input was redirected, and one command-line
#			argument was specified, take that argument to be the
#			column name rather than the table name.

procedure tstat()

pointer inlist			# scratch for list of input table names
char	cl_colname[SZ_COLNAME]	# column name gotten from cl parameter
double	lowlim			# lower & upper limits for histogram
double	highlim			# lower & upper limits for histogram
pointer range_string		# string which gives ranges of row numbers
char	nam_table[SZ_FNAME]	# column name for table name
char	nam_name[SZ_COLNAME]	# column name for column name
char	nam_mean[SZ_COLNAME]	# column name for mean
char	nam_stddev[SZ_COLNAME]	# column name for standard deviation
char	nam_med[SZ_COLNAME]	# column name for median
char	nam_min[SZ_COLNAME]	# column name for minimum
char	nam_max[SZ_COLNAME]	# column name for maximum
char	nam_nrows[SZ_COLNAME]	# column name for number of good rows
#--
pointer list1			# for list of input tables
pointer sp
pointer itp, otp		# ptr to table descriptor
pointer cptr			# ptr to column descriptor
pointer ocp[NUM_COL]		# ptrs to col descriptors for output columns
pointer intab, outtab		# scr for names of input & output tables
char	colname[SZ_COLNAME]	# column name
double	vmean			# mean value
double	vstddev			# standard deviation of values
double	vmedian			# median value
double	vmin, vmax		# minimum & maximum values
int	row			# output row number
int	nrows			# number of rows included and not INDEF
int	ntables			# number of tables in input list
bool	listout			# ASCII output?
bool	tabout			# table output?
bool	new_table		# true if output table does not already exist
int	nargs			# number of command-line arguments
bool	in_redir		# is input redirected?
int	clgeti()
double	clgetd()
int	fstati()
pointer tbnopen()
int	tbnget(), tbnlen()
pointer tbtopn()
pointer tbcnum()
int	tbtacc(), tbpsta()
bool	streq()

begin
	# Allocate scratch for lists of names and for table names.
	call smark (sp)
	call salloc (inlist, SZ_LINE, TY_CHAR)
	call salloc (intab, SZ_LINE, TY_CHAR)
	call salloc (outtab, SZ_LINE, TY_CHAR)
	call salloc (range_string, SZ_LINE, TY_CHAR)

	# Get task parameters.

	nargs = clgeti ("$nargs")
	in_redir = fstati (STDIN, F_REDIR) == YES

	if (in_redir)
	    call strcpy ("STDIN", Memc[inlist], SZ_LINE)
	else
	    call clgstr ("intable", Memc[inlist], SZ_LINE)

	if (fstati (STDOUT, F_REDIR) == YES)
	    call strcpy ("STDOUT", Memc[outtab], SZ_LINE)
	else
	    call clgstr ("outtable", Memc[outtab], SZ_LINE)

	cl_colname[1] = EOS			# initial value
	lowlim = clgetd ("lowlim")		# these limits may be INDEF
	highlim = clgetd ("highlim")
	call clgstr ("rows", Memc[range_string], SZ_LINE)

	# ASCII output?  table output?  create a new output table?
	listout = streq (Memc[outtab], "STDOUT")
	if ( listout || (Memc[outtab] == EOS) ) {
	    tabout = false
	} else {
	    tabout = true
	    new_table = (tbtacc (Memc[outtab]) == NO)
	}

	if (tabout) {
	    call clgstr ("n_tab", nam_table, SZ_FNAME)
	    call clgstr ("n_nam", nam_name, SZ_COLNAME)
	    call clgstr ("n_nrows", nam_nrows, SZ_COLNAME)
	    call clgstr ("n_mean", nam_mean, SZ_COLNAME)
	    call clgstr ("n_stddev", nam_stddev, SZ_COLNAME)
	    call clgstr ("n_median", nam_med, SZ_COLNAME)
	    call clgstr ("n_min", nam_min, SZ_COLNAME)
	    call clgstr ("n_max", nam_max, SZ_COLNAME)
	}

	# Expand the input table list.
	list1 = tbnopen (Memc[inlist])

	ntables = tbnlen (list1)

	if (listout) {
	    row = 0			# just to have a definite value
	    if (ntables > 1) {
		call printf ("# nrows")
		call printf ("            mean        stddev        median")
		call printf ("           min           max\n\n")
	    }
	} else if (tabout) {
	    # Create output table (or open existing table) & define columns.
	    if (new_table)
		otp = tbtopn (Memc[outtab], NEW_FILE, NULL)
	    else
		otp = tbtopn (Memc[outtab], READ_WRITE, NULL)

	    call tbcfnd (otp, nam_table,  ocp[1], 1)
	    call tbcfnd (otp, nam_name,   ocp[2], 1)
	    call tbcfnd (otp, nam_nrows,  ocp[3], 1)
	    call tbcfnd (otp, nam_mean,   ocp[4], 1)
	    call tbcfnd (otp, nam_stddev, ocp[5], 1)
	    call tbcfnd (otp, nam_med,    ocp[6], 1)
	    call tbcfnd (otp, nam_min,    ocp[7], 1)
	    call tbcfnd (otp, nam_max,    ocp[8], 1)

	    if (ocp[1] == NULL)
		call tbcdef (otp, ocp[1],
			nam_table, "", "", -SZ_FNAME, 1, 1)
	    if (ocp[2] == NULL)
		call tbcdef (otp, ocp[2],
			nam_name, "", "", -SZ_COLNAME, 1, 1)
	    if (ocp[3] == NULL)
		call tbcdef (otp, ocp[3],
			nam_nrows, "", "", TY_INT, 1, 1)
	    if (ocp[4] == NULL)
		call tbcdef (otp, ocp[4],
			nam_mean, "", "", TY_DOUBLE, 1, 1)
	    if (ocp[5] == NULL)
		call tbcdef (otp, ocp[5],
			nam_stddev, "", "", TY_DOUBLE, 1, 1)
	    if (ocp[6] == NULL)
		call tbcdef (otp, ocp[6],
			nam_med, "", "", TY_DOUBLE, 1, 1)
	    if (ocp[7] == NULL)
		call tbcdef (otp, ocp[7],
			nam_min, "", "", TY_DOUBLE, 1, 1)
	    if (ocp[8] == NULL)
		call tbcdef (otp, ocp[8],
			nam_max, "", "", TY_DOUBLE, 1, 1)
	    if (new_table)
		call tbtcre (otp)

	    row = tbpsta (otp, TBL_NROWS)	# append to whatever is there
	}

	# Do for each input table.
	while (tbnget (list1, Memc[intab], SZ_LINE) != EOF) {

	    itp = tbtopn (Memc[intab], READ_ONLY, NULL)
	    if (tbpsta (itp, TBL_NCOLS) == 1) {
		cptr = tbcnum (itp, 1)
		call tbcigt (cptr, TBL_COL_NAME, colname, SZ_COLNAME)
	    } else {
		if (cl_colname[1] == EOS) {
		    if (nargs == 1 && in_redir) {
			# in this case, the one argument should be column name
			call clgstr ("intable", cl_colname, SZ_COLNAME)
			# update par file
			call clpstr ("intable", "STDIN")
			call clpstr ("column", cl_colname)
		    } else {
			call clgstr ("column", cl_colname, SZ_COLNAME)
		    }
		}
		call strcpy (cl_colname, colname, SZ_COLNAME)
		call tbcfnd (itp, colname, cptr, 1)
	    }
	    if (cptr == NULL) {
		call tbtclo (itp)
		call eprintf ("column %s not found in %s\n")
		    call pargstr (colname)
		    call pargstr (Memc[intab])
		next
	    }
	    row = row + 1

	    if (listout) {
		call printf ("# %s  %s\n")
		    call pargstr (Memc[intab])
		    call pargstr (colname)
		if (ntables == 1) {
		    call printf ("# nrows")
		    call printf ("            mean        stddev        median")
		    call printf ("           min           max\n")
		}
	    }

	    # Get statistics.
	    iferr {
		call ts_values (itp, cptr, Memc[range_string], lowlim, highlim,
			vmean, vstddev, vmedian, vmin, vmax, nrows)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    if (listout) {

		call printf ("%5d %17.10g %13.6g %13.6g %13.6g %13.6g\n")
		    call pargi (nrows)
		    call pargd (vmean)
		    call pargd (vstddev)
		    call pargd (vmedian)
		    call pargd (vmin)
		    call pargd (vmax)

	    } else if (tabout) {

		# Write the values into the output table.
		call tbeptt (otp, ocp[1], row, Memc[intab])
		call tbeptt (otp, ocp[2], row, colname)
		call tbepti (otp, ocp[3], row, nrows)
		call tbeptd (otp, ocp[4], row, vmean)
		call tbeptd (otp, ocp[5], row, vstddev)
		call tbeptd (otp, ocp[6], row, vmedian)
		call tbeptd (otp, ocp[7], row, vmin)
		call tbeptd (otp, ocp[8], row, vmax)
	    }

	    call tbtclo (itp)		# close current input table
	}

	# Close the output table.  It may be that nothing was written to it.
	if (tabout)
	    call tbtclo (otp)

	# Save the results (from the last input table) in cl parameters.
	call clputi ("nrows", nrows)
	call clputd ("mean", vmean)
	call clputd ("stddev", vstddev)
	call clputd ("median", vmedian)
	call clputd ("vmin", vmin)
	call clputd ("vmax", vmax)

	call tbnclose (list1)
	call sfree (sp)
end

# ts_values -- get statistics for a table column
# This routine gets the mean, standard deviation, minimum and maximum
# values for a table column.  If lower and/or upper cutoff limits were
# specified (i.e. are not INDEF) then values outside that range will not
# be included in the statistics.  INDEF values are also not included.

procedure ts_values (tp, cptr, range_str, lowlim, highlim,
		vmean, vstddev, vmedian, vmin, vmax, nrows)

pointer tp		# i: ptr to table descriptor
pointer cptr		# i: ptr to column descriptor
char	range_str[ARB]	# i: range of row numbers
double	lowlim		# i: lower cutoff of values to be included
double	highlim		# i: upper cutoff of values to be included
double	vmean		# o: mean value
double	vstddev		# o: standard deviation of values
double	vmedian		# o: median value
double	vmin		# o: minimum value
double	vmax		# o: maximum value
int	nrows		# o: number of rows included in the statistics
#--
pointer sp
pointer val			# scratch for array of values (for median)
pointer descrip			# column selector descriptor
double	value			# an element gotten from the table
double	sum, sumsq		# for accumulating sums
double	smin, smax		# temp min & max
double	diff			# current value minus first good value
int	all_elem		# total number of elements in column
int	selrows			# number of selected rows
int	nelem			# number of elements in one cell
int	nret			# number returned (ignored)
int	nr			# current number of rows
int	row			# row number
int	ranges[3,MAX_RANGES]	# ranges of row numbers
int	nvalues			# returned by decode_ranges and ignored
int	stat			# returned by get_next_number
int	i, j			# loop indexes
bool	chklow, chkhi		# were low (high) limits specified?
bool	val_ok			# is current value within limits?
bool	done			# loop-termination flag
int	decode_ranges(), get_next_number()
int	tbagtd()
errchk	tbagtd, tbegtd, tcs_rdaryd

begin
	if (decode_ranges (range_str, ranges, MAX_RANGES, nvalues) != OK) {
	    call eprintf ("rows = `%s'\n")
		call pargstr (range_str)
	    call error (1, "Range of row numbers is invalid.")
	}

	# Get the number of elements per table cell and the number of
	# rows selected using row-selector syntax (as opposed to using
	# the 'rows' task parameter).
	call tbcnel1 (tp, cptr, descrip, nelem, selrows)

	# Find out how many rows there are in the table, restricted by
	# either a row selector or the 'rows' task parameter, or both.
	if (range_str[1] == '-' || range_str[1] == EOS) {
	    all_elem = selrows * nelem
	} else {
	    # Count the number of rows specified.
	    i = 0				# count of row numbers
	    row = 0				# initialize get_next_number
	    done = false
	    while (!done) {
		stat = get_next_number (ranges, row)
		if (stat == EOF || row > selrows)
		    done = true
		else
		    i = i + 1
	    }
	    all_elem = i * nelem
	}

	# Allocate scratch space to hold an entire column.
	call smark (sp)
	call salloc (val, all_elem, TY_DOUBLE)

	row = 0					# reinitialize get_next_number
	stat = get_next_number (ranges, row)
	done = (stat == EOF || row > selrows)

	# Get the data.
	i = 1
	while (!done) {

	    if (descrip == NULL) {
		if (nelem == 1)
		    call tbegtd (tp, cptr, row, Memd[val+i-1])
		else
		    nret = tbagtd (tp, cptr, row, Memd[val+i-1], 1, nelem)
	    } else {
		call tcs_rdaryd (tp, descrip, row, all_elem-i+1,
			nret, Memd[val+i-1])
	    }

	    i = i + nelem

	    stat = get_next_number (ranges, row)
	    done = (stat == EOF || row > selrows)
	}
	if (all_elem != i - 1)
	    call error (1, "not all elements read from column")

	chklow = ! IS_INDEFD(lowlim)
	chkhi = ! IS_INDEFD(highlim)

	# Check which values to include.
	sum = 0.d0
	nr = 0
	do i = 0, all_elem-1 {			# zero indexed
	    value = Memd[val+i]
	    if (!IS_INDEFD(value)) {
		val_ok = true			# an initial value
		if (chkhi && (value > highlim)) {
		    val_ok = false
		    Memd[val+i] = INDEFD
		}
		if (chklow && (value < lowlim)) {
		    val_ok = false
		    Memd[val+i] = INDEFD
		}
		if (val_ok) {
		    nr = nr + 1
		    sum = sum + value
		}
	    }
	}

	if (nr < 1) {
	    # No rows with valid data, so set the output to INDEF.
	    nrows = 0
	    vmean = INDEFD
	    vstddev = INDEFD
	    vmedian = INDEFD
	    vmin = INDEFD
	    vmax = INDEFD
	    return
	} else {
	    # Assign two of the output values.
	    nrows = nr
	    vmean = sum / double(nr)
	}

	# Move good data to the beginning of the array.
	if (nr < all_elem) {
	    j = 0
	    do i = 0, all_elem-1 {		# zero indexed
		if (!IS_INDEFD(Memd[val+i])) {
		    Memd[val+j] = Memd[val+i]
		    j = j + 1
		}
	    }
	}

	# Find the min, max, standard deviation of the good values.
	sumsq = 0.d0
	smin = Memd[val]
	smax = Memd[val]
	do i = 0, nr-1 {			# zero indexed
	    value = Memd[val+i]
	    diff = value - vmean
	    sumsq = sumsq + diff * diff
	    if (value < smin)
		smin = value
	    if (value > smax)
		smax = value
	}

	# Assign output values.

	vmin = smin
	vmax = smax

	if (nr > 1)
	    vstddev = sqrt (sumsq / double(nr-1))
	else
	    vstddev = INDEFD

	# Determine the median.  If there are an even number of values, the
	# middle two are averaged.
	if (nr < 3) {
	    vmedian = vmean
	} else {
	    call asrtd (Memd[val], Memd[val], nr)
	    if (nr / 2 * 2 == nr) {		# even number of values?
		vmedian = (Memd[val+nr/2-1] + Memd[val+nr/2]) / 2.d0
	    } else {
		vmedian = Memd[val+nr/2]
	    }
	}

	call sfree (sp)
end
