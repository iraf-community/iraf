include <error.h>		# defines EA_WARN
include <fset.h>		# to check whether input or output is redirected
include <tbset.h>
include "thistogram.h"		# defines NPAR, etc.

define	MAX_RANGES (SZ_LINE/2)	# max number of ranges of row numbers

# thistogram -- make a histogram of a table column
#
# Phil Hodge,  2-Dec-1988  Task created.
# Phil Hodge, 12-Jan-1989  th_mk_hist:  ignore values that are out of range
# Phil Hodge, 17-Mar-1994  Include parameters dx, clow, chigh.
# Phil Hodge,  3-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge,  8-Apr-1999  Call tbfpri.
# Phil Hodge,  8-Jun-1999  Set input/output to STDIN/STDOUT if redirected.

procedure thistogram()

pointer inlist, outlist		# scr for input & output lists of names
char	colname[SZ_COLNAME]	# column name
int	t_nbins			# number of bins
double	t_vlow, t_vhigh		# lower & upper limits for histogram
double	t_dx			# bin width
double	t_clow, t_chigh		# centers of first and last bins
char	outcolx[SZ_COLNAME]	# column name for indep var for histogram
char	outcoly[SZ_COLNAME]	# column name for dependent var for histogram
#--
pointer sp
pointer itp, otp		# ptr to table descriptor
pointer cptr			# ptr to column descriptor
pointer ocpx, ocpy		# ptr to col descr for output columns
pointer intab, outtab		# scr for names of input & output tables
pointer range_string		# string which gives ranges of row numbers
pointer val, counts		# scr for histogram:  indep & dep var

# These six parameters are copied from t_... in each loop.
int	nbins
double	vlow, vhigh, dx, clow, chigh

pointer list1, list2
int	i, junk
int	nrows			# number of rows included and not INDEF
int	phu_copied		# set by tbfpri and ignored
bool	listout			# is the output ASCII rather than a table?
bool	got[NPAR]		# flags to specify what we have got
bool	find_datamin		# true if we need to find minimum data value
bool	find_datamax		# true if we need to find maximum data value
pointer tbtopn()
double	clgetd()
int	fstati()
pointer tbnopen()
int	clgeti(), tbnget(), tbnlen()
bool	streq()

begin
	# Allocate scratch for lists of names and for table names.
	call smark (sp)
	call salloc (inlist, SZ_FNAME, TY_CHAR)
	call salloc (outlist, SZ_FNAME, TY_CHAR)
	call salloc (intab, SZ_FNAME, TY_CHAR)
	call salloc (outtab, SZ_FNAME, TY_CHAR)
	call salloc (range_string, SZ_FNAME, TY_CHAR)

	# Get task parameters.

	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", Memc[inlist], SZ_FNAME)
	else
	    call clgstr ("intable", Memc[inlist], SZ_FNAME)

	if (fstati (STDOUT, F_REDIR) == YES)
	    call strcpy ("STDOUT", Memc[outlist], SZ_FNAME)
	else
	    call clgstr ("outtable", Memc[outlist], SZ_FNAME)

	call clgstr ("column", colname, SZ_COLNAME)

	# Some of these six parameters may be INDEF.  The "t_" prefix
	# means these are task parameters; they are assigned to variables
	# (same name but without the "t_") in the loop over tables, and
	# those variables may be modified within the loop.
	t_nbins = clgeti ("nbins")
	t_vlow = clgetd ("lowval")
	t_vhigh = clgetd ("highval")
	t_dx = clgetd ("dx")
	t_clow = clgetd ("clow")
	t_chigh = clgetd ("chigh")

	call clgstr ("rows", Memc[range_string], SZ_FNAME)

	listout = streq (Memc[outlist], "STDOUT")	# ASCII output?
	if ( ! listout ) {
	    call clgstr ("outcolx", outcolx, SZ_COLNAME)
	    call clgstr ("outcoly", outcoly, SZ_COLNAME)
	}

	if (!IS_INDEF(t_dx))
	    if (t_dx <= 0.d0)
		call error (1, "dx must not be less than or equal to zero")

	# These parameters are interdependent, so compute what was not
	# specified from those that were, as far as we can.
	call th_options (t_nbins, t_vlow, t_vhigh, t_dx, t_clow, t_chigh,
		got, find_datamin, find_datamax)

	# Expand the input table list.
	list1 = tbnopen (Memc[inlist])

	if ( ! listout ) {
	    # Expand the output table list.
	    list2 = tbnopen (Memc[outlist])
	    if (tbnlen (list1) != tbnlen (list2)) {
		call tbnclose (list1)
		call tbnclose (list2)
		call error (1,
			"Number of input and output tables not the same")
	    }
	}

	# Do for each input table.
	while (tbnget (list1, Memc[intab], SZ_FNAME) != EOF) {

	    # These may be modified within this loop by th_limits.
	    nbins = t_nbins
	    vlow = t_vlow
	    vhigh = t_vhigh
	    dx = t_dx
	    clow = t_clow
	    chigh = t_chigh

	    itp = tbtopn (Memc[intab], READ_ONLY, NULL)
	    call tbcfnd (itp, colname, cptr, 1)
	    if (cptr == NULL) {
		call tbtclo (itp)
		call eprintf ("column not found in %s\n")
		    call pargstr (Memc[intab])
		if ( ! listout )	# skip next output table
		    junk = tbnget (list2, Memc[outtab], SZ_FNAME)
		next
	    }

	    # Get lower & upper limits for the histogram.
	    iferr {
		call th_limits (itp, cptr, Memc[range_string],
			nbins, vlow, vhigh, dx, clow, chigh,
			got, find_datamin, find_datamax)
	    } then {
		call erract (EA_WARN)
		call eprintf ("Table `%s' will be skipped.\n")
		    call pargstr (Memc[intab])
		call tbtclo (itp)
		next
	    }

	    # Get scratch space for the histogram.
	    call malloc (val, nbins, TY_DOUBLE)
	    call malloc (counts, nbins, TY_INT)

	    # Make the histogram.
	    call th_mk_hist (itp, cptr, nbins, vlow, vhigh, dx,
			Memc[range_string], Memd[val], Memi[counts], nrows)

	    if ( listout ) {
		call printf ("# %d rows %s\n")
		    call pargi (nrows)
		    call pargstr (Memc[intab])
		do i = 1, nbins {
		    call printf ("%15.7g %8d\n")
			call pargd (Memd[val+i-1])
			call pargi (Memi[counts+i-1])
		}
	    } else {

		# Create output table & define columns.
		junk = tbnget (list2, Memc[outtab], SZ_FNAME)
		call tbfpri (Memc[intab], Memc[outtab], phu_copied)
		otp = tbtopn (Memc[outtab], NEW_FILE, NULL)
		call tbcdef (otp, ocpx,
			outcolx, "", "", TY_DOUBLE, 1, 1)
		call tbcdef (otp, ocpy,
			outcoly, "histogram", "", TY_INT, 1, 1)
		call tbtcre (otp)

		# Put info records in the header.
		call tbhadt (otp, "intable", Memc[intab])
		call tbhadt (otp, "colname", colname)
		call tbhadi (otp, "nrows", nrows)

		# Write the values into the output table, and close it.
		call tbcptd (otp, ocpx, Memd[val], 1, nbins)
		call tbcpti (otp, ocpy, Memi[counts], 1, nbins)
		call tbtclo (otp)
	    }
	    call tbtclo (itp)

	    call mfree (counts, TY_INT)
	    call mfree (val, TY_DOUBLE)
	}
	call tbnclose (list1)
	if ( ! listout )
	    call tbnclose (list2)
	call sfree (sp)
end

# th_limits -- get limits for histogram
# This routine determines the lower and upper limits of data values for
# making a histogram.  If either of the input values v1 or v2 is not INDEF
# (i.e. if it was specified by the user), then that value is returned as
# vlow or vhigh respectively.  If either or both are INDEF the minimum and
# maximum values in the table column are gotten, and the minimum and maximum
# are extended a little to ensure that the endpoints are included in the
# histogram.  The range is extended by (max - min) / (nbins - 1) / 2
# on each end.  The parameters nbins, vlow, vhigh, and dx may be updated.

procedure th_limits (tp, cptr, range_str,
		nbins, vlow, vhigh, dx, clow, chigh,
		got, find_datamin, find_datamax)

pointer tp		# i: ptr to table descriptor
pointer cptr		# i: ptr to column descriptor
char	range_str[ARB]	# i: range of row numbers
int	nbins		# io: number of bins
double	vlow, vhigh	# io: lower and upper limits
double	dx		# io: bin width
double	clow, chigh	# i: centers of low and high bins
bool	got[NPAR]	# i: flags to specify what we have got
bool	find_datamin	# i: true if we need to find minimum data value
bool	find_datamax	# i: true if we need to find maximum data value
#--
double	value		# an element gotten from the table
double	vmin, vmax	# min & max values in the column
int	nrows		# number of rows in table
int	row		# row number
int	ranges[3,MAX_RANGES]	# ranges of row numbers
int	nvalues			# returned by decode_ranges and ignored
int	stat			# returned by get_next_number
bool	done
int	decode_ranges(), get_next_number(), tbpsta()

begin
	if (find_datamin || find_datamax) {

	    # We must determine either the minimum or maximum or both.

	    if (decode_ranges (range_str, ranges, MAX_RANGES, nvalues) != OK)
		call error (1, "bad range of row numbers")
	    nrows = tbpsta (tp, TBL_NROWS)

	    # First get initial values for min & max in column.  We can't just
	    # take the first value because it might be INDEF.
	    row = 0				# initialize get_next_number
	    stat = get_next_number (ranges, row)	# get first row number
	    done = (stat == EOF) || (row > nrows)
	    while ( ! done ) {
		call tbegtd (tp, cptr, row, value)
		if ( IS_INDEFD(value) ) {
		    # get next row number
		    stat = get_next_number (ranges, row)
		    if ((stat == EOF) || (row > nrows))
			call error (1, "all values are INDEF")
		} else {
		    vmin = value
		    vmax = value
		    done = true
		}
	    }

	    # Update min & max values.
	    stat = get_next_number (ranges, row)	# get next row number
	    done = (stat == EOF) || (row > nrows)
	    while ( ! done ) {
		call tbegtd (tp, cptr, row, value)
		if ( !IS_INDEFD(value) ) {
		    if (value < vmin)
			vmin = value
		    if (value > vmax)
			vmax = value
		}
		stat = get_next_number (ranges, row)	# get next row number
		if ((stat == EOF) || (row > nrows))
		    done = true
	    }

	    # Update parameter values.
	    call th_update (vmin, vmax, nbins, vlow, vhigh, dx, clow, chigh,
			got, find_datamin, find_datamax)
	}
end

# th_mk_hist -- make the histogram

procedure th_mk_hist (tp, cptr, nbins, vlow, vhigh, dx, range_str,
		val, counts, nrows)

pointer tp		# i: ptr to table descriptor
pointer cptr		# i: ptr to column descriptor
int	nbins		# i: number of bins
double	vlow		# i: min value for histogram
double	vhigh		# i: max value for histogram
double	dx		# i: bin width
char	range_str[ARB]	# i: range of row numbers
double	val[nbins]	# o: array of values at center of bins
int	counts[nbins]	# o: the histogram, array of counts within bins
int	nrows		# o: number of rows in range_str and value within limits
#--
double	value		# an element gotten from the table
int	bin		# bin number for output
int	totalrows	# total number of rows in table
int	row		# row number
int	i
int	ranges[3,MAX_RANGES]	# ranges of row numbers
int	nvalues			# returned by decode_ranges and ignored
int	stat			# returned by get_next_number
bool	done
int	decode_ranges(), get_next_number(), tbpsta()

begin
	totalrows = tbpsta (tp, TBL_NROWS)

	# Initialize the range of row numbers.
	if (decode_ranges (range_str, ranges, MAX_RANGES, nvalues) != OK)
	    call error (1, "bad range of row numbers")

	do i = 1, nbins {
	    val[i] = vlow + (i - 0.5d0) * dx	# value at center of bin
	    counts[i] = 0			# initialize histogram
	}

	nrows = 0				# initialize counter
	row = 0					# initialize get_next_number
	stat = get_next_number (ranges, row)	# get first row number
	done = (stat == EOF) || (row > totalrows)
	while ( ! done ) {
	    call tbegtd (tp, cptr, row, value)
	    if ( ! IS_INDEF(value) ) {
		if (value >= vlow && value < vhigh) {
		    if (dx > 0.d0)
			bin = int ((value - vlow) / dx) + 1
		    else
			bin = 1
		    counts[bin] = counts[bin] + 1
		    nrows = nrows + 1
		}
	    }
	    stat = get_next_number (ranges, row)	# get next row number
	    done = (stat == EOF) || (row > totalrows)
	}
end
