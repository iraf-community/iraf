include <error.h>
include <tbset.h>
include "trebin.h"

# tuinterp -- interpolate to regrid a table
# Open the input & output tables, interpolate to uniformly spaced values
# of the independent variable, and close the tables.
#
# Phil Hodge, 15-Apr-1988  Subroutine created
# Phil Hodge, 12-May-1989  Include check for not enough data to interpolate.
# Phil Hodge, 12-Jun-1989  Also copy header parameters.
# Phil Hodge, 30-Jan-1992  Call tbtclo instead of close.
# Phil Hodge, 16-Jun-1993  Check number of rows for interpolation function.
# Phil Hodge,  4-Apr-1994  Errchk tbtopn, and use iferr for tbtcre.
# Phil Hodge, 20-May-1996  Pass extrapolate and ext_value to tuival.
# Phil Hodge, 29-Jul-1998  Add iv_step to calling sequence of tuival.
# Phil Hodge,  8-Apr-1999  Call tbfpri.
# Phil Hodge, 22-Apr-1999  Don't set output table type if outtable = STDOUT.
# Phil Hodge, 25-Apr-2000  Add xtable, padvalue, verbose to calling sequence.
# Phil Hodge, 30-Oct-2001  Delete just the output table, not the whole file,
#			if there's an error.
# Phil Hodge,  2-Jan-2002  Remove the statements to delete the output table
#			in case the input table is not monotonic (because
#			calling tbtclo for a text table caused the error
#			message to be replaced by a misleading message).

procedure tuinterp (intable, xtable, outtable,
		i_func, iv_colname, iv_start, iv_end, iv_step,
		extrapolate, ext_value, padvalue, verbose)

char	intable[ARB]		# i: name of input table
char	xtable[ARB]		# i: table of output indep var values
char	outtable[ARB]		# i: name of output table
int	i_func			# i: interpolation function code
char	iv_colname[SZ_COLNAME]	# i: name of independent variable column
double	iv_start		# i: starting value of independent variable
double	iv_end			# i: ending value of independent variable
double	iv_step			# i: increment in independent variable
bool	extrapolate		# i: true means don't use ext_value
double	ext_value		# i: value to assign to extrapolated points
double	padvalue		# i: value at end of input indep. var. to ignore
bool	verbose			# i: print info?
#--
pointer sp
pointer itp, otp		# descr of input & output tables
pointer iv_icp			# descr for input indep var column
pointer iv_ocp			# descr for output indep var column
pointer icpp, ocpp		# column descr for i & o tables
pointer xout			# scratch for output indep var values
int	ttype			# indicates row- or column-ordered
int	ncols			# number of dependent variable columns
int	incols			# total number of input columns
int	outrows			# number of rows in output table
int	phu_copied		# set by tbfpri and ignored
bool	array			# true if indep var column contains arrays
pointer tbtopn()
int	tbpsta()
bool	strne()
errchk	tbfpri, tbtopn, tbtdel, tudcol, tuxget, tu_getput

begin
	call smark (sp)

	itp = tbtopn (intable, READ_ONLY, 0)
	incols = tbpsta (itp, TBL_NCOLS)

	# Either read or compute the values of the independent variable
	# at which we will interpolate the values from the input table.
	iferr {
	    call tuxget (xtable, iv_start, iv_end, iv_step, padvalue,
		xout, outrows)
	} then {
	    call tbtclo (itp)
	    call erract (EA_ERROR)
	}

	iferr {
	    call tbfpri (intable, outtable, phu_copied)
	    otp = tbtopn (outtable, NEW_FILE, NULL)
	} then {
	    call mfree (xout, TY_DOUBLE)
	    call tbtclo (itp)
	    call erract (EA_ERROR)
	}

	call salloc (icpp, incols, TY_POINTER)
	call salloc (ocpp, incols, TY_POINTER)

	# Define output columns, and get column pointers.
	iferr {
	    call tudcol (itp, otp, iv_colname, outrows,
		iv_icp, iv_ocp, Memi[icpp], Memi[ocpp], ncols, array, verbose)
	} then {
	    call mfree (xout, TY_DOUBLE)
	    call tbtclo (itp)
	    call tbtclo (otp)
	    call erract (EA_ERROR)
	}

	# Output table should be same type as input table.
	if (strne (outtable, "STDOUT")) {
	    ttype = tbpsta (itp, TBL_WHTYPE)
	    call tbpset (otp, TBL_WHTYPE, ttype)
	    if (ttype == TBL_TYPE_S_COL) {
		call tbpset (otp, TBL_ALLROWS,
			max (outrows, tbpsta (itp, TBL_ALLROWS)))
	    }
	}

	iferr {
	    call tbtcre (otp)			# create output table
	} then {
	    call mfree (xout, TY_DOUBLE)
	    call tbtclo (itp)
	    call tbtclo (otp)
	    call erract (EA_ERROR)
	}

	call tbhcal (itp, otp)			# copy all header parameters

	# For each column, get the data, do the interpolation,
	# write the results.
	iferr {
	    call tu_getput (itp, otp, iv_icp, iv_ocp,
		Memi[icpp], Memi[ocpp], ncols, 
		Memd[xout], outrows, iv_step,
		i_func, extrapolate, ext_value, padvalue, array, verbose)
	} then {
	    call mfree (xout, TY_DOUBLE)
	    call tbtclo (itp)
	    call erract (EA_ERROR)
	}

	call mfree (xout, TY_DOUBLE)
	call sfree (sp)

	call tbtclo (itp)
	call tbtclo (otp)
end
