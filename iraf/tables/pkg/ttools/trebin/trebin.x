include <error.h>
include <tbset.h>
include "trebin.h"

# trebin -- resample to uniform spacing
# This task resamples a table or list of tables to uniformly spaced
# values of the independent variable.
#
# Phil Hodge, 14-Apr-1988  Task created.
# Phil Hodge, 30-Jan-1992  Delete inlist, outlist.
# Phil Hodge, 16-Jun-1993  Set the sign of 'step' based on 'start' and 'end'.
# Phil Hodge,  4-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge, 21-May-1996  Include extrapolate and ext_value.
# Phil Hodge, 22-Apr-1999  Get 'step' even if 'start' and 'end' are the same.
# Phil Hodge, 25-Apr-2000  Get inlist, outlist, xlist in this routine
#			instead of in tnam_init; also get padvalue.
# Phil Hodge,  4-Nov-2000  It is an error if step = 0, unless start = end

procedure trebin()

pointer sp
pointer inlist			# scratch for list of input table names
pointer outlist			# scratch for list of output table names
pointer xlist			# scratch for list of table names for X
pointer intable			# scratch for name of input table
pointer outtable		# scratch for name of output table
pointer outdir			# scratch for name of output directory
pointer xtable			# scratch for name of indep var table
double	iv_start		# starting value of independent variable
double	iv_end			# ending value of independent variable
double	iv_step			# increment in independent variable
bool	extrapolate		# true means extrapolate if out of bounds
double	ext_value		# value to use when out of bounds
double	padvalue		# value at end of input indep. var. to ignore
char	iv_col[SZ_COLNAME]	# name of independent variable column
char	func[SZ_FNAME]		# interpolation function
int	i_func			# interpolation function
pointer in_t, xin_t, out_t	# fn template pointers for input & output lists
bool	verbose			# print file names?
double	clgetd()
bool	clgetb()
int	tnam_gio()

begin
	# Get input and output table template lists.
	call smark (sp)
	call salloc (inlist, SZ_LINE, TY_CHAR)
	call salloc (outlist, SZ_LINE, TY_CHAR)
	call salloc (xlist, SZ_LINE, TY_CHAR)
	call salloc (intable, SZ_FNAME, TY_CHAR)
	call salloc (outtable, SZ_FNAME, TY_CHAR)
	call salloc (xtable, SZ_FNAME, TY_CHAR)
	call salloc (outdir, SZ_FNAME, TY_CHAR)

	call clgstr ("intable", Memc[inlist], SZ_LINE)
	call clgstr ("outtable", Memc[outlist], SZ_LINE)
	call clgstr ("column", iv_col, SZ_COLNAME)
	call clgstr ("xtable", Memc[xlist], SZ_LINE)

	# Open the input & output lists of table names.
	call tnam_init (Memc[inlist], Memc[xlist], Memc[outlist],
		in_t, xin_t, out_t, Memc[outdir], SZ_FNAME)

	if (xin_t == NULL) {

	    # Get parameters for linearly spaced output independent variable.
	    iv_start = clgetd ("start")
	    iv_end = clgetd ("end")
	    iv_step = clgetd ("step")
	    if (iv_step == 0.d0 && iv_start != iv_end)
		call error (1, "step = 0 is invalid")

	    # Set the sign of 'step', rather than expecting the user
	    # to set it correctly.
	    if (iv_start < iv_end)
		iv_step = abs (iv_step)
	    else if (iv_start > iv_end)
		iv_step = -abs (iv_step)

	} else {

	    iv_start = 0.d0
	    iv_end = 0.d0
	    iv_step = 0.d0
	}

	call clgstr ("function", func, SZ_FNAME)
	extrapolate = clgetb ("extrapolate")
	if (extrapolate)
	    ext_value = INDEFD			# not used
	else
	    ext_value = clgetd ("value")

	padvalue = clgetd ("padvalue")

	verbose = clgetb ("verbose")

	call tuiset (func, i_func)		# set interpolator type

	# Process each table.
	while (tnam_gio (in_t, xin_t, out_t, Memc[outdir],
		Memc[intable], Memc[xtable], Memc[outtable], SZ_FNAME) != EOF) {

	    if (verbose) {
		if (Memc[xtable] != EOS) {
		    call printf ("%s, %s --> %s\n")
			call pargstr (Memc[intable])
			call pargstr (Memc[xtable])
			call pargstr (Memc[outtable])
		} else {
		    call printf ("%s --> %s\n")
			call pargstr (Memc[intable])
			call pargstr (Memc[outtable])
		}
		call flush (STDOUT)
	    }

	    iferr {
		call tuinterp (Memc[intable], Memc[xtable], Memc[outtable],
			i_func, iv_col, iv_start, iv_end, iv_step,
			extrapolate, ext_value, padvalue, verbose)
	    } then {
		call erract (EA_WARN)
		if (verbose) {
		    call eprintf ("This table will be skipped.\n")
		} else {
		    call eprintf ("Table %s will be skipped.\n")
			call pargstr (Memc[intable])
		}
		next
	    }
	}

	call tnam_cls (in_t, xin_t, out_t)
	call sfree (sp)
end
