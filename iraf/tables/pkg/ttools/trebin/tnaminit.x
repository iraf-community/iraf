include <fset.h>		# to check whether input or output is redirected

# tnam_init -- initialize for input & output names
# Get the input and output table name lists.  If the output is just a
# directory name, the name will be copied to outdir; otherwise, the
# number of names in the input and output lists must be the same.
#
# Phil Hodge, 14-Apr-1988  Task created.
# Phil Hodge, 17-Jun-1993  Change YES to NO in calls to fntopnb.
# Phil Hodge,  4-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge, 22-Apr-1999  Include explicit test for STDOUT, since
#		isdirectory thinks STDOUT is a directory.
# Phil Hodge,  8-Jun-1999  Set input/output to STDIN/STDOUT if redirected.
# Phil Hodge, 25-Apr-2000  Get inlist, xlist, outlist in trebin, and add
#		those three and xin_t to the calling sequence.

procedure tnam_init (inlist, xlist, outlist,
		in_t, xin_t, out_t, outdir, maxch)

char	inlist[ARB]		# i: list of input table names
char	xlist[ARB]		# i: list of table names for output indep var
char	outlist[ARB]		# i: list of output table names
pointer in_t			# o: fnt pointer for input tables
pointer xin_t			# o: fnt pointer for tables of output X
pointer out_t			# o: fnt pointer for output tables
char	outdir[ARB]		# o: if dir_only, name of output directory
int	maxch			# i: size of outdir string
#--
int	n_in, n_xin, n_out	# number of tables in each list
bool	dir_only		# output just a directory name?
pointer tbnopen()
int	isdirectory(), tbnlen()
bool	strne()

begin
	dir_only = false
	if (isdirectory (outlist, outdir, SZ_LINE) > 0 &&
		strne (outlist, "STDOUT"))
	    dir_only = true

	in_t = tbnopen (inlist)
	xin_t = tbnopen (xlist)

	n_in = tbnlen (in_t)
	n_xin = tbnlen (xin_t)
	if (n_xin < 1) {
	    call tbnclose (xin_t)
	    xin_t = NULL
	}

	if (dir_only) {
	    out_t = NULL
	    n_out = 0
	} else {
	    out_t = tbnopen (outlist)
	    n_out = tbnlen (out_t)
	}

	if (xin_t != NULL) {
	    # It's OK to have just one xtable for all intables.
	    if (n_in != n_xin && n_xin != 1) {
		call tnam_cls (in_t, xin_t, out_t)
		call error (1,
			"intable and xtable lists are not the same length")
	    }
	}

	if (out_t != NULL) {
	    if (n_in != n_out) {
		call tnam_cls (in_t, xin_t, out_t)
		call error (1,
			"intable and outtable lists are not the same length")
	    }
	}
end
