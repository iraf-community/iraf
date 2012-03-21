include	<error.h>
include <fset.h>	# used to check whether input or output is redirected
include <tbset.h>

# tcopy -- Copy table(s)

# The input tables are given by an filename template list.  The output
# is either a matching list of tables or a directory.  The number of
# input tables may be either one or match the number of output tables.
# This is based on the t_imcopy procedure.
#
# Phil Hodge, 21-Aug-1987  Task created.
# Phil Hodge,  7-Sep-1988  Change parameter names for tables.
# Phil Hodge, 28-Dec-1989  Use iferr with call to tbtcpy.
# Phil Hodge, 26-Mar-1992  Remove calls to tbtext.
# Phil Hodge,  1-Jul-1995  Modify for FITS tables.
# Phil Hodge, 19-Jul-1995  Replace fnt calls with tbn.
# B.Simon      9-May-1997  Add code similar to trename
# Phil Hodge,  8-Apr-1999  In one_copy, call tbfpri.
# Phil hodge, 16-Apr-1999  Remove ttype from calling sequence of tbparse;
#			use tbttyp to get table type; ext_type is not called.
# Phil Hodge,  7-Jun-1999  If input or output is redirected, set to STDIN
# 			or STDOUT without getting the cl parameter.
# Phil Hodge, 29-Jun-1999  In one_copy, don't call tbtacc if oldfile is STDIN.
# Phil Hodge,  2-Jan-2001  Check $nargs to see whether input & output were
#			specified, rather than relying exclusively on F_REDIR.
# Phil Hodge, 28-Feb-2002  Add a call to sfree at the end of tcopy.

procedure tcopy()

pointer tablist1		# input table list
pointer tablist2		# output table list
bool	verbose			# print file names?
#--
pointer sp
pointer table1			# input table name
pointer fname1			# input file name (i.e. without brackets)
pointer cdfname			# input CDF name or EXTNAME
pointer table2			# output table name
pointer dir1			# input directory name
pointer dir2			# output directory name

pointer list1, list2, tp
int	root_len		# number of char in input directory name
int	numout			# number of names in output list
bool	fitsout			# is the output just one FITS file?

char	src[SZ_FNAME], extn[SZ_FNAME]

int	nargs			# number of command-line arguments
bool	in_redir, out_redir	# is input or output redirected?

pointer tbnopen(), tbtopn()
int	tbnget(), tbnlen()
int	fstati()
int	fnldir(), isdirectory(), strncmp()
int	junk, hdu, tbparse(), exists, tbttyp()
int	clgeti()
bool	clgetb(), streq()

begin
	call smark (sp)
	call salloc (tablist1, SZ_LINE, TY_CHAR)
	call salloc (tablist2, SZ_LINE, TY_CHAR)
	call salloc (table1, SZ_LINE, TY_CHAR)
	call salloc (fname1, SZ_LINE, TY_CHAR)
	call salloc (cdfname, SZ_LINE, TY_CHAR)
	call salloc (table2, SZ_LINE, TY_CHAR)
	call salloc (dir1, SZ_LINE, TY_CHAR)
	call salloc (dir2, SZ_LINE, TY_CHAR)

	# Get input and output table template lists.  What we do with the
	# command-line arguments depends on how many there are and what
	# (input, output, or both) has been redirected.

	nargs = clgeti ("$nargs")
	in_redir = fstati (STDIN, F_REDIR) == YES
	out_redir = fstati (STDOUT, F_REDIR) == YES

	if (in_redir || out_redir) {

	    if (nargs >= 2) {

		if (in_redir) {
		    call strcpy ("STDIN", Memc[tablist1], SZ_LINE)
		    call clpstr ("intable", "STDIN")	# update par file
		} else {
		    call clgstr ("intable", Memc[tablist1], SZ_LINE)
		}
		call clgstr ("outtable", Memc[tablist2], SZ_LINE)

	    } else if (nargs == 1) {

		if (in_redir) {		# output may also have been redirected
		    # The cl thinks the argument is intable, but it's actually
		    # outtable, so assign the value to tablist2.
		    call strcpy ("STDIN", Memc[tablist1], SZ_LINE)
		    call clgstr ("intable", Memc[tablist2], SZ_LINE)
		    # update par file
		    call clpstr ("intable", "STDIN")
		    call clpstr ("outtable", Memc[tablist2])
		} else {		# only output was redirected
		    call clgstr ("intable", Memc[tablist1], SZ_LINE)
		    call strcpy ("STDOUT", Memc[tablist2], SZ_LINE)
		}

	    } else if (nargs == 0) {

		if (in_redir)
		    call strcpy ("STDIN", Memc[tablist1], SZ_LINE)
		else
		    call clgstr ("intable", Memc[tablist1], SZ_LINE)

		if (out_redir)
		    call strcpy ("STDOUT", Memc[tablist2], SZ_LINE)
		else
		    call clgstr ("outtable", Memc[tablist2], SZ_LINE)
	    }

	} else {
	
	    call clgstr ("intable", Memc[tablist1], SZ_LINE)
	    call clgstr ("outtable", Memc[tablist2], SZ_LINE)
	}

	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (Memc[tablist2], Memc[dir2], SZ_LINE) > 0 &&
		!streq (Memc[tablist2], "STDOUT")) {

	    list1 = tbnopen (Memc[tablist1])

	    while (tbnget (list1, Memc[table1], SZ_LINE) != EOF) {

		# Memc[fname1] is the name without any brackets.  We need to
		# remove brackets because they confuse fnldir, which we use
		# to get the length of any directory prefix.

		junk = tbparse (Memc[table1], Memc[fname1], Memc[cdfname],
			SZ_LINE, hdu)
		root_len = fnldir (Memc[fname1], Memc[dir1], SZ_LINE)

		# Copy the output directory name to table2, and concatenate
		# the input file name (without directory prefix and without
		# the bracket suffix).

		call strcpy (Memc[dir2], Memc[table2], SZ_LINE)
		call strcat (Memc[fname1+root_len], Memc[table2], SZ_LINE)

		call one_copy (Memc[table1], Memc[table2], verbose)
	    }

	    call tbnclose (list1)

	} else {

	    # Dummy open of the old file in case it's a URL.
	    if (strncmp (Memc[tablist1], "http://", 7) == 0) {
	        tp = tbtopn (Memc[tablist1], READ_ONLY, NULL)
	        call tbtclo (tp)
	    }

	    # Expand the input and output table lists.
	    list1 = tbnopen (Memc[tablist1])
	    list2 = tbnopen (Memc[tablist2])

	    numout = tbnlen (list2)
	    fitsout = false			# initial value
	    if (numout == 1) {
		# See if the output is a FITS file.  It's OK to have many
		# input tables with just one output FITS file.
		junk = tbnget (list2, Memc[table2], SZ_LINE)
		call tbnrew (list2)
		if (tbttyp (Memc[table2], exists) == TBL_TYPE_FITS)
		    fitsout = true
	    }

	    if (tbnlen (list1) != numout) {
		if (!fitsout) {
		    call tbnclose (list1)
		    call tbnclose (list2)
		    call error (1,
			"Number of input and output tables are not the same.")
		}
	    }

	    # Copy each table.
	    while (tbnget (list1, Memc[table1], SZ_LINE) != EOF) {
		if (!fitsout)
		    junk = tbnget (list2, Memc[table2], SZ_LINE)

		call one_copy (Memc[table1], Memc[table2], verbose)
	    }

	    call tbnclose (list1)
	    call tbnclose (list2)
	}

	call sfree (sp)
end

# ONE_COPY -- Copy a single table

procedure one_copy (oldfile, newfile, verbose)

char	oldfile[ARB]	# i: current file name
char	newfile[ARB]	# i: new file name
bool	verbose		# i: print informational message
#--
bool	done
int	phu_copied	# set by tbfpri and ignored
pointer	sp, oldname, newname, tp

bool	use_fcopy	# true if we should copy the file with fcopy

pointer	tbtopn()
bool	streq(), is_wholetab()
int	tbtacc(), exists, tbttyp()	# exists is ignored
errchk	tbfpri, tbtcpy, tbtopn

begin
	call smark (sp)
	call salloc (oldname, SZ_FNAME, TY_CHAR)
	call salloc (newname, SZ_FNAME, TY_CHAR)

	# Check to make sure the copy is legal

	done = false
	use_fcopy = false
	if (streq (oldfile, newfile)) {
	    call eprintf ("Cannot copy table to itself:  %s\n")
	    call pargstr (oldfile)

	    if (streq (oldfile, "STDIN")) {
		use_fcopy = true

	    } else if (tbtacc (oldfile) == YES) {
		use_fcopy = true

	    } else {
		call eprintf ("Can only copy tables with tcopy: `%s'\n")
		call pargstr (oldfile)
	    }

	    if (use_fcopy) {
		call tbtext (oldfile, Memc[oldname], SZ_FNAME)
		call tbtext (newfile, Memc[newname], SZ_FNAME)

		iferr (call fcopy (Memc[oldname], Memc[newname])) {
		    call erract (EA_WARN)
		} else {
		    done = true
		}
	    }

	} else {
	    # Table extensions are copied by the table
	    # library function tbtcpy

	    iferr {
		call tbfpri (oldfile, newfile, phu_copied)
		call tbtcpy (oldfile, newfile)
	    } then {
		call erract (EA_WARN)
	    } else {
		done = true
	    }
	}

	# Print verbose message

	if (done && verbose) {
	    call printf ("# %s -> %s\n")
	    call pargstr (oldfile)
	    call pargstr (newfile)
	    call flush (STDOUT)
	}

	call sfree (sp)
	return
end
