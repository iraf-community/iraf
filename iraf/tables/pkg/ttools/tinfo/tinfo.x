include	<error.h>
include <fset.h>	# used to check whether input is redirected
include <tbset.h>

# tinfo -- get information about a table
# This task displays such information as the number of rows and columns
# in a table.  The values are also put into parameters for the task.
#
# Phil Hodge, 22-Jul-1987  Task created
# Phil Hodge, 11-Aug-1987  Delete call to tbtext.
# Phil Hodge, 28-Aug-1987  Include maxcols, change name allpar-->maxpar
# Phil Hodge,  7-Sep-1988  Change parameter name for table.
# Phil Hodge,  9-Dec-1988  Input can be a list of tables.
# Phil Hodge, 10-May-1991  Use clpopns instead of clpopnu.
# Phil Hodge, 23-Aug-1991  Change "user" to "header" in output.
# Phil Hodge, 16-Mar-1992  Include text table type.
# Phil Hodge, 26-Mar-1992  Remove call to tbtext; use tbtnam instead.
# Phil Hodge,  8-Apr-1993  Also write software version number to par file.
# Phil Hodge,  6-Aug-1993  Change "version" to "tblversion".
# Phil Hodge, 21-Dec-1994  Change rowlen and rowused from int to real.
# Phil Hodge,  1-Jul-1995  Modify for FITS tables.
# Phil Hodge,  3-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge,  9-Jun-1999  Print table subtype, and save to new cl parameter;
#	if input is redirected, set input to STDIN without getting cl param.
#	Change "row" & "column" to subtypes of table type "stsdas".
# Phil Hodge, 22-Feb-2002  For image subtype, change what is printed from
#	"primary header" to "image", since it can now be an image extension.

procedure tinfo()

pointer tlist		# for list of input table names
pointer tp		# pointer to table descriptor
pointer sp
pointer tname		# scratch for table name
pointer ttype		# scratch for table type
pointer tsubtype	# scratch for table subtype
bool	ttout		# true if the user wants output to the terminal
int	nrows		# number of rows written to the table
int	ncols		# number of columns defined
int	npar		# number of header parameters written to the table
real	rowlen		# (r) row length (unit = SZ_REAL)
real	rowused		# (r) portion of row length used by the defined columns
int	allrows		# (c) number of rows allocated
int	maxpar		# space allocated for header parameters
int	maxcols		# space allocated for column descriptors
int	tbltype		# table type
int	tbl_subtype	# table subtype
int	tblversion	# version number of software that created the table
pointer tbtopn()
int	tbpsta()
bool	clgetb()
int	fstati()
pointer tbnopenp(), tbnopen()
int	tbnget()

begin
	call smark (sp)
	call salloc (tname, SZ_FNAME, TY_CHAR)
	call salloc (ttype, SZ_FNAME, TY_CHAR)
	call salloc (tsubtype, SZ_FNAME, TY_CHAR)
	Memc[ttype] = EOS
	Memc[tsubtype] = EOS

	if (fstati (STDIN, F_REDIR) == YES)
	    tlist = tbnopen ("STDIN")
	else
	    tlist = tbnopenp ("table")

	ttout = clgetb ("ttout")

	# Do for each table in the input list.
	while (tbnget (tlist, Memc[tname], SZ_FNAME) != EOF) {

	    iferr {
		tp = tbtopn (Memc[tname], READ_ONLY, 0)
	    } then {
		call eprintf ("can't open %s\n")
		    call pargstr (Memc[tname])
		call erract (EA_WARN)
		next
	    }

	    nrows   = tbpsta (tp, TBL_NROWS)
	    ncols   = tbpsta (tp, TBL_NCOLS)
	    npar    = tbpsta (tp, TBL_NPAR)
	    rowlen  = real (tbpsta (tp, TBL_ROWLEN_CHAR)) / SZ_REAL
	    rowused = real (tbpsta (tp, TBL_ROWLEN_CHAR_USED)) / SZ_REAL
	    allrows = tbpsta (tp, TBL_ALLROWS)
	    maxpar  = tbpsta (tp, TBL_MAXPAR)
	    maxcols = tbpsta (tp, TBL_MAXCOLS)
	    tbltype = tbpsta (tp, TBL_WHTYPE)
	    tbl_subtype = tbpsta (tp, TBL_SUBTYPE)
	    tblversion  = tbpsta (tp, TBL_VERSION)

	    # Express the table type as a string.
	    if (tbltype == TBL_TYPE_S_ROW)
		call strcpy ("stsdas", Memc[ttype], SZ_FNAME)
	    else if (tbltype == TBL_TYPE_S_COL)
		call strcpy ("stsdas", Memc[ttype], SZ_FNAME)
	    else if (tbltype == TBL_TYPE_TEXT)
		call strcpy ("text", Memc[ttype], SZ_FNAME)
	    else if (tbltype == TBL_TYPE_FITS)
		call strcpy ("fits", Memc[ttype], SZ_FNAME)
	    else
		call strcpy ("unknown", Memc[ttype], SZ_FNAME)

	    # Express the subtype as a string.
	    if (tbltype == TBL_TYPE_TEXT) {
		if (tbl_subtype == TBL_SUBTYPE_SIMPLE)
		    call strcpy ("simple", Memc[tsubtype], SZ_FNAME)
		else if (tbl_subtype == TBL_SUBTYPE_EXPLICIT)
		    call strcpy ("explicit column definitions",
				Memc[tsubtype], SZ_FNAME)
	    } else if (tbltype == TBL_TYPE_FITS) {
		if (tbl_subtype == TBL_SUBTYPE_ASCII)
		    call strcpy ("ascii", Memc[tsubtype], SZ_FNAME)
		else if (tbl_subtype == TBL_SUBTYPE_BINTABLE)
		    call strcpy ("binary", Memc[tsubtype], SZ_FNAME)
		else if (tbl_subtype == TBL_SUBTYPE_IMAGE)
		    call strcpy ("image", Memc[tsubtype], SZ_FNAME)
	    } else if (tbltype == TBL_TYPE_S_ROW) {
		call strcpy ("row ordered", Memc[tsubtype], SZ_FNAME)
	    } else if (tbltype == TBL_TYPE_S_COL) {
		call strcpy ("column ordered", Memc[tsubtype], SZ_FNAME)
	    } else {
		call strcpy ("N/A", Memc[tsubtype], SZ_FNAME)
	    }

	    if (ttout) {
		call tbtnam (tp, Memc[tname], SZ_FNAME)	# get full name
		call printf ("# %s\n")
		    call pargstr (Memc[tname])
		call printf ("%4d rows written to table\n")
		    call pargi (nrows)
		call printf ("%4d columns defined\n")
		    call pargi (ncols)
		call printf ("%4d header parameters written to table\n")
		    call pargi (npar)
		if (tbltype == TBL_TYPE_S_ROW) {
		    call printf ("%6.1f row length in units of SZ_REAL\n")
			call pargr (rowlen)
		    call printf ("%6.1f amount of row length used\n")
			call pargr (rowused)
		} else if (tbltype == TBL_TYPE_S_COL) {
		    call printf ("%4d rows allocated\n")
			call pargi (allrows)
		}
		call printf ("%4d records allocated for header parameters\n")
		    call pargi (maxpar)
		call printf ("%4d space allocated for column descriptors\n")
		    call pargi (maxcols)
		call printf ("table type:  %s")
		    call pargstr (Memc[ttype])
		if (tbl_subtype != TBL_SUBTYPE_SIMPLE &&
		    tbl_subtype != TBL_SUBTYPE_BINTABLE &&
		    tbltype != TBL_TYPE_S_ROW) {
		    call printf ("  %s")
			call pargstr (Memc[tsubtype])
		}
		call printf ("\n")	# after table type and subtype
	    }
	    call tbtclo (tp)
	}

	call clputi ("nrows",   nrows)
	call clputi ("ncols",   ncols)
	call clputi ("npar",    npar)
	call clputr ("rowlen",  rowlen)
	call clputr ("rowused", rowused)
	call clputi ("allrows", allrows)
	call clputi ("maxpar",  maxpar)
	call clputi ("maxcols", maxcols)
	call clpstr ("tbltype", Memc[ttype])
	call clpstr ("subtype", Memc[tsubtype])
	call clputi ("tblversion", tblversion)

	call tbnclose (tlist)
	call sfree (sp)
end
