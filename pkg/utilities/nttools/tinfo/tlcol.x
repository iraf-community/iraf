include <error.h>
include <fset.h>	# used to check whether input is redirected
include <tbset.h>

define	T_MAXDIM	7	# maximum dimension of array
define  SZ_DTYPE	29	# size of string containing column data type

# tlcol -- list column information
# This task writes information about the columns in a table.  At least
# the column names will be listed, and the data types, formats, and units
# may also be listed.  The column name and units are allowed (at least
# by this program) to contain embedded blanks, in which case they will
# be printed enclosed in double quotes.
#
# Phil Hodge, 23-Jul-1987  Task created
# Phil Hodge, 11-Aug-1987  Modify for change in datatype for char string to -n
#			 and change in calling sequence of inquotes.
# Phil Hodge,  3-Feb-1988  Left-justify info except for print format.
# Phil Hodge,  7-Sep-1988  Change parameter name for table.
# Phil Hodge,  9-Dec-1988  Input can be a list of tables.
# Phil Hodge, 10-May-1991  Use clpopns instead of clpopnu.
# Phil Hodge, 26-Mar-1992  Remove call to tbtext; use tbtnam instead.
# Phil Hodge,  1-Apr-1993  Include short datatype.
# Phil Hodge,  6-Jun-1994  Call erract if error opening table; flush STDOUT.
# Phil Hodge, 18-Nov-1994  Print array size combined with data type;
#			increase SZ_DTYPE from 9 to 29; increase width of
#			field for printing data type from 6 to 8.
# Phil Hodge, 13-Jan-1995  Change calling sequence of inquotes.
# Phil Hodge, 19-Jul-1995  Add tp to calling sequence of tl_dtype.
# Phil Hodge,  3-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge,  7-Jun-1999  If input is redirected, set input to STDIN without
#	getting cl param.

procedure tlcol()

pointer tlist			# for list of input table names
pointer tp			# pointer to descriptor for input table
pointer cp			# pointer to column descriptor
pointer sp
pointer tname			# pointer to scratch space for table name
pointer cname, cunits, cfmt	# pointers to scratch space for column info
char	chartyp[SZ_DTYPE]	# data type expressed as a string
int	datatype		# column data type
int	nelem, lenfmt		# length of array; width of format
int	ncols			# number of columns in table
int	nlist			# number of items to list (from one to four)
int	k			# loop index
int	colnum			# column number (ignored)
pointer tbtopn(), tbpsta(), tbcnum()
int	clgeti()
int	fstati()
pointer tbnopenp(), tbnopen()
int	tbnget()

begin
	call smark (sp)
	call salloc (tname, SZ_LINE, TY_CHAR)
	call salloc (cname, SZ_LINE, TY_CHAR)
	call salloc (cunits, SZ_LINE, TY_CHAR)
	call salloc (cfmt, SZ_COLFMT, TY_CHAR)

	if (fstati (STDIN, F_REDIR) == YES)
	    tlist = tbnopen ("STDIN")
	else
	    tlist = tbnopenp ("table")
	nlist = clgeti ("nlist")

	# Do for each table in the input list.
	while (tbnget (tlist, Memc[tname], SZ_LINE) != EOF) {

	    iferr {
		tp = tbtopn (Memc[tname], READ_ONLY, 0)
	    } then {
		call eprintf ("# %s\n")
		    call pargstr (Memc[tname])
		call erract (EA_WARN)
		next
	    }

	    call tbtnam (tp, Memc[tname], SZ_LINE)	# get full name
	    call printf ("# %s\n")
		call pargstr (Memc[tname])

	    ncols = tbpsta (tp, TBL_NCOLS)

	    do k = 1, ncols {
		cp = tbcnum (tp, k)
		call tbcinf (cp,
			colnum, Memc[cname], Memc[cunits], Memc[cfmt],
			datatype, nelem, lenfmt)

		# Enclose column name in quotes if it contains embedded
		# or trailing blanks.
		call inquotes (Memc[cname], Memc[cname], SZ_LINE, YES)
		call printf ("%-16s")		# but name can be longer
		    call pargstr (Memc[cname])

		if (nlist > 1) {		# also print data type

		    # Convert integer data type code to a character string,
		    # and append info about array size if > 1.
		    call tl_dtype (tp, cp, datatype, nelem, chartyp, SZ_DTYPE)

		    call printf (" %-8s")
			call pargstr (chartyp)

		    if (nlist > 2) {		# also print format for display
			call printf (" %8s")
			    call pargstr (Memc[cfmt])

			if (nlist > 3) {	# also print column units
			    # The "NO" means ignore trailing blanks.
			    call inquotes (Memc[cunits], Memc[cunits],
					SZ_LINE, NO)
			    call printf (" %-16s")	# but can be longer
				call pargstr (Memc[cunits])
			}
		    }
		}
		call printf ("\n")		# end of line for each column
	    }
	    call flush (STDOUT)
	    call tbtclo (tp)
	}

	call tbnclose (tlist)
	call sfree (sp)
end
