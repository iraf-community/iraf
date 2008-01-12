include <fset.h>		# to check whether I/O is redirected

define	SYNTAX		   1

# TSELECT -- Create a new table from selected rows of an old table
#
# B.Simon	 7-Oct-1987	First Code
# Phil Hodge	 7-Sep-1988	Change parameter names for tables.
# Phil Hodge	 4-Oct-1995	Use table name template routines tbnopenp, etc.
# B.Simon	25-Aug-1998	Changed to write directly to output table
# Phil Hodge	 8-Apr-1999	Call tbfpri.
# Phil Hodge	 9-Jun-1999	Set input/output to STDIN/STDOUT if redirected.

procedure t_tselect()

pointer ilist			# Input table name template
pointer olist			# Output table name template
pointer	expr			# Expression used to select rows
#--
int	junk
int	phu_copied		# set by tbfpri and ignored
pointer sp, itp, otp, intable, outtable

string	nomatch	"Number of input tables must match output tables"

int	fstati()
int	tbnget(), tbnlen()
pointer	tbtopn(), tbnopenp(), tbnopen()

begin
	# Allocate stack memory for strings

	call smark (sp)
	call salloc (intable, SZ_FNAME, TY_CHAR)
	call salloc (outtable, SZ_FNAME, TY_CHAR)
	call salloc (expr, SZ_LINE, TY_CHAR)

	# Read the task parameters

	if (fstati (STDIN, F_REDIR) == YES)
	    ilist = tbnopen ("STDIN")
	else
	    ilist = tbnopenp ("intable")

	if (fstati (STDOUT, F_REDIR) == YES)
	    olist = tbnopen ("STDOUT")
	else
	    olist = tbnopenp ("outtable")

	call clgstr ("expr", Memc[expr], SZ_LINE)

	# Loop over all table names in the input file name template

	if (tbnlen (ilist) != tbnlen (olist))
	    call error (SYNTAX, nomatch)

	while (tbnget (ilist, Memc[intable], SZ_FNAME) != EOF) {
	    junk = tbnget (olist, Memc[outtable], SZ_FNAME)

	    # Open the tables

	    itp = tbtopn (Memc[intable], READ_ONLY, NULL)
	    call tbfpri (Memc[intable], Memc[outtable], phu_copied)
	    otp = tbtopn (Memc[outtable], NEW_COPY, itp)

	    # Copy header and selected rows to output table

	    call tbtcre (otp)
	    call tbhcal (itp, otp)
	    call subset (itp, otp, Memc[expr])

	    # Close the tables

	    call tbtclo (itp)
	    call tbtclo (otp)
	}

	# Close the filename template lists

	call tbnclose (ilist)
	call tbnclose (olist)
	call sfree (sp)
end
