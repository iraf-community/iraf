include <tbset.h>

#  TITABLE  --  Insert 2D tables into 3D table rows.
#
#  Input tables are given by a filename template list. All row/column 
#  selection on input tables is performed by bracket-enclosed selectors
#  appended to the file name. The output is a 3-D table with no row/column
#  selectors. 
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-97  -  Task created (I.Busko)
#  17-Mar-97  -  Revised after code review (IB)
#   8-Apr-02  -  Remove the unused strings for error messages (P. Hodge)
#   8-Dec-03  -  Use tbtacc() instead of access() to test for a new table;
#                use mfree instead of tcs_close for cpo.


procedure t_titable()

char	tablist[SZ_LINE]		# Input table list
char	output[SZ_PATHNAME]		# Output table name
char	template[SZ_PATHNAME]		# Template table name
int	row				# Row where to begin insertion
bool	verbose				# Print operations ?
#--
char	root[SZ_FNAME]
char	rowselect[SZ_FNAME]
char	colselect[SZ_FNAME]
char	colname[SZ_COLNAME]
char	colunits[SZ_COLUNITS]
char	colfmt[SZ_COLFMT]
pointer	cpo
pointer	otp, list
int	ncpo, rowc
bool	rflag

pointer	imtopen()
int	clgeti(), tbtacc()
bool	clgetb(), streq()

begin
	# Get task parameters.

	call clgstr ("intable", tablist, SZ_LINE)
	call clgstr ("outtable", output, SZ_PATHNAME)
	call clgstr ("template", template, SZ_PATHNAME)
	row     = clgeti ("row")
	verbose = clgetb ("verbose")

	# Abort if invalid output name..
	if (streq (output, "STDOUT"))
	    call error (1, "Invalid output file name.")
	call rdselect (output, root, rowselect, colselect, SZ_FNAME)
	if (rowselect[1] != EOS || colselect[1] != EOS)
	    call error (1, "Sections not permitted on output table name.")

	# Open input list.
	list = imtopen (tablist)	

	# Open/create the output table.
	if (tbtacc (output) == YES)
	    call tiupdate (root, otp, cpo, ncpo)
	else
	    call tinew (template, list, root, rowselect, colselect, colname, 
                       colunits, colfmt, otp, cpo, ncpo)

	# Initialize row counter.
	rowc  = row
	rflag = false
	if (rowc <= 0 || IS_INDEFI(rowc)) rflag = true

	# Do the insertion.
	call tinsert (list, output, otp, cpo, ncpo, rowc, rflag, verbose, 
                      rowselect, colselect, colname, colunits, colfmt)

	# Cleanup. The cpo array was allocated by tiupdate/tinew.
	call mfree (cpo, TY_INT)
	call tbtclo (otp)
	call imtclose (list)
end
