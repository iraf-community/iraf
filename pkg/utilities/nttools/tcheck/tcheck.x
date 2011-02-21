include <tbset.h>
include "tcheck.h"

# TCHECK -- Perform a consistency check on the rows of a table
#
# B.Simon	20-Aug-90	Original
# B.Simon	29-Jul-92	Fixed bug occuring when irow > nrow
# Phil Hodge	 4-Oct-95	Use table name template routines tbnopenp, etc.

procedure tcheck ()

#--
pointer	input		# Table file name template
pointer	chkfile		# Text file containing consistency checks

bool	title		
int	fd, iline, nc
int	keystart, cmdstart, irow, jrow, nrow
pointer	sp, tabname, errmsg, command, tp

string	badexpr  "Syntax error: %s"

int	open(), tbnget(), getlongline()
int	tbpsta(), strlen(), tbl_search()
pointer	tbnopenp(), tbtopn()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (chkfile, SZ_FNAME, TY_CHAR)
	call salloc (tabname, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)
	call salloc (command, SZ_COMMAND, TY_CHAR)

	# Read the task parameters

	input = tbnopenp ("input")
	call clgstr ("chkfile", Memc[chkfile], SZ_FNAME)

	fd = open (Memc[chkfile], READ_ONLY, TEXT_FILE)

	# Check each table

	while (tbnget (input, Memc[tabname], SZ_FNAME) != EOF) {
	    call seek (fd, BOF)
	    tp = tbtopn (Memc[tabname], READ_ONLY, NULL)
	    nrow = tbpsta (tp, TBL_NROWS)
	    title = true

	    # Get each line from the command file

	    repeat {
		nc  = getlongline (fd, Memc[command], SZ_COMMAND, iline)
		if (nc <= 0)
		    break

		Memc[command+nc-1] = EOS
		call cmdsplit (Memc[command], keystart, cmdstart)
		if (cmdstart > 0) {
		    irow = 1
		    while (irow <= nrow) {
			jrow = tbl_search (tp, Memc[command+cmdstart-1], 
					   irow, nrow)
			if (jrow == 0) {
			    break

			} else if (jrow == ERR) {
			    call xer_reset
			    if (strlen (Memc[command+cmdstart-1]) > 60)
				call strcat (" ...", Memc[command+cmdstart+60],
					     SZ_COMMAND)

			    call sprintf (Memc[errmsg], SZ_LINE, badexpr)
			    call pargstr (Memc[command+cmdstart-1])
			    call error (SYNTAX, Memc[errmsg])

			} else {
			    call wrt_check (tp, jrow, Memc[command+keystart-1],
					    Memc[command+cmdstart-1], title)
			    irow = jrow + 1
			}
		    }
		}
	    }
	    call tbtclo (tp)
	}	

	call tbnclose (input)
	call sfree (sp)
end
