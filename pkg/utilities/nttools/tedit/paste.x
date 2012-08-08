include <tbset.h>
include "screen.h"
include "table.h"
include "paste.h"

define	char_		90	# goto label
define	EXTRA_SPACE	20	# extra row length & col descr space

# CLS_PASTE -- Close the paste table and free the descriptor

procedure cls_paste (scr)

pointer	scr		# u: Screen descriptor
#--
pointer	sp, fname, paste

string nodelete "Could not delete paste table"

begin
	paste = TED_PASTE(scr)
	if (paste == NULL)
	    return

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call tbtnam (TED_PSTPTR(paste), Memc[fname], SZ_FNAME)
	call tbtclo (TED_PSTPTR(paste))

	iferr (call delete (Memc[fname]))
	    call warn2_prompt (scr, nodelete, Memc[fname])

	call mfree (paste, TY_STRUCT)
	TED_PASTE(scr) = NULL
	call sfree (sp)

end

# MOVE_PASTE -- Move rows to or from the paste table

procedure move_paste (itp, otp, irow, orow, ncopy)

pointer itp		# i: pointer to descriptor for input table
pointer otp		# i: pointer to descriptor for output table
int	irow		# i: first row of input table to be copied
int	orow		# i: row number of first output row
int	ncopy		# i: number of rows to copy
#--
int	jrow, krow

begin
	krow = orow
	do jrow = irow, irow+ncopy-1 {
	    call tbrcpy (itp, otp, jrow, krow)
	    krow = krow + 1
	}
end

# OPN_PASTE -- Open the paste table

pointer procedure opn_paste (scr)

pointer	scr		# u: Screen descriptor
pointer	paste		# o: Paste descriptor
#--
int	try, junk, ncol
pointer	sp, oldtab, newtab, newdir, tab, tp, pp
pointer	fname

int	tbpsta(), tbparse()
pointer	fnldir(), tbtopn()

string	nopaste  "Cannot create paste table. Command aborted."

begin
	# Allocate dynamic memory for file names

	call smark (sp)
	call salloc (oldtab, SZ_FNAME, TY_CHAR)
	call salloc (newtab, SZ_FNAME, TY_CHAR)
	call salloc (newdir, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Get table name (used to build paste table name)

	tab = TED_TABLE(scr)
	tp = TED_TABPTR(tab)

	call tbtnam (tp, Memc[oldtab], SZ_FNAME)
	junk = tbparse (Memc[oldtab], Memc[fname], Memc[newdir], SZ_FNAME,
		junk)		# added by PEH, 1998 Apr 15
	junk = fnldir (Memc[fname], Memc[newdir], SZ_FNAME)

	# Try to open the paste table in the same directory as the
	# temporary table. If this doesn't work, try the tmp$ directory

	pp = NULL
	do try = 1, 2 {
	    call strcat ("paste", Memc[newdir], SZ_FNAME)
	    call mktemp (Memc[newdir], Memc[newtab], SZ_FNAME)

	    ifnoerr (pp = tbtopn (Memc[newtab], NEW_COPY, tp))
		break

	    call strcpy ("tmp$", Memc[newdir], SZ_FNAME)
	}

	if (pp == NULL) {
	    call warn1_prompt (scr, nopaste)
	    TED_PASTE(scr) = NULL
	    return (NULL)
	}

	# Set the parameters of the paste table, then create it

	ncol = tbpsta (tp, TBL_MAXCOLS) 

	call tbpset (pp, TBL_WHTYPE, TBL_TYPE_S_ROW)
	call tbpset (pp, TBL_INCR_ROWLEN, EXTRA_SPACE)
	call tbpset (pp, TBL_MAXCOLS, ncol + EXTRA_SPACE)
	call tbpset (pp, TBL_MAXPAR, 0)	# no header parameters

	iferr (call tbtcre (pp)) {
	    call warn1_prompt (scr, nopaste)
	    TED_PASTE(scr) = NULL
	    return (NULL)
	}

	# Create paste table descriptor

	call malloc (paste, TED_PSTLEN, TY_STRUCT)
	TED_PSTPTR(paste) = pp
	TED_PSTROWS(paste) = 0

	# Update the screen structure and return the paste descriptor

	TED_PASTE(scr) = paste

	call sfree (sp)
	return (paste)

end
