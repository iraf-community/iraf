include <tbset.h>

# GG_RD2COL -- read X and Y plot data from two column of separate tables

int procedure gg_rd2col (xtable, xcolumn, ytable, ycolumn, 
	rdmarks, errcol, erraxis, 
	x, y, size)

char	xtable[SZ_FNAME], ytable[SZ_FNAME]		# Table names
char	xcolumn[SZ_COLNAME], ycolumn[SZ_COLNAME]	# Column names
bool	rdmarks			# Read errors from file?
char	errcol[SZ_COLNAME]	# Error column name
int	erraxis			# X or Y errors?
pointer	x, y, size		# Pointers to x, y and size vectors

pointer	xtdp, ytdp		# Pointers to table descriptors
pointer	xcdp, ycdp		# Pointers to column descriptors
pointer	null			# Pointer to null
int	numrows			# Number of rows
int	xnumrows, ynumrows	# Number of rows
int	numcols
char	errmsg[SZ_LINE]		# Error message

pointer	tbtopn()
int	tbpsta()
bool	streq()

errchk	tbtopn

begin
    numcols = 1

    xtdp = tbtopn (xtable, READ_ONLY, 0)
    call tbcfnd (xtdp, xcolumn, xcdp, numcols)
    if (xcdp <= 0) {
	call sprintf (errmsg, SZ_LINE, "Cannot find column %s in table %s\n")
	    call pargstr (xcolumn)
	    call pargstr (xtable)
	call error (0, errmsg)
    }

    xnumrows = tbpsta (xtdp, TBL_NROWS)

    if (streq (xtable, ytable))
	ytdp = xtdp
    else
	ytdp = tbtopn (ytable, READ_ONLY, 0)

    call tbcfnd (ytdp, ycolumn, ycdp, numcols)
    if (ycdp <= 0) {
	call sprintf (errmsg, SZ_LINE, "Cannot find column %s in table %s\n")
	    call pargstr (ycolumn)
	    call pargstr (ytable)
	call error (0, errmsg)
    }

    ynumrows = tbpsta (ytdp, TBL_NROWS)

    numrows = min (xnumrows, ynumrows)

    # Allocate space for the plot arrays
#    iferr {
	call malloc (x, numrows, TY_REAL)
	call malloc (y, numrows, TY_REAL)
	call malloc (size, numrows, TY_REAL)
	call malloc (null, numrows, TY_BOOL)
#    } then
#	call erract (EA_FATAL)

    # Read the columns
    call tbcgtr (xtdp, xcdp, Memr[x], Memb[null], 1, numrows)
    call tbcgtr (ytdp, ycdp, Memr[y], Memb[null], 1, numrows)

    if (rdmarks) {
	# Read the errors column
	if (erraxis == 1)
	    # Get errors from X table
	    call ggrser (xtdp, errcol, numrows, 
		Memr[x], Memr[y], Memr[size], erraxis)
	else if (erraxis == 2)
	    # Get errors from Y table
	    call ggrser (ytdp, errcol, numrows, 
		Memr[x], Memr[y], Memr[size], erraxis)
    }

    call tbtclo (xtdp)
    if (xtdp != ytdp)
	call tbtclo (ytdp)
    call mfree  (null, TY_BOOL)

    return (numrows)
end
