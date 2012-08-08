include	"../lib/mctable.h"


# MCT_SAVE - Save table into a text file

procedure mct_save (fname, fmode, table)

char	fname[ARB]		# file name
int	fmode			# file mode
pointer	table			# table descriptor

int	fd			# file descriptor
int	row, col
int	nrows, lastcol

int	open()
char	mct_getc()
short	mct_gets()
int	mct_geti()
long	mct_getl()
real	mct_getr()
double	mct_getd()
complex	mct_getx()
pointer	mct_getp()

errchk	mct_getc(), mct_gets(), mct_geti(), mct_getl()
errchk	mct_getr(), mct_getd(), mct_getx(), mct_getp()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_save: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_save: Bad magic number")

	# Check file mode.
	if (fmode != WRITE_ONLY && fmode != NEW_FILE && 
	    fmode != NEW_FILE   && fmode != TEMP_FILE)
	    call error (0, "mct_save: Bad file mode")

	# Open file.
	iferr (fd = open (fname, fmode, TEXT_FILE))
	    call error (0, "mct_save: Cannot open file")

	# Write table structure.
	call fprintf (fd, "%d	# MCT_MAGIC\n")
	    call pargi (MCT_MAGIC (table))
	call fprintf (fd, "%d	# MCT_TYPE\n")
	    call pargi (MCT_TYPE (table))
	call fprintf (fd, "%d	# MCT_MAXROW\n")
	    call pargi (MCT_MAXROW (table))
	call fprintf (fd, "%d	# MCT_MAXCOL\n")
	    call pargi (MCT_MAXCOL (table))
	call fprintf (fd, "%d	# MCT_INCROWS\n")
	    call pargi (MCT_INCROWS (table))
	call fprintf (fd, "%d	# MCT_NPROWS\n")
	    call pargi (MCT_NPROWS (table))
	call fprintf (fd, "%d	# MCT_NPCOLS\n")
	    call pargi (MCT_NPCOLS (table))
	call fprintf (fd, "%d	# MCT_NGROWS\n")
	    call pargi (MCT_NGROWS (table))
	call fprintf (fd, "%d	# MCT_NGCOLS\n")
	    call pargi (MCT_NGCOLS (table))
	call fprintf (fd, "%d	# MCT_DATA\n")
	    call pargi (MCT_DATA (table))

	# Loop over rows.
	nrows = MCT_NPROWS (table)
	lastcol = MCT_MAXCOL (table)
	do row = 1, nrows {

	    # In the last row the column loop should go only until the highest
	    # column.
	    if (row == nrows)
		lastcol = MCT_NPCOLS (table)

	    # Loop over columns.
	    for (col = 1; col <= lastcol; col = col + 1) {
		switch (MCT_TYPE (table)) {
		case TY_CHAR:
		    call fprintf (fd, "%c ")
			call pargc (mct_getc (table, row, col))
		case TY_SHORT:
		    call fprintf (fd, "%d ")
			call pargs (mct_gets (table, row, col))
		case TY_INT:
		    call fprintf (fd, "%d ")
			call pargi (mct_geti (table, row, col))
		case TY_LONG:
		    call fprintf (fd, "%d ")
			call pargl (mct_getl (table, row, col))
		case TY_REAL:
		    call fprintf (fd, "%g ")
			call pargr (mct_getr (table, row, col))
		case TY_DOUBLE:
		    call fprintf (fd, "%g ")
			call pargd (mct_getd (table, row, col))
		case TY_COMPLEX:
		    call fprintf (fd, "%z ")
			call pargx (mct_getx (table, row, col))
		case TY_POINTER:
		    call fprintf (fd, "%d ")
			call pargi (mct_getp (table, row, col))
		default:
		    call error (0, "mct_save: Unknown data type")
		}
	    }
	    call fprintf (fd, "\n")
	}

	# Close file.
	call close (fd)
end
