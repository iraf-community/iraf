include	"../lib/mctable.h"



# MCT_GET - Get a single value from the table (generic).

char procedure mct_getc (table, row, col)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number

pointer	mct_getrow()
errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_get: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_get: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_CHAR)
	    call error (0, "mct_get: Wrong table type")

	# Check the row and column range.
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_get: Bad row number")
	if (row == MCT_NPROWS (table) && (col < 1 || col > MCT_NPCOLS (table)))
	    call error (0, "mct_get: Bad column number at last row")
	if (row != MCT_NPROWS (table) && (col < 1 || col > MCT_MAXCOL (table)))
	    call error (0, "mct_get: Bad column number")

	# Update the counters.
	MCT_NGROWS (table) = row
	MCT_NGCOLS (table) = col

	# Return value.
	return (Memc[mct_getrow (table, row) + col - 1])
end

# MCT_GET - Get a single value from the table (generic).

short procedure mct_gets (table, row, col)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number

pointer	mct_getrow()
errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_get: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_get: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_SHORT)
	    call error (0, "mct_get: Wrong table type")

	# Check the row and column range.
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_get: Bad row number")
	if (row == MCT_NPROWS (table) && (col < 1 || col > MCT_NPCOLS (table)))
	    call error (0, "mct_get: Bad column number at last row")
	if (row != MCT_NPROWS (table) && (col < 1 || col > MCT_MAXCOL (table)))
	    call error (0, "mct_get: Bad column number")

	# Update the counters.
	MCT_NGROWS (table) = row
	MCT_NGCOLS (table) = col

	# Return value.
	return (Mems[mct_getrow (table, row) + col - 1])
end

# MCT_GET - Get a single value from the table (generic).

int procedure mct_geti (table, row, col)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number

pointer	mct_getrow()
errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_get: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_get: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_INT)
	    call error (0, "mct_get: Wrong table type")

	# Check the row and column range.
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_get: Bad row number")
	if (row == MCT_NPROWS (table) && (col < 1 || col > MCT_NPCOLS (table)))
	    call error (0, "mct_get: Bad column number at last row")
	if (row != MCT_NPROWS (table) && (col < 1 || col > MCT_MAXCOL (table)))
	    call error (0, "mct_get: Bad column number")

	# Update the counters.
	MCT_NGROWS (table) = row
	MCT_NGCOLS (table) = col

	# Return value.
	return (Memi[mct_getrow (table, row) + col - 1])
end

# MCT_GET - Get a single value from the table (generic).

long procedure mct_getl (table, row, col)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number

pointer	mct_getrow()
errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_get: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_get: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_LONG)
	    call error (0, "mct_get: Wrong table type")

	# Check the row and column range.
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_get: Bad row number")
	if (row == MCT_NPROWS (table) && (col < 1 || col > MCT_NPCOLS (table)))
	    call error (0, "mct_get: Bad column number at last row")
	if (row != MCT_NPROWS (table) && (col < 1 || col > MCT_MAXCOL (table)))
	    call error (0, "mct_get: Bad column number")

	# Update the counters.
	MCT_NGROWS (table) = row
	MCT_NGCOLS (table) = col

	# Return value.
	return (Meml[mct_getrow (table, row) + col - 1])
end

# MCT_GET - Get a single value from the table (generic).

real procedure mct_getr (table, row, col)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number

pointer	mct_getrow()
errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_get: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_get: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_REAL)
	    call error (0, "mct_get: Wrong table type")

	# Check the row and column range.
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_get: Bad row number")
	if (row == MCT_NPROWS (table) && (col < 1 || col > MCT_NPCOLS (table)))
	    call error (0, "mct_get: Bad column number at last row")
	if (row != MCT_NPROWS (table) && (col < 1 || col > MCT_MAXCOL (table)))
	    call error (0, "mct_get: Bad column number")

	# Update the counters.
	MCT_NGROWS (table) = row
	MCT_NGCOLS (table) = col

	# Return value.
	return (Memr[mct_getrow (table, row) + col - 1])
end

# MCT_GET - Get a single value from the table (generic).

double procedure mct_getd (table, row, col)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number

pointer	mct_getrow()
errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_get: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_get: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_DOUBLE)
	    call error (0, "mct_get: Wrong table type")

	# Check the row and column range.
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_get: Bad row number")
	if (row == MCT_NPROWS (table) && (col < 1 || col > MCT_NPCOLS (table)))
	    call error (0, "mct_get: Bad column number at last row")
	if (row != MCT_NPROWS (table) && (col < 1 || col > MCT_MAXCOL (table)))
	    call error (0, "mct_get: Bad column number")

	# Update the counters.
	MCT_NGROWS (table) = row
	MCT_NGCOLS (table) = col

	# Return value.
	return (Memd[mct_getrow (table, row) + col - 1])
end

# MCT_GET - Get a single value from the table (generic).

complex procedure mct_getx (table, row, col)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number

pointer	mct_getrow()
errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_get: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_get: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_COMPLEX)
	    call error (0, "mct_get: Wrong table type")

	# Check the row and column range.
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_get: Bad row number")
	if (row == MCT_NPROWS (table) && (col < 1 || col > MCT_NPCOLS (table)))
	    call error (0, "mct_get: Bad column number at last row")
	if (row != MCT_NPROWS (table) && (col < 1 || col > MCT_MAXCOL (table)))
	    call error (0, "mct_get: Bad column number")

	# Update the counters.
	MCT_NGROWS (table) = row
	MCT_NGCOLS (table) = col

	# Return value.
	return (Memx[mct_getrow (table, row) + col - 1])
end

# MCT_GET - Get a single value from the table (generic).

pointer procedure mct_getp (table, row, col)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number

pointer	mct_getrow()
errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_get: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_get: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_POINTER)
	    call error (0, "mct_get: Wrong table type")

	# Check the row and column range.
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_get: Bad row number")
	if (row == MCT_NPROWS (table) && (col < 1 || col > MCT_NPCOLS (table)))
	    call error (0, "mct_get: Bad column number at last row")
	if (row != MCT_NPROWS (table) && (col < 1 || col > MCT_MAXCOL (table)))
	    call error (0, "mct_get: Bad column number")

	# Update the counters.
	MCT_NGROWS (table) = row
	MCT_NGCOLS (table) = col

	# Return value.
	return (MEMP[mct_getrow (table, row) + col - 1])
end
