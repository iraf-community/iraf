include	"../lib/mctable.h"


# MCT_PUT - Put value randomly (generic)

procedure mct_putc (table, row, col, value)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number
char	value			# data value

int	offset
pointer	base
errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_put: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_put: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_CHAR)
	    call error (0, "mct_put: Wrong table type")

	# Test row and column values.
	if (row < 1)
	    call error (0, "mct_put: Row number less than one")
	if (col < 1 || col > MCT_MAXCOL (table))
	    call error (0, "mct_put: Column out of range")

	# Reallocate space if necessary.
	if (row > MCT_MAXROW (table)) {

	    # Compute offset of new area.
	    offset = MCT_MAXROW (table) * MCT_MAXCOL (table)

	    # Recompute new number of rows and reallocate buffer.
	    MCT_MAXROW (table) = MCT_MAXROW (table) + MCT_INCROWS (table)
	    call realloc (MCT_DATA (table), 
		MCT_MAXROW (table) * MCT_MAXCOL (table), TY_CHAR)

	    # Compute base address of new area and clear it with INDEF.
	    base = MCT_DATA (table) + offset
	    call mct_indef (table, base,
			    MCT_INCROWS (table) * MCT_MAXCOL (table))
	}

	# Update row and column counter, only if the new entries are beyond
	# the old limits.

	if (row > MCT_NPROWS (table)) {
	    MCT_NPROWS (table) = row
	    MCT_NPCOLS (table) = col
	} else if (col > MCT_NPCOLS (table))
	    MCT_NPCOLS (table) = col

	# Enter variable.
	base = MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table)
	Memc[base + col - 1] = value
end

# MCT_PUT - Put value randomly (generic)

procedure mct_puts (table, row, col, value)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number
short	value			# data value

int	offset
pointer	base
errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_put: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_put: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_SHORT)
	    call error (0, "mct_put: Wrong table type")

	# Test row and column values.
	if (row < 1)
	    call error (0, "mct_put: Row number less than one")
	if (col < 1 || col > MCT_MAXCOL (table))
	    call error (0, "mct_put: Column out of range")

	# Reallocate space if necessary.
	if (row > MCT_MAXROW (table)) {

	    # Compute offset of new area.
	    offset = MCT_MAXROW (table) * MCT_MAXCOL (table)

	    # Recompute new number of rows and reallocate buffer.
	    MCT_MAXROW (table) = MCT_MAXROW (table) + MCT_INCROWS (table)
	    call realloc (MCT_DATA (table), 
		MCT_MAXROW (table) * MCT_MAXCOL (table), TY_SHORT)

	    # Compute base address of new area and clear it with INDEF.
	    base = MCT_DATA (table) + offset
	    call mct_indef (table, base,
			    MCT_INCROWS (table) * MCT_MAXCOL (table))
	}

	# Update row and column counter, only if the new entries are beyond
	# the old limits.

	if (row > MCT_NPROWS (table)) {
	    MCT_NPROWS (table) = row
	    MCT_NPCOLS (table) = col
	} else if (col > MCT_NPCOLS (table))
	    MCT_NPCOLS (table) = col

	# Enter variable.
	base = MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table)
	Mems[base + col - 1] = value
end

# MCT_PUT - Put value randomly (generic)

procedure mct_puti (table, row, col, value)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number
int	value			# data value

int	offset
pointer	base
errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_put: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_put: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_INT)
	    call error (0, "mct_put: Wrong table type")

	# Test row and column values.
	if (row < 1)
	    call error (0, "mct_put: Row number less than one")
	if (col < 1 || col > MCT_MAXCOL (table))
	    call error (0, "mct_put: Column out of range")

	# Reallocate space if necessary.
	if (row > MCT_MAXROW (table)) {

	    # Compute offset of new area.
	    offset = MCT_MAXROW (table) * MCT_MAXCOL (table)

	    # Recompute new number of rows and reallocate buffer.
	    MCT_MAXROW (table) = MCT_MAXROW (table) + MCT_INCROWS (table)
	    call realloc (MCT_DATA (table), 
		MCT_MAXROW (table) * MCT_MAXCOL (table), TY_INT)

	    # Compute base address of new area and clear it with INDEF.
	    base = MCT_DATA (table) + offset
	    call mct_indef (table, base,
			    MCT_INCROWS (table) * MCT_MAXCOL (table))
	}

	# Update row and column counter, only if the new entries are beyond
	# the old limits.

	if (row > MCT_NPROWS (table)) {
	    MCT_NPROWS (table) = row
	    MCT_NPCOLS (table) = col
	} else if (col > MCT_NPCOLS (table))
	    MCT_NPCOLS (table) = col

	# Enter variable.
	base = MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table)
	Memi[base + col - 1] = value
end

# MCT_PUT - Put value randomly (generic)

procedure mct_putl (table, row, col, value)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number
long	value			# data value

int	offset
pointer	base
errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_put: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_put: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_LONG)
	    call error (0, "mct_put: Wrong table type")

	# Test row and column values.
	if (row < 1)
	    call error (0, "mct_put: Row number less than one")
	if (col < 1 || col > MCT_MAXCOL (table))
	    call error (0, "mct_put: Column out of range")

	# Reallocate space if necessary.
	if (row > MCT_MAXROW (table)) {

	    # Compute offset of new area.
	    offset = MCT_MAXROW (table) * MCT_MAXCOL (table)

	    # Recompute new number of rows and reallocate buffer.
	    MCT_MAXROW (table) = MCT_MAXROW (table) + MCT_INCROWS (table)
	    call realloc (MCT_DATA (table), 
		MCT_MAXROW (table) * MCT_MAXCOL (table), TY_LONG)

	    # Compute base address of new area and clear it with INDEF.
	    base = MCT_DATA (table) + offset
	    call mct_indef (table, base,
			    MCT_INCROWS (table) * MCT_MAXCOL (table))
	}

	# Update row and column counter, only if the new entries are beyond
	# the old limits.

	if (row > MCT_NPROWS (table)) {
	    MCT_NPROWS (table) = row
	    MCT_NPCOLS (table) = col
	} else if (col > MCT_NPCOLS (table))
	    MCT_NPCOLS (table) = col

	# Enter variable.
	base = MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table)
	Meml[base + col - 1] = value
end

# MCT_PUT - Put value randomly (generic)

procedure mct_putr (table, row, col, value)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number
real	value			# data value

int	offset
pointer	base
errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_put: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_put: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_REAL)
	    call error (0, "mct_put: Wrong table type")

	# Test row and column values.
	if (row < 1)
	    call error (0, "mct_put: Row number less than one")
	if (col < 1 || col > MCT_MAXCOL (table))
	    call error (0, "mct_put: Column out of range")

	# Reallocate space if necessary.
	if (row > MCT_MAXROW (table)) {

	    # Compute offset of new area.
	    offset = MCT_MAXROW (table) * MCT_MAXCOL (table)

	    # Recompute new number of rows and reallocate buffer.
	    MCT_MAXROW (table) = MCT_MAXROW (table) + MCT_INCROWS (table)
	    call realloc (MCT_DATA (table), 
		MCT_MAXROW (table) * MCT_MAXCOL (table), TY_REAL)

	    # Compute base address of new area and clear it with INDEF.
	    base = MCT_DATA (table) + offset
	    call mct_indef (table, base,
			    MCT_INCROWS (table) * MCT_MAXCOL (table))
	}

	# Update row and column counter, only if the new entries are beyond
	# the old limits.

	if (row > MCT_NPROWS (table)) {
	    MCT_NPROWS (table) = row
	    MCT_NPCOLS (table) = col
	} else if (col > MCT_NPCOLS (table))
	    MCT_NPCOLS (table) = col

	# Enter variable.
	base = MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table)
	Memr[base + col - 1] = value
end

# MCT_PUT - Put value randomly (generic)

procedure mct_putd (table, row, col, value)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number
double	value			# data value

int	offset
pointer	base
errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_put: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_put: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_DOUBLE)
	    call error (0, "mct_put: Wrong table type")

	# Test row and column values.
	if (row < 1)
	    call error (0, "mct_put: Row number less than one")
	if (col < 1 || col > MCT_MAXCOL (table))
	    call error (0, "mct_put: Column out of range")

	# Reallocate space if necessary.
	if (row > MCT_MAXROW (table)) {

	    # Compute offset of new area.
	    offset = MCT_MAXROW (table) * MCT_MAXCOL (table)

	    # Recompute new number of rows and reallocate buffer.
	    MCT_MAXROW (table) = MCT_MAXROW (table) + MCT_INCROWS (table)
	    call realloc (MCT_DATA (table), 
		MCT_MAXROW (table) * MCT_MAXCOL (table), TY_DOUBLE)

	    # Compute base address of new area and clear it with INDEF.
	    base = MCT_DATA (table) + offset
	    call mct_indef (table, base,
			    MCT_INCROWS (table) * MCT_MAXCOL (table))
	}

	# Update row and column counter, only if the new entries are beyond
	# the old limits.

	if (row > MCT_NPROWS (table)) {
	    MCT_NPROWS (table) = row
	    MCT_NPCOLS (table) = col
	} else if (col > MCT_NPCOLS (table))
	    MCT_NPCOLS (table) = col

	# Enter variable.
	base = MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table)
	Memd[base + col - 1] = value
end

# MCT_PUT - Put value randomly (generic)

procedure mct_putx (table, row, col, value)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number
complex	value			# data value

int	offset
pointer	base
errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_put: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_put: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_COMPLEX)
	    call error (0, "mct_put: Wrong table type")

	# Test row and column values.
	if (row < 1)
	    call error (0, "mct_put: Row number less than one")
	if (col < 1 || col > MCT_MAXCOL (table))
	    call error (0, "mct_put: Column out of range")

	# Reallocate space if necessary.
	if (row > MCT_MAXROW (table)) {

	    # Compute offset of new area.
	    offset = MCT_MAXROW (table) * MCT_MAXCOL (table)

	    # Recompute new number of rows and reallocate buffer.
	    MCT_MAXROW (table) = MCT_MAXROW (table) + MCT_INCROWS (table)
	    call realloc (MCT_DATA (table), 
		MCT_MAXROW (table) * MCT_MAXCOL (table), TY_COMPLEX)

	    # Compute base address of new area and clear it with INDEF.
	    base = MCT_DATA (table) + offset
	    call mct_indef (table, base,
			    MCT_INCROWS (table) * MCT_MAXCOL (table))
	}

	# Update row and column counter, only if the new entries are beyond
	# the old limits.

	if (row > MCT_NPROWS (table)) {
	    MCT_NPROWS (table) = row
	    MCT_NPCOLS (table) = col
	} else if (col > MCT_NPCOLS (table))
	    MCT_NPCOLS (table) = col

	# Enter variable.
	base = MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table)
	Memx[base + col - 1] = value
end

# MCT_PUT - Put value randomly (generic)

procedure mct_putp (table, row, col, value)

pointer	table			# table descriptor
int	row			# row number
int	col			# column number
pointer	value			# data value

int	offset
pointer	base
errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_put: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_put: Bad magic number")

	# Check the table type.
	if (MCT_TYPE (table) != TY_POINTER)
	    call error (0, "mct_put: Wrong table type")

	# Test row and column values.
	if (row < 1)
	    call error (0, "mct_put: Row number less than one")
	if (col < 1 || col > MCT_MAXCOL (table))
	    call error (0, "mct_put: Column out of range")

	# Reallocate space if necessary.
	if (row > MCT_MAXROW (table)) {

	    # Compute offset of new area.
	    offset = MCT_MAXROW (table) * MCT_MAXCOL (table)

	    # Recompute new number of rows and reallocate buffer.
	    MCT_MAXROW (table) = MCT_MAXROW (table) + MCT_INCROWS (table)
	    call realloc (MCT_DATA (table), 
		MCT_MAXROW (table) * MCT_MAXCOL (table), TY_POINTER)

	    # Compute base address of new area and clear it with INDEF.
	    base = MCT_DATA (table) + offset
	    call mct_indef (table, base,
			    MCT_INCROWS (table) * MCT_MAXCOL (table))
	}

	# Update row and column counter, only if the new entries are beyond
	# the old limits.

	if (row > MCT_NPROWS (table)) {
	    MCT_NPROWS (table) = row
	    MCT_NPCOLS (table) = col
	} else if (col > MCT_NPCOLS (table))
	    MCT_NPCOLS (table) = col

	# Enter variable.
	base = MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table)
	MEMP[base + col - 1] = value
end
