include	"../lib/mctable.h"



# MCT_SGET - Get value sequentally (generic)

int procedure mct_sgetc (table, value)

pointer	table			# table descriptor
char	value			# data value (output)

int	row, col		# next row, and column
char	mct_getc()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_sget: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_sget: Bad magic number")
	
	# Check table type.
	if (MCT_TYPE (table) != TY_CHAR)
	    call error (0, "mct_sget: Wrong table type")

	# Get next position.
	row = max (MCT_NGROWS (table), 1)
	col = MCT_NGCOLS (table) + 1

	# Test if it's necessary to go to the next row.
	if (col > MCT_MAXCOL (table)) {
	    col	= 1
	    row = row + 1
	}

	# Get value and return status.
	iferr (value = mct_getc (table, row, col))
	    return (EOF)
	else
	    return (OK)
end

# MCT_SGET - Get value sequentally (generic)

int procedure mct_sgets (table, value)

pointer	table			# table descriptor
short	value			# data value (output)

int	row, col		# next row, and column
short	mct_gets()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_sget: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_sget: Bad magic number")
	
	# Check table type.
	if (MCT_TYPE (table) != TY_SHORT)
	    call error (0, "mct_sget: Wrong table type")

	# Get next position.
	row = max (MCT_NGROWS (table), 1)
	col = MCT_NGCOLS (table) + 1

	# Test if it's necessary to go to the next row.
	if (col > MCT_MAXCOL (table)) {
	    col	= 1
	    row = row + 1
	}

	# Get value and return status.
	iferr (value = mct_gets (table, row, col))
	    return (EOF)
	else
	    return (OK)
end

# MCT_SGET - Get value sequentally (generic)

int procedure mct_sgeti (table, value)

pointer	table			# table descriptor
int	value			# data value (output)

int	row, col		# next row, and column
int	mct_geti()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_sget: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_sget: Bad magic number")
	
	# Check table type.
	if (MCT_TYPE (table) != TY_INT)
	    call error (0, "mct_sget: Wrong table type")

	# Get next position.
	row = max (MCT_NGROWS (table), 1)
	col = MCT_NGCOLS (table) + 1

	# Test if it's necessary to go to the next row.
	if (col > MCT_MAXCOL (table)) {
	    col	= 1
	    row = row + 1
	}

	# Get value and return status.
	iferr (value = mct_geti (table, row, col))
	    return (EOF)
	else
	    return (OK)
end

# MCT_SGET - Get value sequentally (generic)

int procedure mct_sgetl (table, value)

pointer	table			# table descriptor
long	value			# data value (output)

int	row, col		# next row, and column
long	mct_getl()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_sget: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_sget: Bad magic number")
	
	# Check table type.
	if (MCT_TYPE (table) != TY_LONG)
	    call error (0, "mct_sget: Wrong table type")

	# Get next position.
	row = max (MCT_NGROWS (table), 1)
	col = MCT_NGCOLS (table) + 1

	# Test if it's necessary to go to the next row.
	if (col > MCT_MAXCOL (table)) {
	    col	= 1
	    row = row + 1
	}

	# Get value and return status.
	iferr (value = mct_getl (table, row, col))
	    return (EOF)
	else
	    return (OK)
end

# MCT_SGET - Get value sequentally (generic)

int procedure mct_sgetr (table, value)

pointer	table			# table descriptor
real	value			# data value (output)

int	row, col		# next row, and column
real	mct_getr()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_sget: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_sget: Bad magic number")
	
	# Check table type.
	if (MCT_TYPE (table) != TY_REAL)
	    call error (0, "mct_sget: Wrong table type")

	# Get next position.
	row = max (MCT_NGROWS (table), 1)
	col = MCT_NGCOLS (table) + 1

	# Test if it's necessary to go to the next row.
	if (col > MCT_MAXCOL (table)) {
	    col	= 1
	    row = row + 1
	}

	# Get value and return status.
	iferr (value = mct_getr (table, row, col))
	    return (EOF)
	else
	    return (OK)
end

# MCT_SGET - Get value sequentally (generic)

int procedure mct_sgetd (table, value)

pointer	table			# table descriptor
double	value			# data value (output)

int	row, col		# next row, and column
double	mct_getd()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_sget: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_sget: Bad magic number")
	
	# Check table type.
	if (MCT_TYPE (table) != TY_DOUBLE)
	    call error (0, "mct_sget: Wrong table type")

	# Get next position.
	row = max (MCT_NGROWS (table), 1)
	col = MCT_NGCOLS (table) + 1

	# Test if it's necessary to go to the next row.
	if (col > MCT_MAXCOL (table)) {
	    col	= 1
	    row = row + 1
	}

	# Get value and return status.
	iferr (value = mct_getd (table, row, col))
	    return (EOF)
	else
	    return (OK)
end

# MCT_SGET - Get value sequentally (generic)

int procedure mct_sgetx (table, value)

pointer	table			# table descriptor
complex	value			# data value (output)

int	row, col		# next row, and column
complex	mct_getx()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_sget: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_sget: Bad magic number")
	
	# Check table type.
	if (MCT_TYPE (table) != TY_COMPLEX)
	    call error (0, "mct_sget: Wrong table type")

	# Get next position.
	row = max (MCT_NGROWS (table), 1)
	col = MCT_NGCOLS (table) + 1

	# Test if it's necessary to go to the next row.
	if (col > MCT_MAXCOL (table)) {
	    col	= 1
	    row = row + 1
	}

	# Get value and return status.
	iferr (value = mct_getx (table, row, col))
	    return (EOF)
	else
	    return (OK)
end

# MCT_SGET - Get value sequentally (generic)

int procedure mct_sgetp (table, value)

pointer	table			# table descriptor
pointer	value			# data value (output)

int	row, col		# next row, and column
pointer	mct_getp()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_sget: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_sget: Bad magic number")
	
	# Check table type.
	if (MCT_TYPE (table) != TY_POINTER)
	    call error (0, "mct_sget: Wrong table type")

	# Get next position.
	row = max (MCT_NGROWS (table), 1)
	col = MCT_NGCOLS (table) + 1

	# Test if it's necessary to go to the next row.
	if (col > MCT_MAXCOL (table)) {
	    col	= 1
	    row = row + 1
	}

	# Get value and return status.
	iferr (value = mct_getp (table, row, col))
	    return (EOF)
	else
	    return (OK)
end
