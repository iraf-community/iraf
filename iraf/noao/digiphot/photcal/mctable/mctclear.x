include	"../lib/mctable.h"



# MCT_CLEAR - Clear all table values with given value. Do not reset any
# table counter.

procedure mct_clearc (table, value)

pointer	table			# table descriptor
char	value			# value

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_clear: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_clear: Bad magic number")

	# Check table type
	if (MCT_TYPE (table) != TY_CHAR)
	    call error (0, "mct_clear: Wrong table type")

	# Move value to data buffer
	call amovkc (value, Memc[MCT_DATA (table)],
		     MCT_MAXROW (table) * MCT_MAXCOL (table))
end

# MCT_CLEAR - Clear all table values with given value. Do not reset any
# table counter.

procedure mct_clears (table, value)

pointer	table			# table descriptor
short	value			# value

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_clear: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_clear: Bad magic number")

	# Check table type
	if (MCT_TYPE (table) != TY_SHORT)
	    call error (0, "mct_clear: Wrong table type")

	# Move value to data buffer
	call amovks (value, Mems[MCT_DATA (table)],
		     MCT_MAXROW (table) * MCT_MAXCOL (table))
end

# MCT_CLEAR - Clear all table values with given value. Do not reset any
# table counter.

procedure mct_cleari (table, value)

pointer	table			# table descriptor
int	value			# value

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_clear: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_clear: Bad magic number")

	# Check table type
	if (MCT_TYPE (table) != TY_INT)
	    call error (0, "mct_clear: Wrong table type")

	# Move value to data buffer
	call amovki (value, Memi[MCT_DATA (table)],
		     MCT_MAXROW (table) * MCT_MAXCOL (table))
end

# MCT_CLEAR - Clear all table values with given value. Do not reset any
# table counter.

procedure mct_clearl (table, value)

pointer	table			# table descriptor
long	value			# value

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_clear: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_clear: Bad magic number")

	# Check table type
	if (MCT_TYPE (table) != TY_LONG)
	    call error (0, "mct_clear: Wrong table type")

	# Move value to data buffer
	call amovkl (value, Meml[MCT_DATA (table)],
		     MCT_MAXROW (table) * MCT_MAXCOL (table))
end

# MCT_CLEAR - Clear all table values with given value. Do not reset any
# table counter.

procedure mct_clearr (table, value)

pointer	table			# table descriptor
real	value			# value

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_clear: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_clear: Bad magic number")

	# Check table type
	if (MCT_TYPE (table) != TY_REAL)
	    call error (0, "mct_clear: Wrong table type")

	# Move value to data buffer
	call amovkr (value, Memr[MCT_DATA (table)],
		     MCT_MAXROW (table) * MCT_MAXCOL (table))
end

# MCT_CLEAR - Clear all table values with given value. Do not reset any
# table counter.

procedure mct_cleard (table, value)

pointer	table			# table descriptor
double	value			# value

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_clear: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_clear: Bad magic number")

	# Check table type
	if (MCT_TYPE (table) != TY_DOUBLE)
	    call error (0, "mct_clear: Wrong table type")

	# Move value to data buffer
	call amovkd (value, Memd[MCT_DATA (table)],
		     MCT_MAXROW (table) * MCT_MAXCOL (table))
end

# MCT_CLEAR - Clear all table values with given value. Do not reset any
# table counter.

procedure mct_clearx (table, value)

pointer	table			# table descriptor
complex	value			# value

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_clear: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_clear: Bad magic number")

	# Check table type
	if (MCT_TYPE (table) != TY_COMPLEX)
	    call error (0, "mct_clear: Wrong table type")

	# Move value to data buffer
	call amovkx (value, Memx[MCT_DATA (table)],
		     MCT_MAXROW (table) * MCT_MAXCOL (table))
end

# MCT_CLEAR - Clear all table values with given value. Do not reset any
# table counter.

procedure mct_clearp (table, value)

pointer	table			# table descriptor
pointer	value			# value

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_clear: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_clear: Bad magic number")

	# Check table type
	if (MCT_TYPE (table) != TY_POINTER)
	    call error (0, "mct_clear: Wrong table type")

	# Move value to data buffer
	call amovki (value, Memi[MCT_DATA (table)],
		     MCT_MAXROW (table) * MCT_MAXCOL (table))
end
