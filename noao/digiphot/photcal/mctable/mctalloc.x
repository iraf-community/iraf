include	"../lib/mctable.h"


# MCT_ALLOC - Allocate table space, reset all table counters, and clear
# table values to INDEF.

procedure mct_alloc (table, nrows, ncols, type)

pointer	table			# table descriptor (output)
int	nrows			# number of rows
int	ncols			# number of columns
int	type			# data type

errchk	mct_reset()

begin
	# Test number of rows and colums.
	if (nrows < 1)
	    call error (0, "mct_alloc: Zero or negative rows")
	if (ncols < 1)
	    call error (0, "mct_alloc: Zero or negative columns")

	# Check for supported data types.
	if (type != TY_CHAR &&
	    type != TY_SHORT && type != TY_INT && type != TY_LONG &&
	    type != TY_REAL && type != TY_DOUBLE &&
	    type != TY_COMPLEX &&
	    type != TY_POINTER)
	    call error (0, "mct_alloc: Unknown type")

	# Allocate table structure and initialize it. The only
	# value that can change in the future is the maximum number
	# of rows. All others will remain constant.

	call malloc (table, LEN_MCTABLE, TY_STRUCT)
	MCT_MAGIC   (table) = MAGIC
	MCT_TYPE    (table) = type
	MCT_MAXROW  (table) = nrows
	MCT_MAXCOL  (table) = ncols
	MCT_INCROWS (table) = GROWFACTOR (nrows)

	# Allocate data buffer and undefine it.
	call malloc (MCT_DATA (table), nrows * ncols, type)
	call mct_indef (table, MCT_DATA (table), nrows * ncols)

	# Reset table.
	call mct_reset (table)
end
