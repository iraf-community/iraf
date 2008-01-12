include	<tbset.h>
define	SZ_KEYWORD	64

# PARTAB -- Transfer an IRAF parameter value to a table element
#
# B.Simon	17-Aug-87	First Code

procedure t_partab ()

pointer	value		# Value of table element
pointer	table		# Name of table
pointer	column		# Name of column
int	row		# Row number of element in the table

bool	undef
int	eltype
pointer	sp, hd

bool	streq()
int	clgeti(), datatype()
pointer	tbtopn()

begin
	# Allocate storage for character strings

	call smark (sp)
	call salloc (value, SZ_KEYWORD, TY_CHAR)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (column, SZ_COLNAME, TY_CHAR)

	# Read input parameters

	call clgstr ("value", Memc[value], SZ_KEYWORD)
	call clgstr ("table", Memc[table], SZ_FNAME)
	call clgstr ("column", Memc[column], SZ_COLNAME)
	row = clgeti ("row")

	eltype = datatype (Memc[value])
	undef = streq (Memc[value], "INDEF")

	# Write the table element according to its datatype

	hd = tbtopn (Memc[table], READ_WRITE, NULL)
	call puttabdat (hd, Memc[column], row, Memc[value], undef, eltype)
	call tbtclo (hd)

	# Free string storage

	call sfree (sp)
	return
end
