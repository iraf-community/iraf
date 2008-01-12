include	<tbset.h>
define	SZ_KEYWORD	64

# TABPAR -- Transfer a table element to an IRAF parameter
#
# B.Simon	17-Aug-1987	First Code
# Phil Hodge	15-May-2002	Add 'format' parameter.

procedure t_tabpar ()

pointer	table		# Name of table
pointer	column		# Name of column
int	row		# Row number of element in the table
bool	format		# Format the value using table print format?
pointer	value		# Value of table element
bool	undef		# Is table element undefined?

int	eltype
pointer	sp, hd

bool	clgetb()
int	clgeti()
pointer	tbtopn()

begin
	# Allocate storage for character strings

	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (column, SZ_COLNAME, TY_CHAR)
	call salloc (value, SZ_KEYWORD, TY_CHAR)

	# Read input parameters

	call clgstr ("table", Memc[table], SZ_FNAME)
	call clgstr ("column", Memc[column], SZ_COLNAME)
	row = clgeti ("row")
	format = clgetb ("format")

	# Read the table element as a character string

	hd = tbtopn (Memc[table], READ_ONLY, NULL)
	call gettabdat (hd, Memc[column], row, SZ_KEYWORD, format,
			Memc[value], undef, eltype)
	call tbtclo (hd)

	# Write output parameters and free string storage

	call addslash (Memc[value], SZ_KEYWORD)
	call clpstr ("value", Memc[value])
	call clputb ("undef", undef)
	call sfree (sp)
	return
end
