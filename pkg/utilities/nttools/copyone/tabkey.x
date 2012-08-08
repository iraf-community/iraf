include	<tbset.h>
include	"filetype.h"
define	SZ_KEYWORD	64
define	USRERR		1

# TABKEY -- Transfer a table element to a header keyword
#
# B.Simon	17-Aug-87	First Code
# B.Simon	24-Jan-92	Added salloc for errtxt
# Phil Hodge	15-May-2002	Add 'format' argument to gettabdat.

procedure t_tabkey ()

pointer	table		# Name of table
pointer	column		# Name of column
int	row		# Row number of element in the table
pointer	output		# Name of file containing header keyword
pointer	keyword		# Name of header keyword
bool	add		# Is it OK to add a new keyword?

bool	undef
bool	format		# Format the value using table print format?
int	ftype, eltype
pointer	sp, hd, value, errtxt

string	undeferr	"Table element is undefined"
string	unfilerr	"Header file name not found or ambiguous (%s)"

bool	clgetb()
int	clgeti(), filetype()
pointer	immap(), tbtopn()

begin
	# Allocate storage for character strings

	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (column, SZ_COLNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)
	call salloc (value, SZ_KEYWORD, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR) # Added (BPS 01.24.92)

	# Read input parameters

	call clgstr ("table", Memc[table], SZ_FNAME)
	call clgstr ("column", Memc[column], SZ_COLNAME)
	row = clgeti ("row")
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("keyword", Memc[keyword], SZ_KEYWORD)
	add = clgetb("add")

	# Read the table element as a character string

	format = false
	hd = tbtopn (Memc[table], READ_ONLY, NULL)
	call gettabdat (hd, Memc[column], row, SZ_KEYWORD, format,
			Memc[value], undef, eltype)
	call tbtclo (hd)

	# It is an error to try to write an undefined value to the header

	if (undef)
	    call error (USRERR, undeferr)

	ftype = filetype (Memc[output])

	if (ftype == IMAGE_FILE) {

	    # Write image header keyword

	    hd = immap (Memc[output], READ_WRITE, NULL)
	    call putimghdr (hd, Memc[keyword], Memc[value], eltype, add)
	    call imunmap (hd)

	} else if (ftype == TABLE_FILE) {

	    # Write table header keyword

	    hd = tbtopn (Memc[output], READ_WRITE, NULL)
	    call puttabhdr (hd, Memc[keyword], Memc[value], eltype, add)
	    call tbtclo (hd)

	} else {

	    call sprintf (Memc[errtxt], SZ_LINE, unfilerr)
	    call pargstr (Memc[output])
	    call error (USRERR, Memc[errtxt])

	}

	call sfree(sp)
	return
end
