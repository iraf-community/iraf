include	<tbset.h>
include	"filetype.h"

define	SZ_KEYWORD	64
define	USRERR		1

# KEYTAB -- Transfer a header keyword to a table element
#
# B.Simon	17-Aug-87	First Code
# B.Simon	14-Dec-94	Added error checking

procedure t_keytab ()

#--
pointer	input		# Name of file containing header keyword
pointer	keyword		# Name of header keyword
pointer	table		# Name of table
pointer	column		# Name of column
int	row		# Row number of element in the table
bool	silent		# Don't print warning message

bool	undef
int	ftype, keytype, junk
pointer value, errtxt, sp, hd

string	unfilerr	"Header file name not found or ambiguous (%s)"

bool	clgetb()
int	clgeti(), filetype(), errget()
pointer	immap(), tbtopn()

begin
	# Allocate storage for character strings

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (column, SZ_COLNAME, TY_CHAR)
	call salloc (value, SZ_KEYWORD, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Read input parameters

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("keyword", Memc[keyword], SZ_KEYWORD)
	call clgstr ("table", Memc[table], SZ_FNAME)
	call clgstr ("column", Memc[column], SZ_COLNAME)
	row = clgeti ("row")
	silent = clgetb ("silent")

	undef = false
	ftype = filetype (Memc[input])

	if (ftype == IMAGE_FILE) {

	    # Read image header keyword and get datatype

	    hd = immap (Memc[input], READ_ONLY, NULL)
	    iferr {
		call getimghdr (hd, Memc[keyword], SZ_KEYWORD, 
				Memc[value], keytype)
	    } then {
		junk = errget (Memc[errtxt], SZ_LINE)
		call xer_reset
		undef = true

		if (! silent) {
		    call eprintf ("Warning: %s\n")
		    call pargstr (Memc[errtxt])
		}
	    }
	    call imunmap (hd)

	} else if (ftype == TABLE_FILE) {

	    # Read table header keyword and get datatype

	    hd = tbtopn (Memc[input], READ_ONLY, NULL)
	    iferr {
		call gettabhdr (hd, Memc[keyword], SZ_KEYWORD, 
				Memc[value], keytype)
	    } then {
		junk = errget (Memc[errtxt], SZ_LINE)
		call xer_reset
		undef = true

		if (! silent) {
		    call eprintf ("Warning: %s\n")
		    call pargstr (Memc[errtxt])
		}
	    }
	    call tbtclo (hd)

	} else {

	    call sprintf (Memc[errtxt], SZ_LINE, unfilerr)
	    call pargstr (Memc[input])
	    call error (USRERR, Memc[errtxt])

	}

	# Write the table element according to its datatype

	hd = tbtopn (Memc[table], READ_WRITE, NULL)
	call puttabdat (hd, Memc[column], row, Memc[value], undef, keytype)
	call tbtclo (hd)

	# Free string storage

	call sfree (sp)
	return
end
