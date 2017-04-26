include	"filetype.h"

define	SZ_KEYWORD	64
define	USRERR		1

# KEYPAR -- Transfer header keyword to IRAF parameter
#
# B.Simon	14-Aug-87	First Code
# B.Simon	14-Dec-94	Added error checking

procedure t_keypar()

#--
pointer	input		# Name of file containing header keyword
pointer	keyword		# Name of header keyword
bool	silent		# Don't print warning message
pointer	value		# IRAF parameter value

bool	found
int	ftype, keytype, junk
pointer errtxt, sp, hd

string	unfilerr	"Header file name not found or ambiguous (%s)"

bool	clgetb()
int	filetype(), errget()
pointer	immap(), tbtopn()

begin
	# Allocate storage for character strings

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)
	call salloc (value, SZ_KEYWORD, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Read input parameters

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("keyword", Memc[keyword], SZ_KEYWORD)
	silent = clgetb ("silent")

	ftype = filetype (Memc[input])

	if (ftype == IMAGE_FILE) {

	    # Read image header keyword and get datatype

	    found = true
	    hd = immap (Memc[input], READ_ONLY, NULL)
	    iferr {
		call getimghdr (hd, Memc[keyword], SZ_KEYWORD, 
				Memc[value], keytype)
	    } then {
		junk = errget (Memc[errtxt], SZ_LINE)
		call xer_reset

		keytype = TY_CHAR
		Memc[value] = EOS
		found = false

		if (! silent) {
		    call eprintf ("Warning: %s\n")
		    call pargstr (Memc[errtxt])
		}
	    }
	    call imunmap (hd)

	} else if (ftype == TABLE_FILE) {

	    # Read table header keyword and get datatype

	    found = true
	    hd = tbtopn (Memc[input], READ_ONLY, NULL)
	    iferr {
		call gettabhdr (hd, Memc[keyword], SZ_KEYWORD, 
				Memc[value], keytype)
	    } then {
		junk = errget (Memc[errtxt], SZ_LINE)
		call xer_reset

		keytype = TY_CHAR
		Memc[value] = EOS
		found = false

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

	# Write output parameters and free string storage

	call addslash (Memc[value], SZ_KEYWORD)
	call clpstr ("value", Memc[value])
	call clputb ("found", found)
	call sfree(sp)
	return
end
