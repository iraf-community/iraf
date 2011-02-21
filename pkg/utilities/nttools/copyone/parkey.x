include	"filetype.h"

define	SZ_KEYWORD	64
define	USRERR		1

# PARKEY -- Transfer IRAF parameter to header keyword
#
# B.Simon	14-Aug-87	First Code

procedure t_parkey()

pointer	value		# IRAF parameter value
pointer	output		# Name of file containing header keyword
pointer	keyword		# Name of header keyword
bool	add		# Is it OK to add a new keyword?

int	ftype, keytype
pointer errtxt, sp, hd

string	unfilerr	"Header file name not found or ambiguous (%s)"

bool	clgetb()
int	filetype(), datatype()
pointer	immap(), tbtopn()

begin
	# Allocate storage for character strings

	call smark (sp)
	call salloc (value, SZ_KEYWORD, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Read parameters

	call clgstr ("value", Memc[value], SZ_KEYWORD)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("keyword", Memc[keyword], SZ_KEYWORD)
	add = clgetb("add")

	ftype = filetype (Memc[output])
	keytype = datatype (Memc[value])

	if (ftype == IMAGE_FILE) {

	    # Write image header keyword

	    hd = immap (Memc[output], READ_WRITE, NULL)
	    call putimghdr (hd, Memc[keyword], Memc[value], keytype, add)
	    call imunmap (hd)

	} else if (ftype == TABLE_FILE) {

	    # Write table header keyword

	    hd = tbtopn (Memc[output], READ_WRITE, NULL)
	    call puttabhdr (hd, Memc[keyword], Memc[value], keytype, add)
	    call tbtclo (hd)

	} else {

	    call sprintf (Memc[errtxt], SZ_LINE, unfilerr)
	    call pargstr (Memc[output])
	    call error (USRERR, Memc[errtxt])

	}

	call sfree(sp)
	return
end
