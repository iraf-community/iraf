include <ctype.h>
include <tbset.h>
include "tblerr.h"

# tbtext -- append default extension
# If the input table name inname already has an extension then inname is
# just copied to the output string outname; otherwise, outname will be
# assigned the input name plus the default extension ".tab".
# The input and output strings may be the same.
#
# An extension is defined by a '.' that is not followed by anything other
# than alphanumeric characters.  A file name that ends in '.' is regarded
# as having an extension, and nothing will be appended.
#
# Phil Hodge,  7-Aug-1987  Check whether table name is too long.
# Phil Hodge, 20-Mar-1995  Call tbparse.
# Phil Hodge, 30-Sep-1997  Look for extension in Memc[name] instead of inname.
# Phil Hodge, 15-Jun-1998  Use ttype from tbparse to check for a text file or
#			FITS file, and don't append extension in that case.
# Phil Hodge, 12-Apr-1999  Call tbttyp to get file type;
#		remove table type from calling sequence of tbparse;
#		use strlen(defext) instead of LEN_EXT;
#		change SZ_LINE to SZ_FNAME.

procedure tbtext (inname, outname, maxch)

char	inname[ARB]		# i: table name, possibly without extension
char	outname[ARB]		# o: table name, including extension
int	maxch			# i: max number of char in inname or outname
#--
pointer sp
pointer name			# pointer to scratch for name
pointer brackets		# bracketed expression at end of inname
int	hdu			# returned by tbparse and ignored
int	ttype, exists		# returned by tbttyp; exists is ignored
int	dotloc			# location of last '.' in file name
int	k			# loop index
bool	no_change		# true if table name is OK as is
string	defext ".tab"		# the default extension for a table
int	tbttyp()
int	strlen(), locva()
int	tbparse()
errchk	tbparse

begin
	if (strlen(inname) > maxch)
	    call error (ER_TBNAMTOOLONG, "table name is too long")

	call smark (sp)
	call salloc (name, maxch, TY_CHAR)
	call salloc (brackets, SZ_FNAME, TY_CHAR)

	# Extract file name from inname in case inname includes a bracketed
	# expression.
	# The file name is Memc[name], and the extname is Memc[brackets].
	if (tbparse (inname, Memc[name], Memc[brackets], SZ_FNAME, hdu) < 1)
	    call error (1, "no table name given")

	# Get the table type.
	ttype = tbttyp (Memc[name], exists)

	# Check whether we need to append an extension.
	no_change = false		# initial value
	if (ttype == TBL_TYPE_TEXT) {
	    no_change = true
	} else if (ttype == TBL_TYPE_FITS) {
	    no_change = true
	} else {
	    # Search for a dot that is not followed by '$' or ']'.
	    dotloc = 0				# initial value
	    do k = 1, maxch {
		if (Memc[name+k-1] == EOS)
		    break

		if (Memc[name+k-1] == '.')
		    dotloc = k
		else if (!IS_ALNUM(Memc[name+k-1]))
		    dotloc = 0			# reset following special char
	    }
	    if (dotloc > 0)
		no_change = true		# already has an extension
	}

	if (no_change) {

	    # Return the unmodified input name.
	    if (locva (inname) != locva (outname))
		call strcpy (inname, outname, maxch)

	} else {

	    # Append default extension if there is room for it.
	    if (strlen(inname) + strlen(defext) > maxch)
		call error (ER_TBNAMTOOLONG, "table name is too long")
	    call strcat (defext, Memc[name], maxch)
	    call strcpy (Memc[name], outname, maxch)	# copy to output
	    call strcat (Memc[brackets], outname, maxch)
	}
	call sfree (sp)
end
