include <imhdr.h>
include "whatfile.h"

# WHATFILE -- Return integer code indicating type of image
#
# B. Simon,                Original.
# Phil Hodge, 22-Feb-2002  Use tbtacc instead of tbtopn to test for a table.
# Phil Hodge, 19-Sep-2002  Simplify is_image(), just use tbtacc or imaccess.


int procedure whatfile (file)

char	file[ARB]	# i: file name
#--
int	flag

int	is_image()

begin
	# This function exists mostly for backwards compatibility. 
	# The recommended function to use is is_image, as it does
	# not need special macros

	switch (is_image(file)) {
	case ERR:
	    flag = IS_UNKNOWN
	case NO:
	    flag = IS_TABLE
	case YES:
	    flag = IS_IMAGE
	}

	return (flag)
end

# IS_IMAGE -- Return YES if file is image, NO if table, and ERR if can't decide
#
# Note that a function value of NO does not just mean that the file is not
# an image, it means that it _is_ a table.  Note also that while a FITS
# primary header or IMAGE extension can be opened as a table (for access
# to the header), tbtacc will return NO for such an extension.
#
# The 'file' argument to this function should be the complete image or
# table name, i.e. including FITS extension number or name, or STF group
# number.

int procedure is_image (file)

char	file[ARB]	# i: file name
#--
int	image
int	imaccess(), tbtacc()

begin
	if (tbtacc (file) == YES)
	    image = NO
	else if (imaccess (file, READ_ONLY) == YES)
	    image = YES
	else
	    image = ERR

	return image
end
