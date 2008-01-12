include <tbset.h>
include "tbtables.h"

# tbtren -- rename a table
# This procedure renames a table from inname to outname.  The default
# extension will be appended if an extension is not present and the input
# file is a binary table.  If intable is a text file, no extension will be
# appended to outtable.
#
# Phil Hodge, 28-Dec-1989  Open before renaming to verify that it is a table.
# Phil Hodge, 14-May-1992  Check for text table; call tbtext only if binary.
# Phil Hodge, 26-Jun-1995  Modify for FITS file or CDF file.

procedure tbtren (intable, outtable)

char	intable[ARB]	# i: name of table to be renamed to outtable
char	outtable[ARB]	# i: new name of table
#--
errchk	tbtcpy, tbtdel

begin
	# For the time being, use tbtcpy and tbtdel.  After the CDF
	# interface is available, copy code from tbtcpy and tbtdel
	# to here so we don't open the input table twice.

	call tbtcpy (intable, outtable)
	call tbtdel (intable)
end
