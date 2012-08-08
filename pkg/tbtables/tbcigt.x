include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbcigt -- get text info about a column
# This procedure returns information of type string about a column:
# either column name, units, or print format.  The corresponding
# routine for integer information is tbcigi.
#
# Phil Hodge,  2-Oct-1987  Subroutine created
# Phil Hodge, 14-Apr-1998  Use strcpy instead of strpak; remove pformat.

procedure tbcigt (cptr, get_what, outstr, maxch)

pointer cptr			# i: pointer to column descriptor
int	get_what		# i: indicates what string to get
char	outstr[maxch]		# o: column name, units, or print format
int	maxch			# i: maximum length of output string
#--

begin
	switch (get_what) {
	case (TBL_COL_NAME):			# get column name
	    call strcpy (COL_NAME(cptr), outstr, maxch)
	case (TBL_COL_UNITS):			# get units for column
	    call strcpy (COL_UNITS(cptr), outstr, maxch)
	case (TBL_COL_FMT):			# get print format
	    call strcpy (COL_FMT(cptr), outstr, maxch)
	default:
	    call error (ER_TBUNKPARAM, "tbcigt:  unknown parameter")
	}
end
