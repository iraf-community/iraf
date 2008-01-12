include <mach.h>
include "pdm.h"

# PDM_SORT -- Sort the phases into ascending order.

procedure pdm_sort (array, sort, numpts)

pointer	array			# array to sort
pointer	sort			# array to contain the sort indexes
int	numpts			# number of points to sort

pointer	comarray
common /sortcom/ comarray
extern	pdm_compare()

begin
	comarray = array
	if (numpts > 1)
	    call qsort (Memi[sort], numpts, pdm_compare)
end
