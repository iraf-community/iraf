include <mach.h>
include "pdm.h"

# Compare procedure for qsort.

int procedure pdm_compare (item1, item2)

int	item1		# index of first phase
int	item2		# index of second phase

pointer	comarray
common /sortcom/ comarray
double	p1, p2

begin
	p1 = Memd[comarray+item1-1]
	p2 = Memd[comarray+item2-1]

	if (p1 > p2)
	    return (1)
	if (p1 == p2)
	    return (0)
	if (p1 < p2)
	    return (-1)
end
