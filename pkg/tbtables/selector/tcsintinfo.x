include "tcs.h"

# TCS_INTINFO -- Integer information about a column

int procedure tcs_intinfo (descrip, what)

pointer	descrip		# i: column selector
int	what		# i: parameter
#--
int	tbcigi()

begin
	return (tbcigi (TCS_COLUMN(descrip), what))
end
