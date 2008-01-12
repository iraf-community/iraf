include "window.h"

# SCROLLOK -- Set the scroll flag for a window
#
# B.Simon	02-Oct-90	Original

procedure scrollok (win, flag)

int	win		# i: Window descriptor
bool	flag		# i: Flag value
#--
include "window.com"

pointer	pwin
int	btoi()

begin
	pwin = warray[win]
	WIN_SCROLL(pwin) = btoi (flag)

end
