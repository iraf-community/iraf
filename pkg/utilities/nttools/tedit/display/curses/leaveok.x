include "window.h"

# LEAVEOK -- Set the leave flag for a window
#
# B.Simon	02-Oct-90	Original

procedure leaveok (win, flag)

int	win		# i: Window descriptor
bool	flag		# i: Flag value
#--
include "window.com"

pointer	pwin
int	btoi()

begin
	pwin = warray[win]
	WIN_LEAVE(pwin) = btoi (flag)

end
