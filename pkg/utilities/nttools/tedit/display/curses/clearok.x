include "window.h"

# CLEAROK -- Set the clear flag for a window
#
# B.Simon	01-Oct-90	Original

procedure clearok (win, flag)

int	win		# i: Window descriptor
bool	flag		# i: Flag value
#--
include "window.com"

pointer	pwin
int	btoi()

begin
	pwin = warray[win]
	WIN_CLEAR(pwin) = btoi (flag)

end
