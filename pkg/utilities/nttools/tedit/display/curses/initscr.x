include	"../curses.h"
include	"window.h"

# INITSCR -- Initialize curses routines

procedure initscr ()

#--
include "window.com"

int	stdscr
string	cmdlist	 CMDSTR

int	newwin()

begin
	# Initialize global variables

	saved = NO
	echoed = YES
	call aclri (warray, MAXWIN)

	# Initialize terminal

	call ps_begin
	call k_begin (cmdlist)

	# Create standard screen (STDSCR)

	stdscr = newwin (GIANT, GIANT, 1, 1)
	saved = YES

end
