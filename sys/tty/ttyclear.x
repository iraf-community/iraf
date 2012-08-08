# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"tty.h"

# TTYCLEAR -- Clear the terminal screen.

procedure ttyclear (fd, tty)

int	fd
pointer	tty

int	status
bool	ttygetb()
int	ttyctrl()
errchk	ttygetb, ttyctrl

begin
	# If hardcopy terminal, output formfeed instead of clear.
	if (ttygetb (tty, "ht"))
	    status = ttyctrl (fd, tty, "ff", T_NLINES(tty))
	else
	    status = ERR
	if (status == ERR)
	    status = ttyctrl (fd, tty, "cl", T_NLINES(tty))

	# If ff or cl capability not found, the best we can do is output
	# a newline.
	if (status == ERR)
	    call putline (fd, "\n")
end
