# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	"gtr.h"
include	"grc.h"

# GRC_STATUS -- Called by ":.show" to print the values of the cursor mode
# parameters.

procedure grc_status (fd, rc)

int	fd			# output file
pointer	rc			# rcursor descriptor

int	ip, ch
string	keys KEYSTROKES
include	"gtr.com"

begin
	call fprintf (fd, "\tcase\t= %b\n")
	    call pargi (RC_CASE(rc))
	call fprintf (fd, "\tmarkcur\t= %b\n")
	    call pargi (RC_MARKCUR(rc))
	call fprintf (fd, "\taxes\t= %b\n")
	    call pargi (RC_AXES(rc))

	if (wstranset == YES) {
	    call fprintf (fd, "\tview\t= %5.3f %5.3f %5.3f %5.3f\n")
		call pargr (vx1)
		call pargr (vx2)
		call pargr (vy1)
		call pargr (vy2)
	} else
	    call fprintf (fd, "\tview\t= full screen\n")

	call fprintf (fd, "\tkeys\t= %s\n")
	    call pargstr (keys)
	call fprintf (fd, "\t\t->")
	    
	for (ip=1;  keys[ip] != EOS;  ip=ip+1) {
	    ch = RC_KEYS(rc,keys[ip])
	    if (ch != 0)
		call putci (fd, ch)
	    else
		call putci (fd, ' ')
	}

	call fprintf (fd, "\n")
end
