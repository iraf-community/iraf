# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<chars.h>
include	<ttyset.h>
include	"tty.h"

# TTYSETI -- Set a TTY parameter.  Can be used after a ttyodes to override
# termcap and environment parameters affecting terminal control.

procedure ttyseti (tty, parameter, value)

pointer	tty
int	parameter, value
char	parnum[3]
int	junk, itoc()

begin
	switch (parameter) {
	case TTY_PADCHAR:
	    T_PADCHAR(tty) = value
	case TTY_TABS:
	    T_HTOK(tty) = value
	case TTY_SOTYPE:
	    T_SOTYPE(tty) = value
	case TTY_BAUD:
	    T_BAUD(tty) = value
	case TTY_NLINES:
	    T_NLINES(tty) = value
	case TTY_NCOLS:
	    T_NCOLS(tty) = value
	default:
	    junk = itoc (parameter, parnum, 3)
	    call syserrs (SYS_TTYSET, parnum)
	}
end
