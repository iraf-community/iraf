# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<chars.h>
include	<ttyset.h>
include	"tty.h"

# TTYSTATI -- Fetch a TTY parameter.

int procedure ttystati (tty, parameter)

pointer	tty
int	parameter
char	parnum[3]
int	value, junk, itoc()

begin
	switch (parameter) {
	case TTY_PADCHAR:
	    value = T_PADCHAR(tty)
	case TTY_TABS:
	    value = T_HTOK(tty)
	case TTY_SOTYPE:
	    value = T_SOTYPE(tty)
	case TTY_BAUD:
	    value = T_BAUD(tty)
	case TTY_NLINES:
	    value = T_NLINES(tty)
	case TTY_NCOLS:
	    value = T_NCOLS(tty)
	default:
	    junk = itoc (parameter, parnum, 3)
	    call syserrs (SYS_TTYSTAT, parnum)
	}

	return (value)
end
