# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"tty.h"

# TTYCTRL -- Output a control sequence to the terminal.

int procedure ttyctrl (fd, tty, cap, afflncnt)

int	fd, afflncnt
char	cap[ARB]
pointer	tty

pointer	sp, buf
int	status, nchars, ttygets()
errchk	ttygets, ttywrite

begin
	call smark (sp)
	call salloc (buf, SZ_CTRLSTR, TY_CHAR)

	nchars = ttygets (tty, cap, Memc[buf], SZ_CTRLSTR)
	if (nchars > 0) {
	    call ttywrite (fd, tty, Memc[buf], nchars, afflncnt)
	    status = OK
	} else
	    status = ERR

	call sfree (sp)
	return (status)
end
