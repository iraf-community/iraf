# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"tty.h"

# TTYCLEARLN -- Clear the current line.  The cursor is left positioned at the
# left margin.  If the clear has to be performed by overwriting the line with
# blanks, the final column is not cleared.

procedure ttyclearln (fd, tty)

int	fd
pointer	tty
pointer	sp, buf
int	nchars, ttygets()
errchk	salloc, ttygets, putc, ttywrite, fprintf, pargi

begin
	call smark (sp)
	call salloc (buf, SZ_CTRLSTR, TY_CHAR)

	nchars = ttygets (tty, "ce", Memc[buf], SZ_CTRLSTR)
	if (nchars > 0) {
	    call putci (fd, '\r')
	    call ttywrite (fd, tty, Memc[buf], nchars, 1)
	} else {
	    call fprintf (fd, "\r%*w\r")
		call pargi (T_NCOLS(tty) - 1)
	}

	call sfree (sp)
end
