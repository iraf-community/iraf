# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

define	SZ_CCSTR		5

# PUTCC -- Put a character to a file.  This procedure is identical to PUTC,
# except that nonprintable characters are rendered as escape sequences.

procedure putcc (fd, ch)

int	fd
char	ch
char	ccstr[SZ_CCSTR]
int	ip, n, ctocc()

begin
	if (IS_PRINT (ch))
	    call putc (fd, ch)
	else {
	    n = ctocc (ch, ccstr, SZ_CCSTR)
	    do ip = 1, n
		call putc (fd, ccstr[ip])
	}
end
