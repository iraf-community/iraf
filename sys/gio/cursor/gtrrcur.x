# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

# GTR_READCURSOR -- Read the graphics cursor position in NDC coordinates.
# By the time we are called the plot has already been drawn and the
# workstation closed, hence we must reopen the workstation to read the
# cursor (the graphics terminal will not be in graphics mode otherwise).

int procedure gtr_readcursor (fd, x, y, key)

int	fd			# graphics stream
real	x, y			# NDC coords of cursor
int	key			# keystroke value

int	mx, my

begin
	call gki_getcursor (fd, mx, my, key, 0)

	x = real(mx) / GKI_MAXNDC
	y = real(my) / GKI_MAXNDC

	return (key)
end
