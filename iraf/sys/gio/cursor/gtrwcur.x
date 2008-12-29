# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

# GTR_WRITECURSOR -- Write the graphics cursor position in NDC coordinates.

procedure gtr_writecursor (fd, x, y)

int	fd			# graphics stream
real	x, y			# NDC coords of cursor

int	mx, my
int	inint()

begin
	mx = max(0, min(GKI_MAXNDC, inint (x * GKI_MAXNDC)))
	my = max(0, min(GKI_MAXNDC, inint (y * GKI_MAXNDC)))

	call gki_setcursor (fd, mx, my, 0)
end
