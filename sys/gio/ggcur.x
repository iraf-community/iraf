# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gio.h>

# GGCUR -- Get the current graphics cursor position (x,y) in NDC coordinates.

int procedure ggcur (gp, sx, sy, key)

pointer gp			# graphics descriptor
real	sx, sy			# cursor position in NDC coordinates (output)
int	key			# key typed or EOF
int	mx, my

begin
	call gflush (gp)
	call gki_getcursor (GP_FD(gp), mx, my, key, GP_CURSOR(gp))

	sx = real(mx) / GKI_MAXNDC
	sy = real(my) / GKI_MAXNDC

	return (key)
end
