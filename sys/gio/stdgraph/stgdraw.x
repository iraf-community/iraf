# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_DRAW -- Output a device draw line-segment instruction to draw from the
# current position to the position (x,y) in GKI coordinates.

procedure stg_draw (x, y)

int	x, y			# destination
int	stg_encode()
include	"stdgraph.com"

begin
	# Transform the first point from GKI coords to device coords and
	# draw to the transformed point.

	call ttyputs (g_out, g_tty, Memc[SG_STARTDRAW(g_sg)], 1)

	g_reg[1] = x * g_dx + g_x1
	g_reg[2] = y * g_dy + g_y1
	g_reg[E_IOP] = 1
	if (stg_encode (Memc[g_xy], g_mem, g_reg) == OK)
	    call write (g_out, g_mem, g_reg[E_IOP] - 1)
	
	call ttyputs (g_out, g_tty, Memc[SG_ENDDRAW(g_sg)], 1)
end
