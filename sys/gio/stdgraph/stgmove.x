# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_MOVE -- Output a device move instruction to move to the position (x,y)
# in GKI coordinates.

procedure stg_move (x, y)

int	x, y			# destination
int	stg_encode()
include	"stdgraph.com"

begin
	# Transform the first point from GKI coords to device coords and
	# move to the transformed point.

	call ttyputs (g_out, g_tty, Memc[SG_STARTMOVE(g_sg)], 1)

	g_reg[1] = x * g_dx + g_x1
	g_reg[2] = y * g_dy + g_y1
	g_reg[E_IOP] = 1
	if (stg_encode (Memc[g_xy], g_mem, g_reg) == OK)
	    call write (g_out, g_mem, g_reg[E_IOP] - 1)
	
	call ttyputs (g_out, g_tty, Memc[SG_ENDMOVE(g_sg)], 1)
end
