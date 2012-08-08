# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_FREEMAPPING -- Free a mapping.

procedure gim_freemapping (gp, mapping)

pointer	gp			#I graphics descriptor
int	mapping			#I mapping number

short	gim[GIM_FREEMAPPING_LEN]

begin
	gim[GIM_FREEMAPPING_MP] = mapping
	call gescape (gp, GIM_FREEMAPPING, gim, GIM_FREEMAPPING_LEN)
end
