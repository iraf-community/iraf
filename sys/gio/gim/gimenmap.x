# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_ENABLEMAPPING -- Enable a previously defined mapping.  Enabling a 
# mapping does not automatically refresh the destination unless the refresh
# flag is set (refresh=YES).

procedure gim_enablemapping (gp, mapping, refresh)

pointer	gp			#I graphics descriptor
int	mapping			#I mapping to be defined or edited
int	refresh			#I refresh flag

short	gim[GIM_ENABLEMAPPING_LEN]

begin
	gim[GIM_ENABLEMAPPING_MP] = mapping
	gim[GIM_ENABLEMAPPING_FL] = refresh
	call gescape (gp, GIM_ENABLEMAPPING, gim, GIM_ENABLEMAPPING_LEN)
end
