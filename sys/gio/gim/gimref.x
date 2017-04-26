# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_REFRESHMAPPING -- Refresh a previously defined mapping, i.e., repaint
# the destination rect.

procedure gim_refreshmapping (gp, mapping)

pointer	gp			#I graphics descriptor
int	mapping			#I mapping to be defined or edited

short	gim[GIM_REFRESHMAPPING_LEN]

begin
	gim[GIM_REFRESHMAPPING_MP] = mapping
	call gescape (gp, GIM_REFRESHMAPPING, gim, GIM_REFRESHMAPPING_LEN)
end
