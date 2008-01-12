# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_DISABLEMAPPING -- Disable a previously defined mapping.  Disabling a
# mapping does not automatically erase the mapping unless the erase flag
# is set.

procedure gim_disablemapping (gp, mapping, erase)

pointer	gp			#I graphics descriptor
int	mapping			#I mapping to be defined or edited
int	erase			#I erase flag

short	gim[GIM_DISABLEMAPPING_LEN]

begin
	gim[GIM_DISABLEMAPPING_MP] = mapping
	gim[GIM_DISABLEMAPPING_FL] = erase
	call gescape (gp, GIM_DISABLEMAPPING, gim, GIM_DISABLEMAPPING_LEN)
end
