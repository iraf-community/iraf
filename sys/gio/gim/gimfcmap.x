# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_FREECOLORMAP -- Free a colormap.

procedure gim_freecolormap (gp, colormap)

pointer	gp			#I graphics descriptor
int	colormap		#I colormap number

short	gim[GIM_FREECMAP_LEN]

begin
	gim[GIM_FREECMAP_MP] = colormap
	call gescape (gp, GIM_FREECMAP, gim, GIM_FREECMAP_LEN)
end
