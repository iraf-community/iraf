# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gescape.h>

# GIM_IOMAPWRITE -- Write to the iomap.  The iomap maps client pixel values
# (colors) to gterm widget pixel value (widget colormap indices).  The iomap
# should be set only if the client application does not use the gterm widget
# color model.  iomap[i] gives the widget colormap index corresponding to
# client pixel I.

procedure gim_iomapwrite (gp, iomap, first, nelem)

pointer	gp			#I graphics descriptor
int	iomap[ARB]		#I iomap data
int	first			#I first iomap entry to be written
int	nelem			#I number of elements to write

pointer	sp, data
short	gim[GIM_WRITEIOMAP_LEN]
errchk	gpl_flush

begin
	call gpl_flush()

	call smark (sp)
	call salloc (data, nelem, TY_SHORT)

	gim[GIM_WRITEIOMAP_FC] = first
	gim[GIM_WRITEIOMAP_NC] = nelem

	call achtis (iomap, Mems[data], nelem)
	call gki_wescape (GP_FD(gp), GIM_WRITEIOMAP,
	    gim, GIM_WRITEIOMAP_LEN, Mems[data], nelem)

	call sfree (sp)
end
