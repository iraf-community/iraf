# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gescape.h>

# GIM_WRITECOLORMAP -- Write to a colormap.

procedure gim_writecolormap (gp, colormap, first, nelem, r, g, b)

pointer	gp			#I graphics descriptor
int	colormap		#I colormap number (0=screen)
int	first			#I first colormap entry to be written
int	nelem			#I number of elements to write
int	r[ARB],g[ARB],b[ARB]	#I RGB color values (0-255)

int	i
pointer	sp, cm, op
short	gim[GIM_WRITECMAP_LEN]
errchk	gpl_flush

begin
	call gpl_flush()

	call smark (sp)
	call salloc (cm, nelem * 3, TY_SHORT)

	gim[GIM_WRITECMAP_MP] = colormap
	gim[GIM_WRITECMAP_FC] = first
	gim[GIM_WRITECMAP_NC] = nelem

	do i = 1, nelem {
	    op = cm + (i - 1) * 3
	    Mems[op+0] = r[i]
	    Mems[op+1] = g[i]
	    Mems[op+2] = b[i]
	}

	call gki_wescape (GP_FD(gp), GIM_WRITECMAP,
	    gim, GIM_WRITECMAP_LEN, Mems[cm], nelem * 3)

	call sfree (sp)
end
