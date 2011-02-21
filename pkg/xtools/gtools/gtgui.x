# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

# GT_UIVALUES -- Send UI parameters values.

procedure gt_uivalues (gp, gt)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer

int	fd, stropen()
pointer	sp, msg, str1, str2

begin
	if (gt == NULL)
	    return

	call smark (sp)
	call salloc (msg, 20 * SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	fd = stropen (Memc[msg], 20 * SZ_LINE, WRITE_ONLY)

	# SysID
	call fprintf (fd, "%b ")
	    call pargi (GT_SYSID(gt))

	# Titles
	call fprintf (fd, "\"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" ")
	    if (GT_TITLE(gt) == NULL)
		call pargstr ("")
	    else
		call pargstr (Memc[GT_TITLE(gt)])
	    if (GT_SUBTITLE(gt) == NULL)
		call pargstr ("")
	    else
		call pargstr (Memc[GT_SUBTITLE(gt)])
	    if (GT_XLABEL(gt) == NULL)
		call pargstr ("")
	    else
		call pargstr (Memc[GT_XLABEL(gt)])
	    if (GT_XUNITS(gt) == NULL)
		call pargstr ("")
	    else
		call pargstr (Memc[GT_XUNITS(gt)])
	    if (GT_YLABEL(gt) == NULL)
		call pargstr ("")
	    else
		call pargstr (Memc[GT_YLABEL(gt)])
	    if (GT_YUNITS(gt) == NULL)
		call pargstr ("")
	    else
		call pargstr (Memc[GT_YUNITS(gt)])

	# Viewport
	call fprintf (fd, "%4.2f %4.2f %4.2f %4.2f ")
	    call pargr (GT_VXMIN(gt))
	    call pargr (GT_VXMAX(gt))
	    call pargr (GT_VYMIN(gt))
	    call pargr (GT_VYMAX(gt))

	# Window
	call fprintf (fd, "%4.2f %4.2f %4.2f %4.2f ")
	    call pargr (GT_XMIN(gt))
	    call pargr (GT_XMAX(gt))
	    call pargr (GT_YMIN(gt))
	    call pargr (GT_YMAX(gt))

	# Gtools
	call fprintf (fd, "%b %b %b %4.2f %4.2f %g %g ")
	    call pargi (GT_TRANSPOSE(gt))
	    call pargi (GT_XFLIP(gt))
	    call pargi (GT_YFLIP(gt))
	    call pargr (GT_XBUF(gt))
	    call pargr (GT_YBUF(gt))
	    call pargr (GT_LCLIP(gt))
	    call pargr (GT_HCLIP(gt))

	# Plot types
	call gt_gets (gt, GTTYPE, Memc[str1], SZ_LINE)
	call fprintf (fd, "%s %g %g %d ")
	    call pargstr (Memc[str1])
	    call pargr (GT_XSIZE(gt))
	    call pargr (GT_YSIZE(gt))
	    call pargi (GT_COLOR(gt))

	# Axes
	call gt_gets (gt, GTXTRAN, Memc[str1], SZ_LINE)
	call gt_gets (gt, GTYTRAN, Memc[str2], SZ_LINE)
	call fprintf (fd, "%s %s %g %g %s %s %b %b ")
	    switch (Memi[gt+GT_XDRAWAXES]) {
	    case 0:
		call pargstr ("none")
	    case 1:
		call pargstr ("bottom")
	    case 2:
		call pargstr ("top")
	    case 3:
		call pargstr ("both")
	    }
	    switch (Memi[gt+GT_YDRAWAXES]) {
	    case 0:
		call pargstr ("none")
	    case 1:
		call pargstr ("left")
	    case 2:
		call pargstr ("right")
	    case 3:
		call pargstr ("both")
	    }
	    call pargr (Memr[P2R(gt+GT_XAXISWIDTH)])
	    call pargr (Memr[P2R(gt+GT_YAXISWIDTH)])
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	    call pargi (Memi[gt+GT_XDRAWGRID])
	    call pargi (Memi[gt+GT_YDRAWGRID])

	# Ticks
	call fprintf (fd, "%b %b %d %d %d %d %b %b \"%s\" \"%s\" ")
	    call pargi (Memi[gt+GT_XDRAWTICKS])
	    call pargi (Memi[gt+GT_YDRAWTICKS])
	    call pargi (Memi[gt+GT_XNMAJOR])
	    call pargi (Memi[gt+GT_YNMAJOR])
	    call pargi (Memi[gt+GT_XNMINOR])
	    call pargi (Memi[gt+GT_YNMINOR])
	    call pargi (Memi[gt+GT_XLABELTICKS])
	    call pargi (Memi[gt+GT_YLABELTICKS])
	    if (GT_XFORMAT(gt) == NULL)
	       call pargstr ("")
	    else
	       call pargstr (Memc[GT_XFORMAT(gt)])
	    if (GT_YFORMAT(gt) == NULL)
	       call pargstr ("")
	    else
	       call pargstr (Memc[GT_YFORMAT(gt)])

	# Colors
	call fprintf (fd, "%d %d %d %d %d %d %d %d %d %d %d %d %d")
	    call pargi (Memi[gt+GT_FRAMECOLOR])
	    call pargi (Memi[gt+GT_TITLECOLOR])
	    call pargi (Memi[gt+GT_XGRIDCOLOR])
	    call pargi (Memi[gt+GT_YGRIDCOLOR])
	    call pargi (Memi[gt+GT_XAXISLABELCOLOR])
	    call pargi (Memi[gt+GT_YAXISLABELCOLOR])
	    call pargi (Memi[gt+GT_XAXISCOLOR])
	    call pargi (Memi[gt+GT_YAXISCOLOR])
	    call pargi (Memi[gt+GT_XTICKLABELCOLOR])
	    call pargi (Memi[gt+GT_YTICKLABELCOLOR])
	    call pargi (Memi[gt+GT_XTICKCOLOR])
	    call pargi (Memi[gt+GT_YTICKCOLOR])
	    call pargi (Memi[gt+GT_TXCOLOR])

	call strclose (fd)
	call gmsg (gp, "gtvalues", Memc[msg])

	call sfree (sp)
end
