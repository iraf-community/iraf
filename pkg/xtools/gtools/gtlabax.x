# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<gset.h>
include	<gio.h>
include	"gtools.h"
	
# GT_LABAX -- Set graphics axis.

procedure gt_labax (gp, gt)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer

int	nl, len
real	vx1, vx2, vy1, vy2, wx1, wx2, wy1, wy2
pointer	title, xlabel, ylabel

int	strlen()

begin
	if (gt != NULL) {
	    call gt_reset (gp, gt)

	    call ggview (gp, vx1, vx2, vy1, vy2)
	    if (!IS_INDEF(GT_VXMIN(gt)))
		vx1 = GT_VXMIN(gt)
	    if (!IS_INDEF(GT_VXMAX(gt)))
		vx2 = GT_VXMAX(gt)
	    if (!IS_INDEF(GT_VYMIN(gt)))
		vy1 = GT_VYMIN(gt)
	    if (!IS_INDEF(GT_VYMAX(gt)))
		vy2 = GT_VYMAX(gt)
	    call gsview (gp, vx1, vx2, vy1, vy2)

	    call malloc (title, SZ_LINE, TY_CHAR)
	    len = SZ_LINE
	    Memc[title] = EOS
	    if (GT_DRWTITLE(gt) == YES) {
		nl = NO
		if (GT_SYSID(gt) == YES) {
		    call sysid (Memc[title], len)
		    len = len + strlen (Memc[title]) + 1
		    call realloc (title, len, TY_CHAR)
		    nl = YES 
		}
		if (GT_PARAMS(gt) != NULL) {
		    len = len + strlen (Memc[GT_PARAMS(gt)]) + 1
		    call realloc (title, len, TY_CHAR)
		    if (nl == YES)
			call strcat ("\n", Memc[title], len)
		    call strcat (Memc[GT_PARAMS(gt)], Memc[title], len)
		    nl = YES 
		}
		if (GT_TITLE(gt) != NULL) {
		    len = len + strlen (Memc[GT_TITLE(gt)]) + 1
		    call realloc (title, len, TY_CHAR)
		    if (nl == YES)
			call strcat ("\n", Memc[title], len)
		    call strcat (Memc[GT_TITLE(gt)], Memc[title], len)
		    nl = YES 
		}
		if (GT_SUBTITLE(gt) != NULL) {
		    len = len + strlen (Memc[GT_SUBTITLE(gt)]) + 1
		    call realloc (title, len, TY_CHAR)
		    if (nl == YES)
			call strcat ("\n", Memc[title], len)
		    call strcat (Memc[GT_SUBTITLE(gt)], Memc[title], len)
		    nl = YES 
		}
		if (GT_COMMENTS(gt) != NULL) {
		    len = len + strlen (Memc[GT_COMMENTS(gt)]) + 1
		    call realloc (title, len, TY_CHAR)
		    if (nl == YES)
			call strcat ("\n", Memc[title], len)
		    call strcat (Memc[GT_COMMENTS(gt)], Memc[title], len)
		    nl = YES 
		}
	    }

	    call malloc (xlabel, SZ_LINE, TY_CHAR)
	    Memc[xlabel] = EOS
	    if (GT_DRWXLABELS(gt) == YES) {
		if (GT_XLABEL(gt) != NULL)
		    call strcat (Memc[GT_XLABEL(gt)], Memc[xlabel], SZ_LINE)
		if (GT_XUNITS(gt) != NULL) {
		    call strcat (" (", Memc[xlabel], SZ_LINE)
		    call strcat (Memc[GT_XUNITS(gt)], Memc[xlabel], SZ_LINE)
		    call strcat (")", Memc[xlabel], SZ_LINE)
		}
	    }
	    if (GT_XFORMAT(gt) != NULL)
		call gsets (gp, G_XTICKFORMAT, Memc[GT_XFORMAT(gt)])

	    call malloc (ylabel, SZ_LINE, TY_CHAR)
	    Memc[ylabel] = EOS
	    if (GT_DRWYLABELS(gt) == YES) {
		if (GT_YLABEL(gt) != NULL)
		    call strcat (Memc[GT_YLABEL(gt)], Memc[ylabel], SZ_LINE)
		if (GT_YUNITS(gt) != NULL) {
		    call strcat (" (", Memc[ylabel], SZ_LINE)
		    call strcat (Memc[GT_YUNITS(gt)], Memc[ylabel], SZ_LINE)
		    call strcat (")", Memc[ylabel], SZ_LINE)
		}
	    }
	    if (GT_YFORMAT(gt) != NULL)
	    call gsets (gp, G_YTICKFORMAT, Memc[GT_YFORMAT(gt)])

	    call gseti (gp, G_XNMINOR, Memi[gt+GT_XNMINOR])
	    call gseti (gp, G_YNMINOR, Memi[gt+GT_YNMINOR])
	    if (GT_TRANSPOSE(gt) == NO)
	        call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	    else
	        call glabax (gp, Memc[title], Memc[ylabel], Memc[xlabel])

	    call ggview (gp, vx1, vx2, vy1, vy2)
	    call ggwind (gp, wx1, wx2, wy1, wy2)
	    call sprintf (Memc[title], SZ_LINE, "%g %g %g %g %g %g %g %g")
		call pargr (vx1)
		call pargr (vx2)
		call pargr (vy1)
		call pargr (vy2)
		call pargr (wx1)
		call pargr (wx2)
		call pargr (wy1)
		call pargr (wy2)
	    if (GP_UIFNAME(gp) != EOS)
		call gmsg (gp, "gtwcs", Memc[title])

	    call mfree (title, TY_CHAR)
	    call mfree (xlabel, TY_CHAR)
	    call mfree (ylabel, TY_CHAR)
	} else {
	    call gmftitle (gp, "UNTITLED")
	    call gseti (gp, G_XNMINOR, Memi[gt+GT_XNMINOR])
	    call gseti (gp, G_YNMINOR, Memi[gt+GT_YNMINOR])
	    call glabax (gp, "", "", "")
	}
end
