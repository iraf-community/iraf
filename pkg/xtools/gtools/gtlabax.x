# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"
	
# GT_LABAX -- Set graphics axis.

procedure gt_labax (gp, gt)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer

int	len
pointer	title, xlabel, ylabel

int	strlen()

begin
	if (gt != NULL) {
	    call malloc (title, SZ_LINE, TY_CHAR)
	    Memc[title] = EOS
	    if (GT_DRAWTITLE(gt) == YES) {
		len = 0
		if (GT_SYSID(gt) == YES) {
		    len = SZ_LINE
		    call sysid (Memc[title], len)
		    len = strlen (Memc[title])
		    call realloc (title, len, TY_CHAR)
		}
		if (GT_PARAMS(gt) != NULL) {
		    len = len + strlen (Memc[GT_PARAMS(gt)]) + 1
		    call realloc (title, len, TY_CHAR)
		    call strcat ("\n", Memc[title], len)
		    call strcat (Memc[GT_PARAMS(gt)], Memc[title], len)
		}
		if (GT_TITLE(gt) != NULL) {
		    len = len + strlen (Memc[GT_TITLE(gt)]) + 1
		    call realloc (title, len, TY_CHAR)
		    call strcat ("\n", Memc[title], len)
		    call strcat (Memc[GT_TITLE(gt)], Memc[title], len)
		}
		if (GT_SUBTITLE(gt) != NULL) {
		    len = len + strlen (Memc[GT_SUBTITLE(gt)]) + 1
		    call realloc (title, len, TY_CHAR)
		    call strcat ("\n", Memc[title], len)
		    call strcat (Memc[GT_SUBTITLE(gt)], Memc[title], len)
		}
		if (GT_COMMENTS(gt) != NULL) {
		    len = len + strlen (Memc[GT_COMMENTS(gt)]) + 1
		    call realloc (title, len, TY_CHAR)
		    call strcat ("\n", Memc[title], len)
		    call strcat (Memc[GT_COMMENTS(gt)], Memc[title], len)
		}
	    }

	    call malloc (xlabel, SZ_LINE, TY_CHAR)
	    Memc[xlabel] = EOS
	    if (GT_DRAWXLABELS(gt) == YES) {
		if (GT_XLABEL(gt) != NULL)
		    call strcat (Memc[GT_XLABEL(gt)], Memc[xlabel], SZ_LINE)
		if (GT_XUNITS(gt) != NULL) {
		    call strcat (" (", Memc[xlabel], SZ_LINE)
		    call strcat (Memc[GT_XUNITS(gt)], Memc[xlabel], SZ_LINE)
		    call strcat (")", Memc[xlabel], SZ_LINE)
		}
	    }

	    call malloc (ylabel, SZ_LINE, TY_CHAR)
	    Memc[ylabel] = EOS
	    if (GT_DRAWYLABELS(gt) == YES) {
		if (GT_YLABEL(gt) != NULL)
		    call strcat (Memc[GT_YLABEL(gt)], Memc[ylabel], SZ_LINE)
		if (GT_YUNITS(gt) != NULL) {
		    call strcat (" (", Memc[ylabel], SZ_LINE)
		    call strcat (Memc[GT_YUNITS(gt)], Memc[ylabel], SZ_LINE)
		    call strcat (")", Memc[ylabel], SZ_LINE)
		}
	    }

	    call gseti (gp, G_NMINOR, 0)
	    call gmftitle (gp, Memc[title])
	    if (GT_TRANSPOSE(gt) == NO)
	        call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	    else
	        call glabax (gp, Memc[title], Memc[ylabel], Memc[xlabel])

	    call mfree (title, TY_CHAR)
	    call mfree (xlabel, TY_CHAR)
	    call mfree (ylabel, TY_CHAR)
	} else {
	    call gmftitle (gp, "UNTITLED")
	    call gseti (gp, G_NMINOR, 0)
	    call glabax (gp, "", "", "")
	}
end
