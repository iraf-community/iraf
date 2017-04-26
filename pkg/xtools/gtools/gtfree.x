# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gtools.h"

# GT_FREE -- Free extended graphics tools structure.

procedure gt_free (gt)

pointer	gt			# Graphic tools pointer

begin
	if (gt == NULL)
	    return

	call mfree (GT_PARAMS(gt), TY_CHAR)
	call mfree (GT_TITLE(gt), TY_CHAR)
	call mfree (GT_SUBTITLE(gt), TY_CHAR)
	call mfree (GT_COMMENTS(gt), TY_CHAR)
	call mfree (GT_XLABEL(gt), TY_CHAR)
	call mfree (GT_YLABEL(gt), TY_CHAR)
	call mfree (GT_XUNITS(gt), TY_CHAR)
	call mfree (GT_YUNITS(gt), TY_CHAR)
	call mfree (GT_XFORMAT(gt), TY_CHAR)
	call mfree (GT_YFORMAT(gt), TY_CHAR)
	call mfree (gt, TY_STRUCT)
end
