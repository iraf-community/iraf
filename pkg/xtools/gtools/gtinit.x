# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

# GT_INIT -- Allocate and initialize extended graphic tools structure.

pointer procedure gt_init ()

pointer	gt

begin
	# Initialize the graphics.

	call malloc (gt, LEN_GT, TY_STRUCT)
	GT_XMIN(gt) = INDEFR
	GT_XMAX(gt) = INDEFR
	GT_YMIN(gt) = INDEFR
	GT_YMAX(gt) = INDEFR
	call gt_sets (gt, GTXTRAN, "linear")
	call gt_sets (gt, GTYTRAN, "linear")
	GT_XSIZE(gt) = 2.
	GT_YSIZE(gt) = 2.
	GT_SYSID(gt) = YES
	GT_PARAMS(gt) = NULL
	GT_TITLE(gt) = NULL
	GT_SUBTITLE(gt) = NULL
	GT_COMMENTS(gt) = NULL
	GT_XLABEL(gt) = NULL
	GT_YLABEL(gt) = NULL
	GT_XUNITS(gt) = NULL
	GT_YUNITS(gt) = NULL
	GT_XBUF(gt) = .03
	GT_YBUF(gt) = .03
	GT_TRANSPOSE(gt) = NO
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTMARK, "plus")
	call gt_seti (gt, GTLINE, 1)

	return (gt)
end
