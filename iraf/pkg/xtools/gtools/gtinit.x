# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

# GT_INIT1 -- Open the GTOOLS pointer.

pointer procedure gt_init1 (gp)

pointer	gp
pointer	gt

pointer	gt_init()
errchk	gt_init, gt_ireset

begin
	# Initialize the graphics.

	gt = gt_init()
	call gt_ireset (gp, gt)

	return (gt)
end


# GT_INIT -- Allocate and initialize GTOOLS pointer.
#
# This is an older version.  To properly set things either gt_ireset
# should be called after gt_init or use the new gt_init1.

pointer procedure gt_init ()

size_t	sz_val
pointer	gt

begin
	# Initialize the graphics.

	sz_val = LEN_GT
	call calloc (gt, sz_val, TY_STRUCT)
	GT_VXMIN(gt) = INDEFR
	GT_VXMAX(gt) = INDEFR
	GT_VYMIN(gt) = INDEFR
	GT_VYMAX(gt) = INDEFR
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
	GT_DRWTITLE(gt) = YES
	GT_DRWXLABELS(gt) = YES
	GT_DRWYLABELS(gt) = YES
	GT_XFORMAT(gt) = NULL
	GT_YFORMAT(gt) = NULL
	GT_XBUF(gt) = .03
	GT_YBUF(gt) = .03
	GT_LCLIP(gt) = 0.
	GT_HCLIP(gt) = 0.
	GT_XFLIP(gt) = NO
	GT_YFLIP(gt) = NO
	GT_TRANSPOSE(gt) = NO
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTMARK, "plus")
	call gt_seti (gt, GTLINE, 1)
	call gt_seti (gt, GTCOLOR, 1)

	GT_RESET(gt) = NO

	return (gt)
end


# GT_IRESET -- Initialize GTOOLS values from GP pointer.

procedure gt_ireset (gp, gt)

pointer	gp			#I GIO pointer
pointer	gt			#I GTOOLS pointer

int	gstati()
real	gstatr()

begin
	Memi[P2I(gt+GT_TXUP)] = gstati (gp, G_TXUP)
	Memr[P2R(gt+GT_TXSIZE)] = gstatr (gp, G_TXSIZE)
	Memi[P2I(gt+GT_TXPATH)] = gstati (gp, G_TXPATH)
	Memr[P2R(gt+GT_TXSPACING)] = gstatr (gp, G_TXSPACING)
	Memi[P2I(gt+GT_TXHJUSTIFY)] = gstati (gp, G_TXHJUSTIFY)
	Memi[P2I(gt+GT_TXVJUSTIFY)] = gstati (gp, G_TXVJUSTIFY)
	Memi[P2I(gt+GT_TXFONT)] = gstati (gp, G_TXFONT)
	Memi[P2I(gt+GT_TXQUALITY)] = gstati (gp, G_TXQUALITY)
	Memi[P2I(gt+GT_TXCOLOR)] = gstati (gp, G_TXCOLOR)

	Memi[P2I(gt+GT_DRAWTITLE)] = gstati (gp, G_DRAWTITLE)
	Memr[P2R(gt+GT_TITLESIZE)] = gstatr (gp, G_TITLESIZE)
	#Memi[P2I(gt+GT_TITLEJUST)] = gstati (gp, G_TITLEJUST)
	Memi[P2I(gt+GT_NTITLELINES)] = gstati (gp, G_NTITLELINES)
	Memr[P2R(gt+GT_ASPECT)] = gstatr (gp, G_ASPECT)
	#Memr[P2R(gt+GT_CHARSIZE)] = gstatr (gp, G_CHARSIZE)
	Memi[P2I(gt+GT_TITLECOLOR)] = gstati (gp, G_TITLECOLOR)
	Memi[P2I(gt+GT_FRAMECOLOR)] = gstati (gp, G_FRAMECOLOR)

	Memi[P2I(gt+GT_XDRAWAXES)] = gstati (gp, G_XDRAWAXES)
	Memi[P2I(gt+GT_XSETAXISPOS)] = gstati (gp, G_XSETAXISPOS)
	Memr[P2R(gt+GT_XAXISPOS1)] = gstatr (gp, G_XAXISPOS1)
	Memr[P2R(gt+GT_XAXISPOS2)] = gstatr (gp, G_XAXISPOS2)
	Memi[P2I(gt+GT_XDRAWGRID)] = gstati (gp, G_YDRAWGRID)
	Memi[P2I(gt+GT_XROUND)] = gstati (gp, G_XROUND)
	Memi[P2I(gt+GT_XLABELAXIS)] = gstati (gp, G_XLABELAXIS)
	Memr[P2R(gt+GT_XAXISLABELSIZE)] = gstatr (gp, G_XAXISLABELSIZE)
	Memi[P2I(gt+GT_XDRAWTICKS)] = gstati (gp, G_XDRAWTICKS)
	Memi[P2I(gt+GT_XLABELTICKS)] = gstati (gp, G_XLABELTICKS)
	Memi[P2I(gt+GT_XNMAJOR)] = gstati (gp, G_XNMAJOR)
	#Memi[P2I(gt+GT_XNMINOR)] = gstati (gp, G_XNMINOR)
	Memi[P2I(gt+GT_XNMINOR)] = 0
	Memr[P2R(gt+GT_XMAJORLENGTH)] = gstatr (gp, G_XMAJORLENGTH)
	Memr[P2R(gt+GT_XMINORLENGTH)] = gstatr (gp, G_XMINORLENGTH)
	Memr[P2R(gt+GT_XMAJORWIDTH)] = gstatr (gp, G_XMAJORWIDTH)
	Memr[P2R(gt+GT_XMINORWIDTH)] = gstatr (gp, G_XMINORWIDTH)
	Memr[P2R(gt+GT_XAXISWIDTH)] = gstatr (gp, G_XAXISWIDTH)
	Memr[P2R(gt+GT_XTICKLABELSIZE)] = gstatr (gp, G_XTICKLABELSIZE)
	Memi[P2I(gt+GT_XGRIDCOLOR)] = gstati (gp, G_XGRIDCOLOR)
	Memi[P2I(gt+GT_XAXISLABELCOLOR)] = gstati (gp, G_XAXISLABELCOLOR)
	Memi[P2I(gt+GT_XAXISCOLOR)] = gstati (gp, G_XAXISCOLOR)
	Memi[P2I(gt+GT_XTICKLABELCOLOR)] = gstati (gp, G_XTICKLABELCOLOR)
	Memi[P2I(gt+GT_XTICKCOLOR)] = gstati (gp, G_XTICKCOLOR)

	Memi[P2I(gt+GT_YDRAWAXES)] = gstati (gp, G_YDRAWAXES)
	Memi[P2I(gt+GT_YSETAXISPOS)] = gstati (gp, G_YSETAXISPOS)
	Memr[P2R(gt+GT_YAXISPOS1)] = gstatr (gp, G_YAXISPOS1)
	Memr[P2R(gt+GT_YAXISPOS2)] = gstatr (gp, G_YAXISPOS2)
	Memi[P2I(gt+GT_YDRAWGRID)] = gstati (gp, G_XDRAWGRID)
	Memi[P2I(gt+GT_YROUND)] = gstati (gp, G_YROUND)
	Memi[P2I(gt+GT_YLABELAXIS)] = gstati (gp, G_YLABELAXIS)
	Memr[P2R(gt+GT_YAXISLABELSIZE)] = gstatr (gp, G_YAXISLABELSIZE)
	Memi[P2I(gt+GT_YDRAWTICKS)] = gstati (gp, G_YDRAWTICKS)
	Memi[P2I(gt+GT_YLABELTICKS)] = gstati (gp, G_YLABELTICKS)
	Memi[P2I(gt+GT_YNMAJOR)] = gstati (gp, G_YNMAJOR)
	#Memi[P2I(gt+GT_YNMINOR)] = gstati (gp, G_YNMINOR)
	Memi[P2I(gt+GT_YNMINOR)] = 0
	Memr[P2R(gt+GT_YMAJORLENGTH)] = gstatr (gp, G_YMAJORLENGTH)
	Memr[P2R(gt+GT_YMINORLENGTH)] = gstatr (gp, G_YMINORLENGTH)
	Memr[P2R(gt+GT_YMAJORWIDTH)] = gstatr (gp, G_YMAJORWIDTH)
	Memr[P2R(gt+GT_YMINORWIDTH)] = gstatr (gp, G_YMINORWIDTH)
	Memr[P2R(gt+GT_YAXISWIDTH)] = gstatr (gp, G_YAXISWIDTH)
	Memr[P2R(gt+GT_YTICKLABELSIZE)] = gstatr (gp, G_YTICKLABELSIZE)
	Memi[P2I(gt+GT_YGRIDCOLOR)] = gstati (gp, G_YGRIDCOLOR)
	Memi[P2I(gt+GT_YAXISLABELCOLOR)] = gstati (gp, G_YAXISLABELCOLOR)
	Memi[P2I(gt+GT_YAXISCOLOR)] = gstati (gp, G_YAXISCOLOR)
	Memi[P2I(gt+GT_YTICKLABELCOLOR)] = gstati (gp, G_YTICKLABELCOLOR)
	Memi[P2I(gt+GT_YTICKCOLOR)] = gstati (gp, G_YTICKCOLOR)

	GT_RESET(gt) = YES
end
