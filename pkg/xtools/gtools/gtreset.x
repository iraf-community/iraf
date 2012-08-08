# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

# GT_RESET -- Reset parameters after a gclear, greset, or gcancel.

procedure gt_reset (gp, gt)

pointer	gp			#I GIO pointer
pointer	gt			#I GTOOLS pointer

begin
	if (GT_RESET(gt) == NO)
	    call gt_ireset (gp, gt)

	call gseti (gp, G_TXUP, Memi[gt+GT_TXUP])
	call gsetr (gp, G_TXSIZE, Memr[P2R(gt+GT_TXSIZE)])
	call gseti (gp, G_TXPATH, Memi[gt+GT_TXPATH])
	call gsetr (gp, G_TXSPACING, Memr[P2R(gt+GT_TXSPACING)])
	call gseti (gp, G_TXHJUSTIFY, Memi[gt+GT_TXHJUSTIFY])
	call gseti (gp, G_TXVJUSTIFY, Memi[gt+GT_TXVJUSTIFY])
	call gseti (gp, G_TXFONT, Memi[gt+GT_TXFONT])
	call gseti (gp, G_TXQUALITY, Memi[gt+GT_TXQUALITY])
	call gseti (gp, G_TXCOLOR, Memi[gt+GT_TXCOLOR])

	call gseti (gp, G_DRAWTITLE, Memi[gt+GT_DRAWTITLE])
	call gsetr (gp, G_TITLESIZE, Memr[P2R(gt+GT_TITLESIZE)])
	#call gseti (gp, G_TITLEJUST, Memi[gt+GT_TITLEJUST])
	call gseti (gp, G_NTITLELINES, Memi[gt+GT_NTITLELINES])
	call gsetr (gp, G_ASPECT, Memr[P2R(gt+GT_ASPECT)])
	#call gsetr (gp, G_CHARSIZE, Memr[P2R(gt+GT_CHARSIZE)])
	call gseti (gp, G_TITLECOLOR, Memi[gt+GT_TITLECOLOR])
	call gseti (gp, G_FRAMECOLOR, Memi[gt+GT_FRAMECOLOR])

	call gseti (gp, G_XDRAWAXES, Memi[gt+GT_XDRAWAXES])
	call gseti (gp, G_XSETAXISPOS, Memi[gt+GT_XSETAXISPOS])
	call gsetr (gp, G_XAXISPOS1, Memr[P2R(gt+GT_XAXISPOS1)])
	call gsetr (gp, G_XAXISPOS2, Memr[P2R(gt+GT_XAXISPOS2)])
	call gseti (gp, G_YDRAWGRID, Memi[gt+GT_XDRAWGRID])
	call gseti (gp, G_XROUND, Memi[gt+GT_XROUND])
	call gseti (gp, G_XLABELAXIS, Memi[gt+GT_XLABELAXIS])
	call gsetr (gp, G_XAXISLABELSIZE, Memr[P2R(gt+GT_XAXISLABELSIZE)])
	call gseti (gp, G_XDRAWTICKS, Memi[gt+GT_XDRAWTICKS])
	call gseti (gp, G_XLABELTICKS, Memi[gt+GT_XLABELTICKS])
	call gseti (gp, G_XNMAJOR, Memi[gt+GT_XNMAJOR])
	call gseti (gp, G_XNMINOR, Memi[gt+GT_XNMINOR])
	call gsetr (gp, G_XMAJORLENGTH, Memr[P2R(gt+GT_XMAJORLENGTH)])
	call gsetr (gp, G_XMINORLENGTH, Memr[P2R(gt+GT_XMINORLENGTH)])
	call gsetr (gp, G_XMAJORWIDTH, Memr[P2R(gt+GT_XMAJORWIDTH)])
	call gsetr (gp, G_XMINORWIDTH, Memr[P2R(gt+GT_XMINORWIDTH)])
	call gsetr (gp, G_XAXISWIDTH, Memr[P2R(gt+GT_XAXISWIDTH)])
	call gsetr (gp, G_XTICKLABELSIZE, Memr[P2R(gt+GT_XTICKLABELSIZE)])
	call gseti (gp, G_XGRIDCOLOR, Memi[gt+GT_XGRIDCOLOR])
	call gseti (gp, G_XAXISLABELCOLOR, Memi[gt+GT_XAXISLABELCOLOR])
	call gseti (gp, G_XAXISCOLOR, Memi[gt+GT_XAXISCOLOR])
	call gseti (gp, G_XTICKLABELCOLOR, Memi[gt+GT_XTICKLABELCOLOR])
	call gseti (gp, G_XTICKCOLOR, Memi[gt+GT_XTICKCOLOR])

	call gseti (gp, G_YDRAWAXES, Memi[gt+GT_YDRAWAXES])
	call gseti (gp, G_YSETAXISPOS, Memi[gt+GT_YSETAXISPOS])
	call gsetr (gp, G_YAXISPOS1, Memr[P2R(gt+GT_YAXISPOS1)])
	call gsetr (gp, G_YAXISPOS2, Memr[P2R(gt+GT_YAXISPOS2)])
	call gseti (gp, G_XDRAWGRID, Memi[gt+GT_YDRAWGRID])
	call gseti (gp, G_YROUND, Memi[gt+GT_YROUND])
	call gseti (gp, G_YLABELAXIS, Memi[gt+GT_YLABELAXIS])
	call gsetr (gp, G_YAXISLABELSIZE, Memr[P2R(gt+GT_YAXISLABELSIZE)])
	call gseti (gp, G_YDRAWTICKS, Memi[gt+GT_YDRAWTICKS])
	call gseti (gp, G_YLABELTICKS, Memi[gt+GT_YLABELTICKS])
	call gseti (gp, G_YNMAJOR, Memi[gt+GT_YNMAJOR])
	call gseti (gp, G_YNMINOR, Memi[gt+GT_YNMINOR])
	call gsetr (gp, G_YMAJORLENGTH, Memr[P2R(gt+GT_YMAJORLENGTH)])
	call gsetr (gp, G_YMINORLENGTH, Memr[P2R(gt+GT_YMINORLENGTH)])
	call gsetr (gp, G_YMAJORWIDTH, Memr[P2R(gt+GT_YMAJORWIDTH)])
	call gsetr (gp, G_YMINORWIDTH, Memr[P2R(gt+GT_YMINORWIDTH)])
	call gsetr (gp, G_YAXISWIDTH, Memr[P2R(gt+GT_YAXISWIDTH)])
	call gsetr (gp, G_YTICKLABELSIZE, Memr[P2R(gt+GT_YTICKLABELSIZE)])
	call gseti (gp, G_YGRIDCOLOR, Memi[gt+GT_YGRIDCOLOR])
	call gseti (gp, G_YAXISLABELCOLOR, Memi[gt+GT_YAXISLABELCOLOR])
	call gseti (gp, G_YAXISCOLOR, Memi[gt+GT_YAXISCOLOR])
	call gseti (gp, G_YTICKLABELCOLOR, Memi[gt+GT_YTICKLABELCOLOR])
	call gseti (gp, G_YTICKCOLOR, Memi[gt+GT_YTICKCOLOR])
end
