include <gset.h>
include "igi.h"
include "commands.h"

#  8/20/91 Removed ^Ls. ZGL
## 9 July 1992  Added viewport coordiantes;
##              view_wcs(), ii_pmove(), ii_pdraw(). ZGL

procedure ig_move (cmd, igs)

int	cmd		# Command
pointer	igs		# igi parameters structure

int	in		# Input stream
pointer	tokvals		# Token value structure
pointer	igps		# Plot parameters structure
real	xpos, ypos
int	token
int	wcs

int	gettok(), gstati()

begin
	call lcmdcat (igs, YES)

	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	token = gettok (igs)

	if (token == CONSTANT) {
	    # First argument:  X location
	    if (LOP_TYPE(tokvals) == TY_REAL)
		xpos = LOP_VALR(tokvals)
	    else
		xpos = real (LOP_VALI(tokvals))

	} else if (IS_NEWCOMMAND(token)) {
	    # No argument;  list the current pen position
	    call printf ("Current position:  %g, %g (WCS), %.3f, %.3f (NDC)")
		call pargr (MG_XPOS(igps))
		call pargr (MG_YPOS(igps))
	    wcs = gstati (GIO_GP(igs), G_WCS)
	    call ig_gctran (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps), 
		xpos, ypos, wcs, 0)
		call pargr (xpos)
		call pargr (ypos)
	    return

	} else {
	    call eprintf ("Numeric value required for X coordinate ")
	    return
	}

	call lcmdcat (igs, NO)
	token = gettok (igs)

	if (token == CONSTANT) {
	    # Second argument:  Y location
	    if (LOP_TYPE(tokvals) == TY_REAL)
		ypos = LOP_VALR(tokvals)
	    else
		ypos = real (LOP_VALI(tokvals))

	} else {
	    call eprintf ("Numeric value required for Y coordinate ")
	    return
	}

	call lcmdcat (igs, NO)

	switch (cmd) {
	case DRAW:
	    call ii_draw (igs, xpos, ypos)
	    call cmdcat  (igs, YES)

	case DDRAW:
	    call ii_ddraw(igs, xpos, ypos)
	    call cmdcat  (igs, YES)

	case PDRAW:
	    call ii_pdraw(igs, xpos, ypos)
	    call cmdcat  (igs, YES)

	case VDRAW:
	    call ii_vdraw(igs, xpos, ypos)
	    call cmdcat  (igs, YES)

	case MOVE, RELOCATE:
	    call ii_move (igs, xpos, ypos)
	    call cmdcat  (igs, NO)

	case DMOVE, DRELOCATE:
	    call ii_dmove(igs, xpos, ypos)
	    call cmdcat  (igs, NO)

	case PMOVE, PRELOCATE:
	    call ii_pmove(igs, xpos, ypos)
	    call cmdcat  (igs, NO)

	case VMOVE, VRELOCATE:
	    call ii_vmove(igs, xpos, ypos)
	    call cmdcat  (igs, NO)
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Current position:  %g, %g (WCS), %.3f, %.3f (NDC)")
		call pargr (MG_XPOS(igps))
		call pargr (MG_YPOS(igps))
	    wcs = gstati (GIO_GP(igs), G_WCS)
	    call ig_gctran (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps), 
		xpos, ypos, wcs , 0)
		call pargr (xpos)
		call pargr (ypos)
	}
end


procedure ii_draw (igs, xpos, ypos)

pointer	igs		# igi parameters structure
real	xpos, ypos

pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	call gseti (GIO_GP(igs), G_CLIP, NO)

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))

	# Set the polyline type
	call setltype (igs, MG_LTYPEN(igps))

	# Draw the line in WCS
	call gadraw (GIO_GP(igs), xpos, ypos)

	call gflush (GIO_GP(igs))

	MG_XPOS(igps) = xpos
	MG_YPOS(igps) = ypos
end


procedure ii_ddraw (igs, xpos, ypos)

pointer	igs		# igi parameters structure
real	xpos, ypos

int	wcs

int	gstati()

begin
	# Convert NDC to WCS
	wcs = gstati (GIO_GP(igs), G_WCS)
	call ig_gctran (GIO_GP(igs), xpos, ypos, xpos, ypos, 0, wcs)

	call ii_draw (igs, xpos, ypos)
end


procedure ii_vdraw (igs, xpos, ypos)

pointer	igs		# igi parameters structure
real	xpos, ypos


begin
	# Convert VPC to WCS
	call vpc_wcs (igs, xpos, ypos, xpos, ypos)

	call ii_draw (igs, xpos, ypos)
end


procedure ii_pdraw (igs, xpos, ypos)

pointer	igs		# igi parameters structure
real	xpos, ypos

begin
	# Convert VPC to WCS
	call view_wcs (igs, xpos, ypos, xpos, ypos)

	call ii_draw (igs, xpos, ypos)
end


procedure ii_move (igs, xpos, ypos)

pointer	igs		# igi parameters structure
real	xpos, ypos

pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	# Move to WCS location
	call gamove (GIO_GP(igs), xpos, ypos)

	call gflush (GIO_GP(igs))

	MG_XPOS(igps) = xpos
	MG_YPOS(igps) = ypos
end


procedure ii_dmove (igs, xpos, ypos)

pointer	igs		# igi parameters structure
real	xpos, ypos

int	wcs

int	gstati()

begin
	# Convert NDC to WCS
	wcs = gstati (GIO_GP(igs), G_WCS)
	call ig_gctran (GIO_GP(igs), xpos, ypos, xpos, ypos, 0, wcs)

	# Move to NDC location
	call ii_move (igs, xpos, ypos)
end


procedure ii_vmove (igs, xpos, ypos)

pointer	igs		# igi parameters structure
real	xpos, ypos

begin
	# Convert VPC to WCS
	call vpc_wcs(igs, xpos, ypos, xpos, ypos)

	# Move to VPC location
	call ii_move (igs, xpos, ypos)
end


procedure ii_pmove (igs, xpos, ypos)

#  ii_pmove -- Move current (pen) position in absolute viewport coordinates. 

pointer	igs		# igi parameters structure
real	xpos, ypos	# Viewport coordinates

begin
	# Convert VC to WCS
	call view_wcs(igs, xpos, ypos, xpos, ypos)

	# Move to VPC location
	call ii_move (igs, xpos, ypos)
end


procedure vpc_wcs (igs, vx, vy, wx, wy)

#  vpc_wcs -- Transform virtual page cordinates (VPC) to world
#  coordinates (WC).  Viewport coordinates are the edges of the axes,
#  a subset of the virtual page.

pointer	igs			# igi parameters structure descriptor
real	vx, vy			# Virtual Page position
real	wx, wy			# WCS position

pointer	igps
real	gvl, gvr, gvb, gvt
real	dwl, dwr, dwb, dwt
real	vpl, vpr, vpb, vpt
real    gvx, gvy

begin
	igps = PLOT_PARMS(igs)

	# GIO viewport in NDC
	call ggview (GIO_GP(igs), gvl, gvr, gvb, gvt)

	# GIO window, WC of viewport
	call ggwind (GIO_GP(igs), dwl, dwr, dwb, dwt)

	# igi virtual page
	vpl = MG_PAGELEFT(igps)
	vpr = MG_PAGERIGHT(igps)
	vpb = MG_PAGEBOTTOM(igps)
	vpt = MG_PAGETOP(igps)

	gvx = vpl + vx * (vpr - vpl)
	gvy = vpb + vy * (vpt - vpb)

	wx = dwl + (gvx - gvl) * (dwr - dwl) / (gvr - gvl)
	wy = dwb + (gvy - gvb) * (dwt - dwb) / (gvt - gvb)
end


procedure view_wcs (igs, vx, vy, wx, wy)

#  view_wcs -- Transform viewport cordinates (VC) to world coordinates
#  (WC).  Viewport coordinates are the edges of the axes, a subset of the
#  virtual page.

## 9 July 1992  ZGL

pointer	igs			# igi parameters structure descriptor
real	vx, vy			# Viewport position
real	wx, wy			# WCS position

pointer	igps
real	gvl, gvr, gvb, gvt
real	dwl, dwr, dwb, dwt
real	vpl, vpr, vpb, vpt
real    gvx, gvy

begin
	igps = PLOT_PARMS(igs)

	# GIO viewport in NDC
	call ggview (GIO_GP(igs), gvl, gvr, gvb, gvt)

	# GIO window, WC of viewport
	call ggwind (GIO_GP(igs), dwl, dwr, dwb, dwt)

	# igi viewport in NDC
	vpl = MG_VIEWLEFT(igps)
	vpr = MG_VIEWRIGHT(igps)
	vpb = MG_VIEWBOTTOM(igps)
	vpt = MG_VIEWTOP(igps)

	gvx = vpl + vx * (vpr - vpl)
	gvy = vpb + vy * (vpt - vpb)

	wx = dwl + (gvx - gvl) * (dwr - dwl) / (gvr - gvl)
	wy = dwb + (gvy - gvb) * (dwt - dwb) / (gvt - gvb)
end
