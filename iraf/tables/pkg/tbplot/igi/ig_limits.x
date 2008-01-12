include "igi.h"

#  IG_LIMITS -- Set the plot limits either autoscaling on the data or 
#  using the specified data window.

#  8/20/91 Removed ^Ls. ZGL
## 24 June 1992  changed autosc() for possible Z data.  ZGL

procedure ig_limits (igs)

pointer	igs			# igi parameters structure

pointer	igps			# Plot parameters structure
real	wl, wr, wb, wt		# Window

real	get_real()

errchk	get_real

begin
	call lcmdcat (igs, YES)

	iferr (wl = get_real (igs))
	    # Invalid parameter
	    return

	if (IS_INDEF (wl)) {
	    # No arguments;  autoscale on the data
	    wl = INDEF
	    wr = INDEF
	    wb = INDEF
	    wt = INDEF

	} else {
	    # Valid left edge;  there should be three more arguments

	    # Right edge
	    iferr (wr = get_real (igs))
		return

	    # Bottom edge
	    iferr (wb = get_real (igs))
		return

	    # Top edge
	    iferr (wt = get_real (igs))
		return
	}

	call cmdcat (igs, NO)

	call ii_limits (igs, wl, wr, wb, wt)

	if (DEBUG_OUTPUT(igs) == YES) {
	    igps = PLOT_PARMS(igs)
	    call eprintf ("Data window:  %.3f %.3f %.3f %.3f (WC) ")
		call pargr (MG_WINDLEFT(igps))
		call pargr (MG_WINDRIGHT(igps))
		call pargr (MG_WINDBOTTOM(igps))
		call pargr (MG_WINDTOP(igps))
	}
end


procedure ii_limits (igs, wl, wr, wb, wt)

pointer	igs			# igi parameters structure
real	wl, wr, wb, wt		# Data window

pointer	igps			# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	if (IS_INDEF(wl) && IS_INDEF(wr) && IS_INDEF(wb) && IS_INDEF(wt))
	    call autosc (GIO_GP(igs), 
		MG_XDATAP(igps), MG_YDATAP(igps), 
		MG_XNPTS(igps),  MG_YNPTS(igps), 
		MG_ZDATAP(igps), MG_ZNPTSX(igps), MG_ZNPTSY(igps),
		wl, wr, wb, wt)

	else
	    call scwind (GIO_GP(igs), wl, wr, wb, wt)

	call ggwind (GIO_GP(igs), 
	    MG_WINDLEFT(igps),   MG_WINDRIGHT(igps), 
	    MG_WINDBOTTOM(igps), MG_WINDTOP(igps))
end


#  AUTOSC -- Set the window using the input data vectors

procedure autosc (gp, xcol, ycol, xnpts, ynpts,
                  zcol, znx, zny,
                  wl, wr, wb, wt)

pointer	gp
pointer xcol,  ycol
int	xnpts, ynpts
pointer	zcol
int	znx, zny
real	wl, wr, wb, wt

begin
	if (xcol == NULL && ycol == NULL && zcol == NULL) {
	    call eprintf ("No X, Y or Z data ")
	    return
	}

	if (zcol == NULL) {
	    # No Z data

	    if (xcol == NULL && ycol == NULL) {
		call eprintf ("No X, Y or Z data ")

	    } else if (ycol == NULL) {
		call eprintf ("No Y or Z data ")

	    } else if (xcol == NULL) {
		# Y data only
		call gascale (gp, Memr[ycol], ynpts, 2)
		call gswind  (gp, 1.0, real (ynpts), INDEF, INDEF)

	    } else {
		# Both X and Y data
		call gascale (gp, Memr[xcol], xnpts, 1)
		call gascale (gp, Memr[ycol], ynpts, 2)
	    } 

	} else {
	    # We have Z data

	    if (xcol == NULL && ycol == NULL) {
		# No X or Y data;  use pixel coordinates of pixmap
		wl = 0.5;  wr = real (znx) + 0.5
		wb = 0.5;  wt = real (zny) + 0.5
		call gswind  (gp, wl, wr, wb, wt)

	    } else if (ycol == NULL) {
		#  X data only;  use X data and pixmap Y dimension
		call gascale (gp, Memr[xcol], xnpts, 1)
		wb = 0.5;  wt = real (zny) + 0.5
		call gswind  (gp, INDEF, INDEF, wb, wt)

	    } else if (xcol == NULL) {
		# Y data only;  Use Y data and pixmap X dimension
		call gascale (gp, Memr[ycol], ynpts, 2)
		wl = 0.5;  wr = real (znx) + 0.5
		call gswind  (gp, wl, wr, INDEF, INDEF)

	    } else {
		# Both X and Y data
		call gascale (gp, Memr[xcol], xnpts, 1)
		call gascale (gp, Memr[ycol], ynpts, 2)
	    } 

	}
end


#  SCWIND -- Explicitly set the data window

procedure scwind (gp, wl, wr, wb, wt)

pointer	gp			# Graphics pointer
real	wl, wr, wb, wt		# Data window

begin
	if (wl == wr)
	    # Set only the Y axis
	    call gswind (gp, INDEF, INDEF, wb, wt)
	else if (wb == wt)
	    # Set only the X axis
	    call gswind (gp, wl, wr, INDEF, INDEF)
	else
	    # Set both axes
	    call gswind (gp, wl, wr, wb, wt)
end
