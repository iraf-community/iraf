include <gset.h>
include "igi.h"

#  8/20/91 Removed ^Ls. ZGL

procedure ig_window (igs)

pointer	igs			# igi parameters structure

pointer	igps			# igi parameters structure
int	nxp, nyp
int	pane
real	gvl, gvr, gvb, gvt	# GIO viewport
pointer	tokvals			# Token value structure
int	token

int	gettok()

begin
	call lcmdcat (igs, YES)

	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	# Get the first argument
	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    # No argument;  list the window
	    call show_window (STDOUT, igs)
	    call cmdcat (igs, NO)
	    return

	} else if (token == CONSTANT) {
	    # Valid numeric argument;
	    # Could be number of horizonal panes or pane number
	    if (LOP_TYPE(tokvals) == TY_INT)
		nxp = LOP_VALI(tokvals)
	    else
		nxp = int (LOP_VALR(tokvals))
	    call lcmdcat (igs, NO)

	    # Get another argument
	    token = gettok (igs)
	    if (token == CONSTANT) {
		# Number of vertical panes (tiles) or vertical index
		if (LOP_TYPE(tokvals) == TY_INT)
		    nyp = LOP_VALI(tokvals)
		else
		    nyp = int (LOP_VALR(tokvals))
		call lcmdcat (igs, NO)

		# Get another argument
		token = gettok (igs)
		if (token == CONSTANT) {
		    # Third argument:  pane (tile) number
		    if (LOP_TYPE(tokvals) == TY_INT)
			pane = LOP_VALI(tokvals)
		    else
			pane = int (LOP_VALR(tokvals))
		    call lcmdcat (igs, NO)
		} else if (IS_NEWCOMMAND(token)) {
		    # Two arguments only;
		    # Retile window but stay in same tile number
		    pane = MG_PANE(igps)
		} else {
		    call eprintf ("Numeric value required ")
		    return
		}

	    } else if (IS_NEWCOMMAND(token)) {
		# One argument only;  move to another tile (pane)
		pane = nxp
		nxp  = MG_NXPANE(igps)
		nyp  = MG_NYPANE(igps)
	    } else {
		call eprintf ("Numeric value required ")
		return
	    }

	} else {
	    call eprintf ("Numeric value required ")
	    return
	}

	call cmdcat (igs, NO)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("%d X %d panes;  Pane: %d ")
		call pargi (nxp)
		call pargi (nyp)
		    call pargi (pane)
	}

	if (nxp * nyp > MAX_TILES) {
	    # Too many panes (tiles) specified
	    call eprintf ("Too many panes (tiles) specified:  %d;  ")
		call pargi (nxp * nyp)
	    call eprintf ("Cannot have more than %d ")
		call pargi (MAX_TILES)
	    return
	} 

	if (nxp <= 0 || nyp <= 0) {
	    call eprintf ("Invalid window: %d %d %d ")
		call pargi (nxp)
		call pargi (nyp)
		call pargi (pane)
	    return
	} 

	call ii_window (igs, nxp, nyp, pane)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call ggview (GIO_GP(igs), gvl, gvr, gvb, gvt)
	    call eprintf ("GIO Viewport:\t\t%.3f\t%.3f\t%.3f\t%.3f (NDC) ")
		call pargr (gvl)
		call pargr (gvr)
		call pargr (gvb)
		call pargr (gvt)
	}
end


procedure ii_window (igs, nxp, nyp, pane)

pointer	igs			# igi parameters structure descriptor
int	nxp, nyp		# Number of tiles in X and Y
int	pane			# Tile (WCS) number

pointer	igps			# igi parameters structure
int	ixp, iyp		# Tile coordinate
real	vpl, vpr, vpb, vpt	# Virtual page
real	gwl, gwr, gwb, gwt	# GIO data window

begin
	igps = PLOT_PARMS(igs)

	# Find the window coordinates of the specified pane
	ixp = mod (pane, nxp)
	if (ixp == 0)
	    ixp = nxp
	iyp = 1 + (pane - 1) / nxp

	MG_NXPANE(igps) = nxp
	MG_NYPANE(igps) = nyp
	MG_PANE(igps)   = pane

	# Window pane defines the "virtual page" on the device
	vpl = real (ixp - 1) / real (nxp)
	vpr = real (ixp) / real (nxp)
	vpb = real (iyp - 1) / real (nyp)
	vpt = real (iyp) / real (nyp)

	MG_PAGELEFT(igps)   = vpl
	MG_PAGERIGHT(igps)  = vpr
	MG_PAGEBOTTOM(igps) = vpb
	MG_PAGETOP(igps)    = vpt

	# Set the WCS for the specified pane
	call gseti  (GIO_GP(igs), G_WCS, pane)

	# Virtual page and (already defined) viewport define the
	# GIO vieport on the device
	call vpage (igs)

	# Data window (already defined globally)
	gwl = MG_WINDLEFT(igps)
	gwr = MG_WINDRIGHT(igps)
	gwb = MG_WINDBOTTOM(igps)
	gwt = MG_WINDTOP(igps)

	call gswind (GIO_GP(igs), gwl, gwr, gwb, gwt)
end
