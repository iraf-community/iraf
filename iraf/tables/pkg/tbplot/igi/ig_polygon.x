include <gset.h>
include "igi.h"

#  8/20/91 Removed ^Ls. ZGL

procedure ig_polygon (igs)

pointer	igs		# igi parameters structure

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	if (MG_YDATAP(PLOT_PARMS(igs)) == NULL) {
	    call eprintf ("No Y data ")
	    return
	}

	call ii_polygon (igs)
end


procedure ii_polygon (igs)

pointer	igs		# igi parameters structure

int	i
int	igps
int	npts
pointer	tmpx

begin
	# Clip at viewport boundary
	call gseti (GIO_GP(igs), G_CLIP, YES)

	igps = PLOT_PARMS(igs)

	# Set the polyline type
	call setltype (igs, MG_LTYPEN(igps))

	# Set the line width
	call gsetr (GIO_GP(igs), G_PLWIDTH, MG_LWEIGHT(igps))

	# Draw a polyfill connecting the points in the input column(s)
	# Move pen position to last point drawn.
	if (MG_XDATAP(igps) == NULL) {
	    # Y data only;  use pixel numbers for X
	    npts = MG_YNPTS(igps)
	    call malloc (tmpx, npts, TY_REAL)
	    do i = 1, npts
		Memr[tmpx+i-1] = i
	    call gfill (GIO_GP(igs), Memr[tmpx], Memr[MG_YDATAP(igps)],
			npts, MG_FILLPAT(igps))
	    MG_XPOS(igps) = real (npts)
	    MG_YPOS(igps) = Memr[MG_YDATAP(igps)+npts-1]
	    call mfree (tmpx, TY_REAL)
	} else {
	    # Both X and Y data
	    npts = min (MG_XNPTS(igps), MG_YNPTS(igps))
	    call gfill (GIO_GP(igs), Memr[MG_XDATAP(igps)],
			Memr[MG_YDATAP(igps)], npts, MG_FILLPAT(igps))
	    MG_XPOS(igps) = Memr[MG_XDATAP(igps)+npts-1]
	    MG_YPOS(igps) = Memr[MG_YDATAP(igps)+npts-1]
	}

	MG_NPTS(igps) = npts

	# SEMANTICS MODIFICATION: The previous definition required that
	# the pen position does not move.  This is inconsistent with
	# other drawing packages and within igi itself and is undocumented.
	# This version will move the pen position (see above code).  We
	# leave this comment in in case this comes under question again.
	#
	#call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	#
	call gflush (GIO_GP(igs))
end
