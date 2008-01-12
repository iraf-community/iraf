include <gset.h>
include "igi.h"

#  IG_POINTS -- Draw markers at the coordinates in the input column(s)

#  8/20/91 Removed ^Ls. ZGL
## 8/3/92  Simplified calling sequences
## (use structure for angle, fill pattern).  ZGL

procedure ig_points (igs)

pointer	igs		# Parameters structure

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	if (MG_YDATAP(PLOT_PARMS(igs)) == NULL) {
	    call eprintf ("No Y data ")
	    return
	}

	call ii_points (igs)
end


#  II_POINTS -- Draw markers at the coordinates in the input column(s)

procedure ii_points (igs)

pointer	igs		# Parameters structure

int	npts
pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	if (MG_YDATAP(igps) == NULL)
	    return

	call gseti (GIO_GP(igs), G_CLIP, YES)

	# Y data exists
	if (MG_XDATAP(igps) == NULL) {
	    # Y data only;  use pixel numbers for X
	    npts = MG_YNPTS(igps)
	    if (MG_PDATAP(igps) == NULL)
		# No points data;  use global style
		call mgvpnt (igs, Memr[MG_YDATAP(igps)], 
		    npts, MG_PTYPN(igps), MG_PTYPS(igps), MG_EXPAND(igps))
	    else
		# Use individual marker styles
		call mgvmpnt (igs, Memr[MG_YDATAP(igps)], 
		    Memr[MG_PDATAP(igps)], npts, MG_EXPAND(igps))

	    MG_XPOS(igps) = real (npts)
	    MG_YPOS(igps) = Memr[MG_YDATAP(igps)+npts-1]
	} else {
	    # Both X and Y data
	    npts = min (MG_XNPTS(igps), MG_YNPTS(igps))
	    if (MG_PDATAP(igps) == NULL)
		# No points data;  use global style
		call mgppnt (igs, 
		    Memr[MG_XDATAP(igps)], Memr[MG_YDATAP(igps)], 
		    npts, MG_PTYPN(igps), MG_PTYPS(igps), MG_EXPAND(igps))
	    else
		# Use individual marker styles
		call mgpmpnt (igs, 
		    Memr[MG_XDATAP(igps)], Memr[MG_YDATAP(igps)], 
		    Memr[MG_PDATAP(igps)], npts, MG_EXPAND(igps))
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


procedure mgvmpnt (mgp, ydata, pdata, npts, expand)

pointer	mgp
real	ydata[ARB]
real	pdata[ARB]
int	npts
real	expand

int	pix
int	ptypen
int	ptypes
real	size

begin
	do pix = 1, npts {
	    call ptypns (pdata[pix], ptypen, ptypes, size)
	    size = size * expand
	    call mgpoint (mgp, real (pix), ydata[pix], ptypen, ptypes, size)
	}
end


procedure mgpmpnt (mgp, xdata, ydata, pdata, npts, expand)

pointer	mgp
real	xdata[ARB]
real	ydata[ARB]
real	pdata[ARB]
int	npts
real	expand

int	pix
int	ptypen
int	ptypes
real	size

begin
	do pix = 1, npts {
	    call ptypns (pdata[pix], ptypen, ptypes, size)
	    size = size * expand
	    call mgpoint (mgp, xdata[pix], ydata[pix], ptypen, ptypes, size)
	}
end


procedure mgvpnt (mgp, ydata, npts, ptypen, ptypes, size)

pointer	mgp
real	ydata[ARB]
int	npts
int	ptypen
int	ptypes
real	size

int	pix

begin
	do pix = 1, npts
	    call mgpoint (mgp, real (pix), ydata[pix], ptypen, ptypes, size)
end


procedure mgppnt (mgp, xdata, ydata, npts, ptypen, ptypes, size)

pointer	mgp
real	xdata[ARB]
real	ydata[ARB]
int	npts
int	ptypen
int	ptypes
real	size

int	pix

begin
	do pix = 1, npts {
	    call mgpoint (mgp, xdata[pix], ydata[pix], ptypen, ptypes, size)
	}
end


#  PTYPNS -- Decode a real valued point style code into the style code, 
#  number of vertices, and size.

procedure ptypns (pdata, ptypen, ptypes, size)

real	pdata
int	ptypen
int	ptypes
real	size

bool	fp_equalr()

begin
	ptypen = int (pdata) / 10
	ptypes = int (pdata) - (ptypen * 10)

	if (pdata < 1.0)
	    size = pdata
	else
	    size = mod (pdata, real (int (pdata)))

	if (fp_equalr (size, 0.0))
	    size = 1.0
end
