include <gset.h>
include "igi.h"
include "commands.h"

#  8/20/91 Removed ^Ls. ZGL
## 17 June 1992  Changed MG_QUALITY to MG_FONTSET.  ZGL

procedure ig_xylabel (cmd, igs)

int	cmd		# Command index 
pointer	igs		# igi parameters structure

pointer	sp, line
pointer	igps		# Plot parameters structure

begin
	call lcmdcat (igs, YES)

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	call igstarg (igs, Memc[line], SZ_LINE)
	#call lcmdcat (igs, NO)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Label:  %s ")
		call pargstr (Memc[line])
	}

	igps = PLOT_PARMS(igs)

	switch (cmd) {
	case TITLE:
	    # Horizontal label centered above top X axis
	    if (Memc[line] == EOS || Memc[line] == '\n')
		return

	    call ii_title (igs, Memc[line])

	case XLABEL:
	    # Horizontal label centered under bottom X axis
	    if (Memc[line] == EOS || Memc[line] == '\n')
		# Use current X label
		call strcpy (MG_XLABEL(igps), Memc[Line], SZ_LINE)

	    call ii_xlabel (igs, Memc[line])

	case YLABEL:
	    # Vertical label centered left of left Y axis
	    if (Memc[line] == EOS || Memc[line] == '\n')
		# Use current Y label
		call strcpy (MG_YLABEL(igps), Memc[Line], SZ_LINE)

	    call ii_ylabel (igs, Memc[line])
	}

	call cmdcat (igs, YES)
	call sfree (sp)
end


#  II_TITLE -- Write a horizontal label centered above top X axis

procedure ii_title (igs, label)

pointer	igs		# igi parameters structure
char	label[ARB]

pointer	igps		# igi parameters structure
real	delndc
real	xpos, ypos
real	size
real	angle
int	just
real	vl, vr, vb, vt

begin
	igps = PLOT_PARMS(igs)

	call ggview (GIO_GP(igs), vl, vr, vb, vt)
	size = MG_EXPAND(igps) * MG_CHARSIZE(igps)

	xpos = (vr + vl) / 2.0
	ypos = vt + size

	if (ypos >= MG_PAGETOP(igps))
	    # Outside the window
	    return

	ypos   = min (ypos, MG_PAGETOP(igps))
	delndc = MG_PAGETOP(igps) - ypos
	angle  = 0.0
	just   = 8

	# Coordinates in WCS
	call ndc_wcs (igs, xpos, ypos, xpos, ypos)

	size = min (size, delndc)
	size = size / MG_CHARSIZE(igps)
	call setltype (igs, SOLID_LINE)

	call mgostr (igs, xpos, ypos, label, 
	    size, angle, just, MG_FONTSET(igps))

	call gflush (GIO_GP(igs))
end


#  II_XLABEL -- Write a horizontal label centered below bottom X axis

procedure ii_xlabel (igs, label)

pointer	igs		# igi parameters structure
char	label[ARB]

pointer	igps		# igi parameters structure
real	delndc
real	xpos, ypos
real	size
real	angle
int	just
real	vl, vr, vb, vt

begin
	igps = PLOT_PARMS(igs)

	call ggview (GIO_GP(igs), vl, vr, vb, vt)
	size = MG_EXPAND(igps) * MG_CHARSIZE(igps)

	xpos = (vl + vr) / 2.0
	ypos = vb - 4.0 * size

	if (ypos <= MG_PAGEBOTTOM(igps))
	    # Outside the window
	    return

	ypos   = max (ypos, MG_PAGEBOTTOM(igps))
	delndc = ypos - MG_PAGEBOTTOM(igps)
	angle  = 0.0
	just   = 2

	# Coordinates in WCS
	call ndc_wcs (igs, xpos, ypos, xpos, ypos)

	size = min (size, delndc)
	size = size / MG_CHARSIZE(igps)
	call setltype (igs, SOLID_LINE)

	call mgostr (igs, xpos, ypos, label, 
	    size, angle, just, MG_FONTSET(igps))

	call gflush (GIO_GP(igs))
end


#  II_YLABEL -- Write a vertical label centered left of left Y axis

procedure ii_ylabel (igs, label)

pointer	igs		# igi parameters structure
char	label[ARB]

pointer	igps		# igi parameters structure
real	delndc
real	xpos, ypos
real	size
real	angle
int	just
real	vl, vr, vb, vt

begin
	igps = PLOT_PARMS(igs)

	call ggview (GIO_GP(igs), vl, vr, vb, vt)
	size = MG_EXPAND(igps) * MG_CHARSIZE(igps)

	ypos = (vb + vt) / 2.0
	xpos = vl - 5.0 * size

	if (xpos <= MG_PAGELEFT(igps))
	    # Outside the window
	    return

	xpos   = max (xpos, MG_PAGELEFT(igps))
	delndc = xpos - MG_PAGELEFT(igps)

	# Coordinates in WCS
	call ndc_wcs (igs, xpos, ypos, xpos, ypos)

	size  = min (size, delndc)
	size  = size / MG_CHARSIZE(igps)
	angle = 90.0
	just  = 8

	call setltype (igs, SOLID_LINE)

	call mgostr (igs, xpos, ypos, label, 
	    size, angle, just, MG_FONTSET(igps))

	call gflush (GIO_GP(igs))
end


procedure ndc_wcs (igs, nx, ny, wx, wy)

pointer	igs		# igi parameters structure
real	nx, ny		# NDC
real	wx, wy		# WCS

int	wcs

int	gstati()

begin
	wcs = gstati (GIO_GP(igs), G_WCS)
	call ig_gctran (GIO_GP(igs), nx, ny, wx, wy, 0, wcs)
end
