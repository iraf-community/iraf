include "igi.h"

#  VPAGE -- Find the GIO viewport edges from the igi virtual page 
#  and viewport edges and set the GIO scale.

#  8/20/91 Removed ^Ls. ZGL

procedure vpage (igs)

pointer	igs			# igi parameters structure

pointer	igps			# Plot parameters structure
real	gvl, gvr, gvb, gvt	# GIO viewport

begin
	igps = PLOT_PARMS(igs)

	call vpc_ndc (igs, MG_VIEWLEFT(igps),  MG_VIEWBOTTOM(igps), gvl, gvb)
	call vpc_ndc (igs, MG_VIEWRIGHT(igps), MG_VIEWTOP(igps),    gvr, gvt)
	call gsview (GIO_GP(igs), gvl, gvr, gvb, gvt)
end


procedure vpc_ndc (igs, vpx, vpy, gvx, gvy)

pointer	igs			# igi parameters structure
real	vpx, vpy		# Viewport in virtual page coordinates
real	gvx, gvy		# GIO viewport (NDC)

pointer	igps			# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	gvx = MG_PAGELEFT(igps) + vpx * 
	    (MG_PAGERIGHT(igps) - MG_PAGELEFT(igps))

	gvy = MG_PAGEBOTTOM(igps) + vpy * 
	    (MG_PAGETOP(igps) - MG_PAGEBOTTOM(igps))
end
