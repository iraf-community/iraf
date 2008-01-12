include <gset.h>
include "igi.h"

#  IG_DOT -- Draw a marker at the current position in the current style, 
#  size, and angle.

#  8/20/91 Removed ^Ls. ZGL
## 8/5/92  Make consistent with mgpoint changes.  ZGL

procedure ig_dot (igs)

pointer	igs		# igi parameters structure

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)
	call ii_dot  (igs)
end


procedure ii_dot (igs)

pointer	igs		# igi parameters structure

pointer	igps		# Parameters structure descriptor

begin
	igps = PLOT_PARMS(igs)

	call gseti (GIO_GP(igs), G_CLIP, NO)

	call mgpoint (igs, MG_XPOS(igps), MG_YPOS(igps), 
	    MG_PTYPN(igps), MG_PTYPS(igps), MG_EXPAND(igps))

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	call gflush (GIO_GP(igs))
end
