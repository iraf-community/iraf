include <gset.h>
include "igi.h"

#  8/20/91 Removed ^Ls. ZGL
## 16 June 1992  Changed MG_QUALITY to MG_FONTSET. ZGL

procedure ig_label (igs)

pointer	igs		# igi parameters structure

pointer	sp, line

begin
	call lcmdcat (igs, YES)

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	call igstarg (igs, Memc[line], SZ_LINE)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Label:  %s ")
		call pargstr (Memc[line])
	}

	call ii_label (igs, Memc[line])

	call cmdcat (igs, YES)
	call sfree  (sp)
end


procedure ii_label (igs, label)

pointer	igs		# igi parameters structure
char	label[ARB]

pointer	igps		# parameters structure

begin
	call gseti (GIO_GP(igs), G_CLIP, NO)

	igps = PLOT_PARMS(igs)

	call setltype (igs, SOLID_LINE)

	call mgostr (igs, MG_XPOS(igps), MG_YPOS(igps), 
	    label, MG_EXPAND(igps), MG_ANGLE(igps), 
	    MG_IJUSTC(igps), MG_FONTSET(igps))

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	call gflush (GIO_GP(igs))
end
