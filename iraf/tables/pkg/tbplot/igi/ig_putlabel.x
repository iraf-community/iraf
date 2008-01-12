include <gset.h>
include "igi.h"

#  8/20/91 Removed ^Ls. ZGL
## 17 June 1992.  Changed MG_QUALITY to MG_FONTSET.  ZGL


procedure ig_putlabel (igs)

pointer	igs		# igi parameters structure

int	just
pointer	sp, label, cjust

int	get_int()

errchk	get_int

begin
	call lcmdcat (igs, YES)

	iferr (just = get_int (igs))
	    return

	call smark (sp)
	call salloc (label, SZ_LINE, TY_CHAR)
	call igstarg (igs, Memc[label], SZ_LINE)

	call ii_putlabel (igs, just, Memc[label])
	call cmdcat (igs, YES)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call salloc (cjust, SZ_LINE, TY_CHAR)
	    call justic (just, Memc[cjust], SZ_LINE)
	    call eprintf ("Justify:  %s (%d);  Label:  %s")
		call pargstr (Memc[cjust])
		call pargi (just)
		call pargstr (Memc[label])
	}

	call sfree (sp)
end


procedure ii_putlabel (igs, just, label)

pointer	igs		# igi parameters structure
int	just		# Justification code
char	label[ARB]

pointer	igps		# parameters structure

begin
	igps = PLOT_PARMS(igs)

	call setltype (igs, SOLID_LINE)
	call gseti (GIO_GP(igs), G_CLIP, NO)

	call mgostr (igs, MG_XPOS(igps), MG_YPOS(igps), label, 
	    MG_EXPAND(igps), MG_ANGLE(igps), just, MG_FONTSET(igps))

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	call gflush (GIO_GP(igs))
end
