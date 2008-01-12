include <gset.h>
include "igi.h"

procedure setltype (igs, ilt)

pointer	igs		# igi parameters structure
int	ilt		# Line type

int	glt		# GIO line type

begin
	if (MG_DRAW(PLOT_PARMS(igs)) == YES) {
	    switch (ilt) {
	    case CLEAR_LINE:
		glt = GL_CLEAR
	    case SOLID_LINE:
		glt = GL_SOLID
	    case DOTTED_LINE:
		glt = GL_DOTTED
	    case SHORT_DASH:
		glt = GL_DASHED
	    case LONG_DASH:
		glt = GL_DASHED
	    case DOT_SHORT_DASH:
		glt = GL_DOTDASH
	    case DOT_LONG_DASH:
		glt = GL_DOTDASH
	    case SHORT_LONG_DASH:
		glt = GL_DASHED
	    default:
		glt = GL_SOLID
	    }
	} else
	    glt = GL_CLEAR

	call gseti (GIO_GP(igs), G_PLTYPE, glt)
	call gseti (GIO_GP(igs), G_PMLTYPE, glt)
end
