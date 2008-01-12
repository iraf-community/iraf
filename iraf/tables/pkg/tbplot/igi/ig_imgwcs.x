include "igi.h"

procedure ig_imgwcs (igs)

#  IG_IMGWCS -- Toggle the internal igi parameter that specifies whether
#  to use an image WCS to fill the X buffer when reading an image section.

pointer	igs		# Parameters structure

int	igps

begin
	call lcmdcat (igs, YES)
	igps = PLOT_PARMS(igs)

	if (MG_IMGWCS(igps) == YES)
	    MG_IMGWCS(igps) = NO
	else
	    MG_IMGWCS(igps) = YES
end
