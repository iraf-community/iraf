# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>

# GSTXCI -- Set colour index.  This function is currently implemented
# by setting the text font to bold when the color index > 1, and to
# the default (roman) otherwise.

procedure gstxci (coli)

int	coli		# Text colour index

begin
	if (coli > 1)
	    call gsawi (G_TXFONT, GT_BOLD)
	else 
	    call gsawi (G_TXFONT, GT_ROMAN)
end
