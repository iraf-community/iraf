# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSFAIS -- Set fill area interior style.  Integer variable "gk_style" is
# set and stored in "gks.com".  Procedure GFA will use this value.

procedure gsfais (ints)

int	ints		# Fill area interior style

include	"gks.com"

begin
	switch (ints) {
	case GHOLLO:
	    gk_style = GF_HOLLOW
	case GSOLID:
	    gk_style = GF_SOLID
	case GPATTR:
	    gk_style = GF_HATCH4
	case GHATCH:
	    gk_style = GF_HATCH1
	default:
	    gk_style = GF_HOLLOW
	}
end
