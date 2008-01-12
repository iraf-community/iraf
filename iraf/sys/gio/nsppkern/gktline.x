# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gset.h>
include	"gkt.h"

# GKT_LINETYPE -- Set the line type option in the nspp world.

procedure gkt_linetype (index)

int	index		# index for line type switch statement

int	linetype
include	"gkt.com"

begin
	switch (index) {
	    case GL_CLEAR:
		linetype = 0
	    case GL_DASHED:
		linetype = 0FF00X
	    case GL_DOTTED:
		linetype = 08888X
	    case GL_DOTDASH:
		linetype = 0F040X
	    default:
		linetype = 0FFFFX	# GL_SOLID and default
	}

	call optn (*"dp", linetype)
end
