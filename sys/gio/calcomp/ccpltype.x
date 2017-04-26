# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gset.h>
include	"ccp.h"

# CCP_LINETYPE -- Set the line type option in the nspp world.

procedure ccp_linetype (index)

int	index		# index for line type switch statement

include	"ccp.com"

begin
	switch (index) {
	case GL_CLEAR:
	    g_ltype = 0
	case GL_DASHED:
	    g_ltype = 2
	case GL_DOTTED:
	    g_ltype = 3
	case GL_DOTDASH:
	    g_ltype = 4
	default:
	    g_ltype = 1		# GL_SOLID and default
	}
end
