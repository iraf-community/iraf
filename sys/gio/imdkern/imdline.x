# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gset.h>
include	"imd.h"

# IMD_LINETYPE -- Set the line type option.

procedure imd_linetype (index)

int	index		# index for line type switch statement

int	linetype
include	"imd.com"

begin
	switch (index) {
	case GL_CLEAR:
	    linetype = 0
	case GL_DASHED:
	    linetype = 2
	case GL_DOTTED:
	    linetype = 3
	case GL_DOTDASH:
	    linetype = 4
	default:
	    linetype = 1		# solid
	}

	# This will be done in software in a future version of the IMD kernel.
	# call idk_linetype (g_out, linetype)
end
