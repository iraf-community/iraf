# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQPMI -- Inquire Polymarker index.

procedure gqpmi (errind, index)

int	errind			# Error indicator
real	index                   # Polymarker index - returned value.

include	"gks.com"

begin
	errind = 0
	index = 1.0
end
