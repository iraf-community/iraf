# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQPMI -- Inquire Polymarker index.

procedure gqpmi (errind, index)

real	index                   # Polymarker index - returned value.
int	errind			# Error indicator
include	"gks.com"

begin
	errind = 0
	index = 1.0
end
