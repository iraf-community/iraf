# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>

# GSELNT -- Select normalization transformation (same as world coord sys)

procedure gselnt (wcs)

int	wcs		# Transformation number

begin
	call gsawi (G_WCS, wcs)
end
