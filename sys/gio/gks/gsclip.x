# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>

# GSCLIP -- Set clipping flag.

procedure gsclip (iclip)

int	iclip		# New value of clipping flag

begin
	call gsawi (G_CLIP, iclip)
end
