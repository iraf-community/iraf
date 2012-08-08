# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GXGTX -- Text.  Ouptut an spp string with gtext.  The string has already
# been unpacked from an f77 to spp string.

procedure gxgtx (px, py, chars)

real	px, py		# Text position in world coordinates
char	chars[ARB]	# String of characters

int	i
include	"gks.com"

begin
	do i = 1, NDEV {
	    if (gk_status[i] == ACTIVE)
		call gtext (gp[i], px, py, chars, "")
	}
end
