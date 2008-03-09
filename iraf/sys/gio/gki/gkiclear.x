# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_CLEAR -- Clear the workstation screen.
#
# BOI GKI_CLEAR 0
#    
#        L(i)            set to the constant 3 (no data fields)

procedure gki_clear (fd)

int	fd			# output file

size_t	sz_val
pointer	epa
short	gki[GKI_CLEAR_LEN]
data	gki[1] /BOI/, gki[2] /GKI_CLEAR/, gki[3] /LEN_GKIHDR/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_CLEAR]
	    if (epa != 0)
		call zcall1 (epa, 0)
	} else {
	    sz_val = GKI_CLEAR_LEN * SZ_SHORT
	    call write (gk_fd[fd], gki, sz_val)
	}
end
