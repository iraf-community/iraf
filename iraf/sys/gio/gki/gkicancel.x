# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_CANCEL -- Cancel graphics output and reset internal parameters.
#
# BOI GKI_CANCEL 0
#    
#        L(i)            set to the constant 3 (no data fields)

procedure gki_cancel (fd)

int	fd			# output file

int	epa
short	gki[GKI_CANCEL_LEN]
data	gki[1] /BOI/, gki[2] /GKI_CANCEL/, gki[3] /LEN_GKIHDR/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_CANCEL]
	    if (epa != 0)
		call zcall1 (epa, 0)
	} else
	    call write (gk_fd[fd], gki, GKI_CANCEL_LEN * SZ_SHORT)
end
