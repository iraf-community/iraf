# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_REACTIVATEWS -- Reactivate the workstation (enable graphics).
#
# BOI GKI_REACTIVATEWS L F
#    
#        L(i)            4
#        F               flags (0,AW_PAUSE,AW_CLEAR)

procedure gki_reactivatews (fd, flags)

int	fd			# output file
int	flags			# action modifier flags

int	epa, nchars
short	gki[GKI_REACTIVATEWS_LEN]
data	gki[1] /BOI/, gki[2] /GKI_REACTIVATEWS/, gki[3] /GKI_REACTIVATEWS_LEN/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_REACTIVATEWS]
	    if (epa != 0)
		call zcall1 (epa, flags)

	} else {
	    # Send a copy to the pseudofile i/o controller.
	    gki[GKI_REACTIVATEWS_F] = flags
	    nchars = GKI_REACTIVATEWS_LEN * SZ_SHORT
	    if (IS_FILE(fd) && (fd >= STDGRAPH && fd <= STDPLOT)) {
		call write (PSIOCTRL, fd, SZ_INT32)
		call write (PSIOCTRL, gki, nchars)
		call flush (PSIOCTRL)
	    }

	    # Now send a copy to the graphics kernel.
	    call write (gk_fd[fd], gki, nchars)
	}
end
