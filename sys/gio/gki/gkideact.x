# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_DEACTIVATEWS -- Deactivate the workstation (disable graphics).
#
# BOI GKI_DEACTIVATEWS L F
#    
#        L(i)            4
#        F               flags (0,AW_PAUSE,AW_CLEAR)

procedure gki_deactivatews (fd, flags)

int	fd			# output file
int	flags			# action modifier flags

int	epa, nchars
short	gki[GKI_DEACTIVATEWS_LEN]
data	gki[1] /BOI/, gki[2] /GKI_DEACTIVATEWS/, gki[3] /GKI_DEACTIVATEWS_LEN/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_DEACTIVATEWS]
	    if (epa != 0)
		call zcall1 (epa, flags)

	} else {
	    # Send a copy to the pseudofile i/o controller.
	    gki[GKI_DEACTIVATEWS_F] = flags
	    nchars = GKI_DEACTIVATEWS_LEN * SZ_SHORT
	    if (IS_FILE(fd) && (fd >= STDGRAPH && fd <= STDPLOT)) {
		call write (PSIOCTRL, fd, SZ_INT32)
		call write (PSIOCTRL, gki, nchars)
		call flush (PSIOCTRL)
	    }

	    # Now send a copy to the graphics kernel.
	    call write (gk_fd[fd], gki, nchars)
	}
end
