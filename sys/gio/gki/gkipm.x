# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_POLYMARKER -- Output a polymarker.
#
# BOI GKI_POLYMARKER L N P
#    
#        L(i)            4 + (N * 2)
#        N(i)            number of points in the polymarker
#        P(Np)           list of points (x,y pairs)

procedure gki_polymarker (fd, points, npts)

int	fd			# output file
short	points[ARB]		# polymarker
int	npts			# number of (x,y) points in polymarker

int	epa
short	gki[GKI_POLYMARKER_LEN]
data	gki[1] /BOI/, gki[2] /GKI_POLYMARKER/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_POLYMARKER]
	    if (epa != 0)
		call zcall2 (epa, points, npts)
	} else {
	    gki[GKI_POLYMARKER_L] = GKI_POLYMARKER_LEN + (npts * 2)
	    gki[GKI_POLYMARKER_N] = npts

	    call write (gk_fd[fd], gki, GKI_POLYMARKER_LEN * SZ_SHORT)
	    call write (gk_fd[fd], points, (npts * 2) * SZ_SHORT)
	}
end
