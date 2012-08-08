# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_FILLAREA -- Output the fill area instruction.
#
# BOI GKI_FILLAREA L N P
#    
#        L(i)            4 + (N * 2)
#        N(i)            number of points defining the polygon to be filled
#        P(Np)           list of points (x,y pairs)

procedure gki_fillarea (fd, points, npts)

int	fd			# output file
short	points[ARB]		# polygon defining area to be filled
int	npts			# number of (x,y) points in polygon

int	epa
short	gki[GKI_FILLAREA_LEN]
data	gki[1] /BOI/, gki[2] /GKI_FILLAREA/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_FILLAREA]
	    if (epa != 0)
		call zcall2 (epa, points, npts)
	} else {
	    gki[GKI_FILLAREA_L] = GKI_FILLAREA_LEN + (npts * 2)
	    gki[GKI_FILLAREA_N] = npts

	    call write (gk_fd[fd], gki, GKI_FILLAREA_LEN * SZ_SHORT)
	    call write (gk_fd[fd], points, (npts * 2) * SZ_SHORT)
	}
end
