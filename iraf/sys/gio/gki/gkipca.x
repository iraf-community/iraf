# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_PUTCELLARRAY -- Output a cell array (pixel array).
#
# BOI GKI_PUTCELLARRAY L LL UR NC NL P
#    
#        L(i)            9 + (NC * NL)
#        LL(p)           coordinates of lower left corner of output area
#        UR(p)           coordinates of upper right corner of output area
#        NC(i)           number of columns in array
#        NL(i)           number of lines in array
#        P(NCNLi)        array of color indices (pixels) stored by row

procedure gki_putcellarray (fd, m, nx, ny, x1,y1, x2,y2)

int	fd			# output file
int	nx, ny			# number of columns and lines in M
short	m[nx,ny]		# pixel array
int	x1, y1			# lower left corner of window to be written
int	x2, y2			# upper right corner of window to be written

int	epa
short	gki[GKI_PUTCELLARRAY_LEN]
data	gki[1] /BOI/, gki[2] /GKI_PUTCELLARRAY/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_PUTCELLARRAY]
	    if (epa != 0)
		call zcall7 (epa, m, nx,ny, x1,y1, x2,y2)
	} else {
	    gki[GKI_PUTCELLARRAY_L]    = GKI_PUTCELLARRAY_LEN + (nx * ny)
	    gki[GKI_PUTCELLARRAY_LL]   = x1
	    gki[GKI_PUTCELLARRAY_LL+1] = y1
	    gki[GKI_PUTCELLARRAY_UR]   = x2
	    gki[GKI_PUTCELLARRAY_UR+1] = y2
	    gki[GKI_PUTCELLARRAY_NC]   = nx
	    gki[GKI_PUTCELLARRAY_NL]   = ny

	    call write (gk_fd[fd], gki, GKI_PUTCELLARRAY_LEN)
	    call write (gk_fd[fd], m, (nx * ny) * SZ_SHORT)
	}
end
