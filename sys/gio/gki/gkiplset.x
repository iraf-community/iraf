# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>
include	<gio.h>

# GKI_PLSET -- Set the polyline attributes.
#
# BOI GKI_PLSET L LT LW CI
#
#        L(i)            6
#        LT(i)           linetype number
#        LW(r)           linewidth scale factor
#        CI(i)           polyline color index

procedure gki_plset (fd, ap)

int	fd			# output file
pointer	ap			# pointer to polyline attribute structure

int	epa
short	gki[GKI_PLSET_LEN]
data	gki[1] /BOI/, gki[2] /GKI_PLSET/, gki[3] /GKI_PLSET_LEN/
include	"gki.com"

begin
	gki[GKI_PLSET_LT] = PL_LTYPE(ap)
	gki[GKI_PLSET_LW] = GKI_PACKREAL (PL_WIDTH(ap))
	gki[GKI_PLSET_CI] = PL_COLOR(ap)

	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_PLSET]
	    if (epa != 0)
		call zcall1 (epa, gki)
	} else
	    call write (gk_fd[fd], gki, GKI_PLSET_LEN * SZ_SHORT)
end
