# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>
include	<gio.h>

# GKI_PMSET -- Set the polymarker attributes.
#
# BOI GKI_PMSET L MT MW CI
#
#        L(i)            6
#        MT(i)           marktype (not used at present)
#        MW(i)           marksize, NDC coords (not used at present)
#        CI(i)           marker color index

procedure gki_pmset (fd, ap)

int	fd			# output file
pointer	ap			# pointer to polymarker attribute structure

int	epa
short	gki[GKI_PMSET_LEN]
data	gki[1] /BOI/, gki[2] /GKI_PMSET/, gki[3] /GKI_PMSET_LEN/
include	"gki.com"

begin
	gki[GKI_PMSET_MT] = PM_LTYPE(ap)
	gki[GKI_PMSET_MW] = GKI_PACKREAL (PM_WIDTH(ap))
	gki[GKI_PMSET_CI] = PM_COLOR(ap)

	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_PMSET]
	    if (epa != 0)
		call zcall1 (epa, gki)
	} else
	    call write (gk_fd[fd], gki, GKI_PMSET_LEN * SZ_SHORT)
end
