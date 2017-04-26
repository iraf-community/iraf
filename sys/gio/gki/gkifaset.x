# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>
include	<gio.h>

# GKI_FASET -- Set the fill area attributes.
#
# BOI GKI_FASET L FS CI
#
#        L(i)            5
#        FS(i)           fill style (0=clear,1=hollow,2=solid,3-6=hatch)
#        CI(i)           fill area color index

procedure gki_faset (fd, ap)

int	fd			# output file
pointer	ap			# pointer to fillarea attribute structure

int	epa
short	gki[GKI_FASET_LEN]
data	gki[1] /BOI/, gki[2] /GKI_FASET/, gki[3] /GKI_FASET_LEN/
include	"gki.com"

begin
	gki[GKI_FASET_FS] = FA_STYLE(ap)
	gki[GKI_FASET_CI] = FA_COLOR(ap)

	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_FASET]
	    if (epa != 0)
		call zcall1 (epa, gki)
	} else
	    call write (gk_fd[fd], gki, GKI_FASET_LEN * SZ_SHORT)
end
