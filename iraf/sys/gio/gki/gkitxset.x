# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>
include	<gio.h>

# GKI_TXSET -- Set the text drawing attributes.
#
# BOI GKI_TXSET L UP SZ SP P HJ VJ F Q CI
#
#        L(i)            12
#        UP(i)           character up vector (degrees)
#        SZ(r)           character size scale factor
#        SP(r)           character spacing
#        P(i)            path (0,1=right,2=left,3=up,4=down)
#        HJ(i)           horizontal justification
#                            (0=normal,1=center,2=left,3=right)
#        VJ(i)           vertical justification
#                            (0=normal,1=center,2=up,3=down)
#        F(i)            font (0,1=roman,2=greek,3=italic,4=bold)
#        Q(i)            quality (0=normal,1=low,2=medium,3=high)
#        CI(i)           text color index

procedure gki_txset (fd, ap)

int	fd			# output file
pointer	ap			# pointer to attribute structure

int	epa
short	gki[GKI_TXSET_LEN]
data	gki[1] /BOI/, gki[2] /GKI_TXSET/, gki[3] /GKI_TXSET_LEN/
include	"gki.com"

begin
	gki[GKI_TXSET_UP] = TX_UP(ap)
	gki[GKI_TXSET_SZ] = GKI_PACKREAL (TX_SIZE(ap))
	gki[GKI_TXSET_SP] = GKI_PACKREAL (TX_SPACING(ap))
	gki[GKI_TXSET_P ] = TX_PATH(ap)
	gki[GKI_TXSET_HJ] = TX_HJUSTIFY(ap)
	gki[GKI_TXSET_VJ] = TX_VJUSTIFY(ap)
	gki[GKI_TXSET_F ] = TX_FONT(ap)
	gki[GKI_TXSET_Q ] = TX_QUALITY(ap)
	gki[GKI_TXSET_CI] = TX_COLOR(ap)

	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_TXSET]
	    if (epa != 0)
		call zcall1 (epa, gki)
	} else
	    call write (gk_fd[fd], gki, GKI_TXSET_LEN * SZ_SHORT)
end
