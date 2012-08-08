# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_SETCURSOR -- Set the position of a device cursor.
#
# BOI GKI_SETCURSOR L CN POS
#
#        L(i)            6
#        CN(i)           cursor number
#        POS(p)          new cursor position

procedure gki_setcursor (fd, x, y, cursor)

int	fd			# output file
int	x, y			# new cursor position
int	cursor			# cursor to be set

int	epa
short	gki[GKI_SETCURSOR_LEN]
data	gki[1] /BOI/, gki[2] /GKI_SETCURSOR/, gki[3] /GKI_SETCURSOR_LEN/
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_SETCURSOR]
	    if (epa != 0)
		call zcall3 (epa, x, y, cursor)
	} else {
	    gki[GKI_SETCURSOR_CN]    = cursor
	    gki[GKI_SETCURSOR_POS]   = x
	    gki[GKI_SETCURSOR_POS+1] = y

	    call write (gk_fd[fd], gki, GKI_SETCURSOR_LEN * SZ_SHORT)
	}
end
