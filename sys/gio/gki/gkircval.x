# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

# GKI_RETCURSORVALUE -- Return a cursor value.  Used by a graphics kernel to
# return a cursor value to GIO in response to a GETCURSOR instruction.
#
#                BOI GKI_CURSORVALUE L CN KEY SX SY RN RX RY
#
#        where
#
#                L(i)            10
#                CN(i)           cursor number
#                KEY(i)          keystroke value (>= 0 or EOF)
#                SX(i)           NDC X screen coordinate of cursor
#                SY(i)           NDC Y screen coordinate of cursor
#                RN(i)           raster number or zero
#                RX(i)           NDC X raster coordinate of cursor
#                RY(i)           NDC Y raster coordinate of cursor
#
# The screen or display window coordinates SX and SY of the cursor are
# returned for all devices.  Only some devices support multiple rasters.
# If the device supports rasters and the cursor is in a raster when read, the
# raster number and raster coordinates are returned in RN,RX,RY.  This is in
# addition to the screen coordinates SX,SY.  If raster coordinates are not
# returned, the raster number will be set to zero and RX,RY will be the same
# as SX,SY.

procedure gki_retcursorvalue (fd, cn, key, sx, sy, raster, rx, ry)

int	fd			#I output file
int	cn			#I cursor number
int	key			#I keystroke value
int	sx, sy			#I screen coordinates of cursor (GKI coords)
int	raster			#I raster number
int	rx, ry			#I raster coordinates of cursor (GKI coords)

short	gki[GKI_CURSORVALUE_LEN]
data	gki[1] /BOI/, gki[2] /GKI_CURSORVALUE/, gki[3] /GKI_CURSORVALUE_LEN/

begin
	gki[GKI_CURSORVALUE_CN ] = cn
	gki[GKI_CURSORVALUE_KEY] = key
	gki[GKI_CURSORVALUE_SX ] = sx
	gki[GKI_CURSORVALUE_SY ] = sy
	gki[GKI_CURSORVALUE_RN ] = raster
	gki[GKI_CURSORVALUE_RX ] = rx
	gki[GKI_CURSORVALUE_RY ] = ry

	call write (fd, gki, GKI_CURSORVALUE_LEN * SZ_SHORT)
end
