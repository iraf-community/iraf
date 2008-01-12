# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gio.h>

# GGCUR -- Perform a graphics cursor read.  The current graphics cursor is
# read and the cursor value is returned.  On output, CN is the cursor number
# which was read, KEY is the key typed to terminate the cursor read, SX,SY
# are the NDC screen coordinates of the cursor, RASTER is the raster number
# or zero, and RX,RY are the raster-relative coordinates of the cursor.  If
# the device does not support rasters or if the cursor is not in a rasters
# when read, RASTER is zero on output and RX,RY are the same as SX,SY.

int procedure ggcur (gp, cn, key, sx, sy, raster, rx, ry)

pointer gp			#I graphics descriptor
int	cn			#O cursor which was read
int	key			#O key typed or EOF
real	sx, sy			#O screen position of cursor in NDC coordinates
int	raster			#O raster number
real	rx, ry			#O raster position of cursor in NDC coordinates

int	m_sx, m_sy
int	m_rx, m_ry

begin
	call gflush (gp)
	call gki_getcursor (GP_FD(gp), GP_CURSOR(gp),
	    cn, key, m_sx, m_sy, raster, m_rx, m_ry)

	sx = real(m_sx) / GKI_MAXNDC
	sy = real(m_sy) / GKI_MAXNDC
	rx = real(m_rx) / GKI_MAXNDC
	ry = real(m_ry) / GKI_MAXNDC

	return (key)
end
