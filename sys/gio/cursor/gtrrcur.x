# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

# GTR_READCURSOR -- Read the graphics cursor position in NDC coordinates.
# By the time we are called the plot has already been drawn and the
# workstation closed, hence we must reopen the workstation to read the
# cursor (the graphics terminal will not be in graphics mode otherwise).

int procedure gtr_readcursor (fd, key, sx, sy, raster, rx, ry)

int	fd			#I graphics stream
int	key			#O keystroke value
real	sx, sy			#O NDC screen coords of cursor
int	raster			#O raster number
real	rx, ry			#O NDC raster coords of cursor

int	cn
int	m_sx, m_sy
int	m_rx, m_ry

begin
	call gki_getcursor (fd, 0,
	    cn, key, m_sx, m_sy, raster, m_rx, m_ry)

	sx = real(m_sx) / GKI_MAXNDC
	sy = real(m_sy) / GKI_MAXNDC
	rx = real(m_rx) / GKI_MAXNDC
	ry = real(m_ry) / GKI_MAXNDC

	return (key)
end
