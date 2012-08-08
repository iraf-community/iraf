# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GLINE -- Draw a line connecting two points.

procedure gline (gp, x1, y1, x2, y2)

pointer	gp			# graphics descriptor
real	x1, y1			# first point
real	x2, y2			# second point

begin
	call gamove (gp, x1, y1)
	call gadraw (gp, x2, y2)
end
