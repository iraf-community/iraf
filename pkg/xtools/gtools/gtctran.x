# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GT_XCTRAN -- Transform x between two WCS.  Return new value as a function.

real procedure gt_xctran (gp, x1, wcs1, wcs2)

pointer	gp			# GIO pointer
real	x1			# X value to be transformed
int	wcs1			# Input WCS
int	wcs2			# Output WCS

real	x2, y2

begin
	call gctran (gp, x1, 0., x2, y2, wcs1, wcs2)
	return (x2)
end


# GT_YCTRAN -- Transform y between two WCS.  Return new value as a function.

real procedure gt_yctran (gp, y1, wcs1, wcs2)

pointer	gp			# GIO pointer
real	y1			# Y value to be transformed
int	wcs1			# Input WCS
int	wcs2			# Output WCS

real	x2, y2

begin
	call gctran (gp, 0., y1, x2, y2, wcs1, wcs2)
	return (y2)
end
