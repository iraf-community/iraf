# AST_COORD -- Convert spherical coordinates to new system.
#
# This procedure converts the longitude-latitude coordinates (a1, b1)
# of a point on a sphere into corresponding coordinates (a2, b2) in a
# different coordinate system that is specified by the coordinates of its
# origin (ao, bo).  The range of a2 will be from -pi to pi.

procedure ast_coord (ao, bo, ap, bp, a1, b1, a2, b2)

double	ao, bo		# Origin of new coordinates (radians)
double	ap, bp		# Pole of new coordinates (radians)
double	a1, b1		# Coordinates to be converted (radians)
double	a2, b2		# Converted coordinates (radians)

double	sao, cao, sbo, cbo, sbp, cbp
double	x, y, z, xp, yp, zp, temp

begin
	x = cos (a1) * cos (b1)
	y = sin (a1) * cos (b1)
	z = sin (b1)
	xp = cos (ap) * cos (bp)
	yp = sin (ap) * cos (bp)
	zp = sin (bp)

	# Rotate the origin about z.
	sao = sin (ao)
	cao = cos (ao)
	sbo = sin (bo)
	cbo = cos (bo)
	temp = -xp * sao + yp * cao
	xp = xp * cao + yp * sao
	yp = temp
	temp = -x * sao + y * cao
	x = x * cao + y * sao
	y = temp

	# Rotate the origin about y.
	temp = -xp * sbo + zp * cbo
	xp = xp * cbo + zp * sbo
	zp = temp
	temp = -x * sbo + z * cbo
	x = x * cbo + z * sbo
	z = temp

	# Rotate pole around x.
	sbp = zp
	cbp = yp
	temp = y * cbp + z * sbp
	y = y * sbp - z * cbp
	z = temp

	# Final angular coordinates.
	a2 = atan2 (y, x)
	b2 = asin (z)
end
