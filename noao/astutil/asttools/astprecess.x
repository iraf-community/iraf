include	<math.h>

# AST_PRECESS -- Precess coordinates from epoch1 to epoch2.
#
# The method used here is based on the new IAU system described in the
# supplement to the 1984 Astronomical Almanac.  The precession is
# done in two steps; precess epoch1 to the standard epoch J2000.0 and then
# precess from the standard epoch to epoch2.  The precession between
# any two dates is done this way because the rotation matrix coefficients
# are given relative to the standard epoch.

procedure ast_precess (ra1, dec1, epoch1, ra2, dec2, epoch2)

double	ra1, dec1, epoch1		# First coordinates
double	ra2, dec2, epoch2		# Second coordinates

double	r0[3], r1[3], p[3, 3]
bool	fp_equald()

begin
	# If the input epoch is 0 or undefined then assume the input epoch
	# is the same as the output epoch.  If the two epochs are the same
	# then return the coordinates from epoch1.

	if ((epoch1 == 0.) || IS_INDEFD (epoch1) || fp_equald(epoch1, epoch2)) {
	    ra2 = ra1
	    dec2 = dec1
	    return
	}

	# Rectangular equitorial coordinates (direction cosines).
	ra2 = DEGTORAD (ra1 * 15.)
	dec2 = DEGTORAD (dec1)

	r0[1] = cos (ra2) * cos (dec2)
	r0[2] = sin (ra2) * cos (dec2)
	r0[3] = sin (dec2)

	# If epoch1 is not the standard epoch then precess to the standard
	# epoch.

	if (epoch1 != 2000.) {
	    call ast_rotmatrix (epoch1, p)

	    # Note that we multiply by the inverse of p which is the
	    # transpose of p.

	    r1[1] = p[1, 1] * r0[1] + p[1, 2] * r0[2] + p[1, 3] * r0[3]
	    r1[2] = p[2, 1] * r0[1] + p[2, 2] * r0[2] + p[2, 3] * r0[3]
	    r1[3] = p[3, 1] * r0[1] + p[3, 2] * r0[2] + p[3, 3] * r0[3]
	    r0[1] = r1[1]
	    r0[2] = r1[2]
	    r0[3] = r1[3]
	}

	# If epoch2 is not the standard epoch then precess from the standard
	# epoch to the desired epoch.

	if (epoch2 != 2000.) {
	    call ast_rotmatrix (epoch2, p)
	    r1[1] = p[1, 1] * r0[1] + p[2, 1] * r0[2] + p[3, 1] * r0[3]
	    r1[2] = p[1, 2] * r0[1] + p[2, 2] * r0[2] + p[3, 2] * r0[3]
	    r1[3] = p[1, 3] * r0[1] + p[2, 3] * r0[2] + p[3, 3] * r0[3]
	    r0[1] = r1[1]
	    r0[2] = r1[2]
	    r0[3] = r1[3]
	}

	# Convert from radians to hours and degrees.
	ra2 = RADTODEG (atan2 (r0[2], r0[1]) / 15.)
	dec2 = RADTODEG (asin (r0[3]))
	if (ra2 < 0.)
	    ra2 = ra2 + 24
end


# ROTMATRIX -- Compute the precession rotation matrix from the standard epoch
# J2000.0 to the specified epoch.

procedure ast_rotmatrix (epoch, p)

double	epoch		# Epoch of date
double	p[3, 3]		# Rotation matrix

double	t, a, b, c, ca, cb, cc, sa, sb, sc
double	ast_julday()

begin
	# The rotation matrix coefficients are polynomials in time measured
	# in Julian centuries from the standard epoch.  The coefficients are
	# in degrees.

	t = (ast_julday (epoch) - 2451545.0d0) / 36525d0

	a = t * (0.6406161d0 + t * (0.0000839d0 + t * 0.0000050d0))
	b = t * (0.6406161d0 + t * (0.0003041d0 + t * 0.0000051d0))
	c = t * (0.5567530d0 - t * (0.0001185d0 + t * 0.0000116d0))

	# Compute the cosines and sines once for efficiency.
	ca = cos (DEGTORAD (a))
	sa = sin (DEGTORAD (a))
	cb = cos (DEGTORAD (b))
	sb = sin (DEGTORAD (b))
	cc = cos (DEGTORAD (c))
	sc = sin (DEGTORAD (c))

	# Compute the rotation matrix from the sines and cosines.
	p[1, 1] = ca * cb * cc - sa * sb
	p[2, 1] = -sa * cb * cc - ca * sb
	p[3, 1] = -cb * sc
	p[1, 2] = ca * sb * cc + sa * cb
	p[2, 2] = -sa * sb * cc + ca * cb
	p[3, 2] = -sb * sc
	p[1, 3] = ca * sc
	p[2, 3] = -sa * sc
	p[3, 3] = cc
end
