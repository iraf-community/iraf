# EPHEM -- Calculate ephemeris data for the sun, return latitude and
# longitude of sub-earth point.

procedure ephem (month, day, year, hour, minute, second, image_r,
	bn_degrees, cldc_degrees, verbose)
 
int	month			# time of observation
int	day			#
int	year			#
int	hour			#
int	minute			#
int	second			#
real	image_r			# image radius
real	bn_degrees		# solar latitude of sub-earth point (degrees)
real	cldc_degrees		# Carrington longitude of disk center
bool	verbose			# verbose flag

real	radians_per_degree, pi, two_pi, st, d, dd
real	ma, sin_ma, sin_two_ma, ml, e, e_squared, e_cubed
real	ep, ea, r, image_r_squared, tl
real	lan, bn, p, p_degrees
real	sl1, sl2, cldc, cos_bn, x, cl1
real	sin_three_ma, sec_bn, y
real	dd_squared, dd_cubed, c, s, cl2, sln
int	mac[12]

data	mac/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/

begin
	# This version ignores lunar and planetary perturbations.
	radians_per_degree = .017453292519943
	pi = 3.1415926536
	two_pi = pi + pi

	d = 365 * year + (year - 1)/4 + mac[month] + day
	if (month >= 3 && mod(year, 4) == 0)
	    d = d + 1.
	st = second / 3600. + minute / 60. + hour
	d = d + st/24. -.5
	dd = d / 10000.
	dd_squared = dd * dd
	dd_cubed = dd * dd * dd

	# Mean anomaly.
	ma = radians_per_degree * (358.475845 + .985600267 * d - 1.12e-5 *
	    dd_squared - 7.e-8 * dd_cubed)
	ma = mod(ma, two_pi)
	sin_ma = sin(ma)
	sin_two_ma = sin(2. * ma)
	sin_three_ma = sin(3. * ma)

	# Mean longitude.
	ml = radians_per_degree *
	    (279.696678 + .9856473354 * d + 2.267e-5 *  dd_squared)
	ml = mod(ml, two_pi)

	# Ecentricity.
	e = 0.01675104 - 1.1444e-5 * dd - 9.4e-9 * dd_squared
	e_squared = e * e
	e_cubed = e_squared * e

	# Obliquity.
	ep = radians_per_degree * (23.452294 -
	    3.5626e-3 * dd - 1.23e-7 * dd_squared + 1.03e-8 * dd_cubed)

	# Eccentric anomaly.
	ea = ma + (e - e_cubed/8.) * sin_ma + e_squared * sin_two_ma/2. + 3. *
	    e_cubed * sin_three_ma/8.

	# Radius vector.
	r = 1.00000003 * (1. - e * cos(ea))

	# Image radius.
	image_r = 961.18 / r
	image_r_squared = image_r * image_r

	# True longitude.
	tl = ml + (2. * e - e_cubed/4.) * sin_ma + 5. * e_squared *
	    sin_two_ma/4. + 13. * e_cubed * sin_three_ma/12.
	tl = mod(tl, two_pi)

	# Longitude of ascending node of solar equator.
	lan = radians_per_degree * (73.666667 + 0.0139583 * (year + 50.))

	# Solar latitude of sub-earth point.
	bn = asin(sin(tl - lan) * .12620)
	bn_degrees = bn / radians_per_degree
	if (verbose) {
	    call printf("B0 (degrees) = %10.5f\n")
	        call pargr(bn_degrees)
	}

	# Position angle of rotation axis.
	p = atan(-cos(tl) * tan(ep)) + atan(-cos(tl - lan) * .12722)
	p_degrees = p/radians_per_degree
	if (verbose) {
	    call printf("P-angle (degrees) = %10.5f\n")
	        call pargr(p_degrees)
	}

	# Carrington longitude of disk center.
	sl1 = (d + 16800.) * 360./25.38
	sl2 = mod(sl1, 360.)
	sln = 360. - sl2
	sln = radians_per_degree * sln

	cos_bn = cos(bn)
	sec_bn = 1./cos_bn
	c = +1.
	s = +1.
	x = -sec_bn * cos(tl - lan)
	if (x < 0.)
	    c = -1.
	y = -sec_bn * sin(tl - lan) * .99200495
	if (y < 0.)
	    s = -1.

	cl1 = tan(tl - lan) * 0.99200495
	cl2 = atan(cl1)
	if (s == 1. && c == 1.)
	    cldc = sln + cl2
	if (s == -1. && c == -1.)
	    cldc = sln + cl2 + pi
	if (s == 1. && c == -1.)
	    cldc = sln + cl2 + pi
	if (s == -1. && c == 1.)
	    cldc = sln + cl2
	if (cldc < 0.)
	    cldc = cldc + two_pi
	if (cldc > two_pi)
	    cldc = mod(cldc, two_pi)

	cldc_degrees = cldc / radians_per_degree
	if (verbose) {
	    call printf ("L0 (degrees) = %10.5f\n")
	        call pargr (cldc_degrees)
	}
end
