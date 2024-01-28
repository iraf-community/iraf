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

double	radians_per_degree, pi, two_pi, st, d, dd
double	ma, sin_ma, sin_two_ma, ml, e, e_squared, e_cubed
double	ep, ea, r, image_r_squared, tl
double	lan, bn, p, p_degrees
double	sl1, sl2, cldc, cos_bn, x, cl1
double	sin_three_ma, sec_bn, y
double	dd_squared, dd_cubed, c, s, cl2, sln
int	mac[12]

data	mac/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/

begin
	# This version ignores lunar and planetary perturbations.
	radians_per_degree = .017453292519943d+0
	pi = 3.1415926536d+0
	two_pi = pi + pi

	d = double(365 * year + (year - 1)/4 + mac[month] + day)
	if (month >= 3 && mod(year, 4) == 0)
	    d = d + 1.d+0
	st = double(second / 3600. + minute / 60. + hour)
	d = d + st/24.d+0 -.5d+0
	dd = d / 10000.d+0
	dd_squared = dd * dd
	dd_cubed = dd * dd * dd

	# Mean anomaly.
	ma = radians_per_degree * (358.475845d+0 + .985600267d+0 *
	    d - 1.12d-5 * dd_squared - 7.d-8 * dd_cubed)
	ma = mod(ma, two_pi)
	sin_ma = sin(ma)
	sin_two_ma = sin(2.d+0 * ma)
	sin_three_ma = sin(3.d+0 * ma)

	# Mean longitude.
	ml = radians_per_degree *
	    (279.696678d+0 + .9856473354d+0 * d + 2.267d-5 *  dd_squared)
	ml = mod(ml, two_pi)

	# Ecentricity.
	e = 0.01675104d+0 - 1.1444d-5 * dd - 9.4d-9 * dd_squared
	e_squared = e * e
	e_cubed = e_squared * e

	# Obliquity.
	ep = radians_per_degree * (23.452294d+0 -
	    3.5626d-3 * dd - 1.23d-7 * dd_squared + 1.03d-8 * dd_cubed)

	# Eccentric anomaly.
	ea = ma + (e - e_cubed/8.d+0) * sin_ma + e_squared * sin_two_ma/2.d+0 +
	    3.d+0 * e_cubed * sin_three_ma/8.d+0

	# Radius vector.
	r = 1.00000003d+0 * (1.d+0 - e * cos(ea))

	# Image radius.
	image_r = real(961.18d+0 / r)
	image_r_squared = double(image_r * image_r)

	# True longitude.
	tl = ml + (2.d+0 * e - e_cubed/4.d+0) * sin_ma + 5.d+0 * e_squared *
	    sin_two_ma/4.d+0 + 13.d+0 * e_cubed * sin_three_ma/12.d+0
	tl = mod(tl, two_pi)

	# Longitude of ascending node of solar equator.
	lan = radians_per_degree * (73.666667d+0 + 0.0139583d+0 *
	    (year + 50.d+0))

	# Solar latitude of sub-earth point.
	bn = asin(sin(tl - lan) * .12620d+0)
	bn_degrees = real(bn / radians_per_degree)
	if (verbose) {
	    call printf("B0 (degrees) = %10.5f\n")
	        call pargr(bn_degrees)
	}

	# Position angle of rotation axis.
	p = atan(-cos(tl) * tan(ep)) + atan(-cos(tl - lan) * .12722d+0)
	p_degrees = p/radians_per_degree
	if (verbose) {
	    call printf("P-angle (degrees) = %10.5f\n")
	        call pargr(real(p_degrees))
	}

	# Carrington longitude of disk center.
	sl1 = (d + 16800.d+0) * 360.d+0/25.38d+0
	sl2 = mod(sl1, 360.d+0)
	sln = 360.d+0 - sl2
	sln = radians_per_degree * sln

	cos_bn = cos(bn)
	sec_bn = 1.d+0/cos_bn
	c = +1.d+0
	s = +1.d+0
	x = -sec_bn * cos(tl - lan)
	if (x < 0.)
	    c = -1.d+0
	y = -sec_bn * sin(tl - lan) * .99200495d+0
	if (y < 0.)
	    s = -1.d+0

	cl1 = tan(tl - lan) * 0.99200495d+0
	cl2 = atan(cl1)
	if (s == 1.d+0 && c == 1.d+0)
	    cldc = sln + cl2
	if (s == -1.d+0 && c == -1.d+0)
	    cldc = sln + cl2 + pi
	if (s == 1.d+0 && c == -1.d+0)
	    cldc = sln + cl2 + pi
	if (s == -1.d+0 && c == 1.d+0)
	    cldc = sln + cl2
	if (cldc < 0.d+0)
	    cldc = cldc + two_pi
	if (cldc > two_pi)
	    cldc = mod(cldc, two_pi)

	cldc_degrees = real(cldc / radians_per_degree)
	if (verbose) {
	    call printf ("L0 (degrees) = %10.5f\n")
	        call pargr (cldc_degrees)
	}
end
