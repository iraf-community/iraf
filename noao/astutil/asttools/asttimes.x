define	J2000		2000.0D0		# J2000
define	JD2000		2451545.0D0		# J2000 Julian Date
define	JYEAR		365.25D0		# Julian year


# AST_DATE_TO_EPOCH -- Convert Gregorian date and solar mean time to
# a Julian epoch.  A Julian epoch has 365.25 days per year and 24
# hours per day.

procedure ast_date_to_epoch (year, month, day, ut, epoch)

int	year			# Year
int	month			# Month (1-12)
int	day			# Day of month
double	ut			# Universal time for date (mean solar day)
double	epoch			# Julian epoch

double	jd, ast_date_to_julday()

begin
	jd = ast_date_to_julday (year, month, day, ut)
	epoch = J2000 + (jd - JD2000) / JYEAR
end


# AST_EPOCH_TO_DATE -- Convert a Julian epoch to year, month, day, and time.

procedure ast_epoch_to_date (epoch, year, month, day, ut)

double	epoch			# Julian epoch
int	year			# Year
int	month			# Month (1-12)
int	day			# Day of month
double	ut			# Universal time for date

double	jd

begin
	jd = JD2000 + (epoch - J2000) * JYEAR
	call ast_julday_to_date (jd, year, month, day, ut)
end


# AST_DAY_OF_YEAR -- The day number for the given year is returned.

int procedure ast_day_of_year (year, month, day)

int	year			# Year
int	month			# Month (1-12)
int	day			# Day of month

int	d
int	bom[13]			# Beginning of month
data	bom/1,32,60,91,121,152,182,213,244,274,305,335,366/

begin
	d = bom[month] + day - 1
	if (month > 2 && mod (year, 4) == 0 &&
	    (mod (year, 100) != 0 || mod (year, 400) == 0))
	    d = d + 1
	return (d)
end


# AST_DAY_OF_WEEK -- Return the day of the week for the given Julian day.
# The integer day of the week is 0=Sunday - 6=Saturday.  The character string
# is the three character abbreviation for the day of the week.  Note that
# the day of the week is for Greenwich if the standard UT is used.

procedure ast_day_of_week (jd, d, name, sz_name)

double	jd		# Julian date
int	d		# Day of the week (0=SUN)
char	name[sz_name]	# Name for day of the week
int	sz_name		# Size of name string

begin
	d = mod (int (jd - 0.5) + 2, 7)
	switch (d) {
	case 0:
	    call strcpy ("SUN", name, sz_name)
	case 1:
	    call strcpy ("MON", name, sz_name)
	case 2:
	    call strcpy ("TUE", name, sz_name)
	case 3:
	    call strcpy ("WED", name, sz_name)
	case 4:
	    call strcpy ("THU", name, sz_name)
	case 5:
	    call strcpy ("FRI", name, sz_name)
	case 6:
	    call strcpy ("SAT", name, sz_name)
	}
end


# AST_JULDAY -- Convert epoch to Julian day.

double procedure ast_julday (epoch)

double	epoch			# Epoch

double	jd

begin
	jd = JD2000 + (epoch - J2000) * JYEAR
	return (jd)
end


# AST_DATE_TO_JULDAY -- Convert date to Julian day.
# This assumes dates after year 99.

double procedure ast_date_to_julday (year, month, day, t)

int	year			# Year
int	month			# Month (1-12)
int	day			# Day of month
double	t			# Time for date (mean solar day)

double	jd
int	y, m, d

begin
	if (year < 100)
	    y = 1900 + year
	else
	    y = year

	if (month > 2)
	    m = month + 1
	else {
	    m = month + 13
	    y = y - 1
	}

	jd = int (JYEAR * y) + int (30.6001 * m) + day + 1720995
	if (day + 31 * (m + 12 * y) >= 588829) {
	    d = int (y / 100)
	    m = int (y / 400)
	    jd = jd + 2 - d + m
	}
	jd = jd - 0.5 + int (t * 360000. + 0.5) / 360000. / 24.
	return (jd)
end


# AST_JULDAY_TO_DATE -- Convert Julian date to calendar date.
# This procedure is taken from `eraJd2cal` of ERFA (Essential Routines
# for Fundamental Astronomy) and converted to SPP.  ERFA is a C
# library containing key algorithms for astronomy, and is based on the
# `SOFA library <http://www.iausofa.org/>`_ published by the
# International Astronomical Union (IAU).
#
# ERFA is intended to replicate the functionality of SOFA (aside from
# possible bugfixes in ERFA that have not yet been included in SOFA),
# but is licensed under a three-clause BSD license to enable its
# compatibility with a wide range of open source licenses. Permission
# for this release has been obtained from the SOFA board
#
# ERFA can be found at https://github.com/liberfa/erfa/
#
#  Notes:
#
#  1) The earliest valid date is -68569.5 (-4900 March 1).  The
#     largest value accepted is 1e9.
#
#  2) The Julian Date is apportioned in any convenient way between
#     the arguments dj1 and dj2.  For example, JD=2450123.7 could
#     be expressed in any of these ways, among others:
#
#            dj1             dj2
#
#         2450123.7           0.0       (JD method)
#         2451545.0       -1421.3       (J2000 method)
#         2400000.5       50123.2       (MJD method)
#         2450123.5           0.2       (date & time method)
#
#     In this implementation, dj2 is set to 0.0.
#
#  3) In early eras the conversion is from the "proleptic Gregorian
#     calendar";  no account is taken of the date(s) of adoption of
#     the Gregorian calendar, nor is the AD/BC numbering convention
#     observed.
#
#  Reference:
#
#     Explanatory Supplement to the Astronomical Almanac,
#     P. Kenneth Seidelmann (ed), University Science Books (1992),
#     Section 12.92 (p604).
#
#  Copyright (C) 2013-2017, NumFOCUS Foundation.
#  Derived, with permission, from the SOFA library.

procedure ast_julday_to_date (j, year, month, day, t)

double	j,dj2			# Julian day
data dj2 /0./
int	year			# Year
int	month			# Month (1-12)
int	day			# Day of month
double	t			# Time for date (mean solar day)

long jd, l, n, i, k
double d1, d2, f1, f2, f, d
double mod(), nint()

begin
	# Copy the date, big then small, and re-align to midnight.
	if (j >= dj2) {
	  d1 = j
	  d2 = dj2
	} else {
	  d1 = dj2
	  d2 = j
	}
	d2 = d2 - 0.5

	# Separate day and fraction.
	f1 = mod(d1, 1.0)
	f2 = mod(d2, 1.0)
	f = mod(f1 + f2, 1.0)
	if (f < 0.0) f = f + 1.0
	d = nint(d1-f1) + nint(d2-f2) + nint(f1+f2-f)
	jd = long(nint(d)) + 1

	# Express day in Gregorian calendar.
	l = jd + 68569
	n = (4 * l) / 146097
	l = l - (146097 * n + 3) / 4
	i = (4000 * (l + 1)) / 1461001
	l = l - ((1461 * i) / 4 - 31)
	k = (80 * l) / 2447
	day = int(l - (2447 * k) / 80)
	l = k / 11
	month = int(k + 2 - 12 * l)
	year = int(100 * (n - 49) + i + l)
	t = f * 24
end


# AST_MST -- Mean sidereal time of the epoch at the given longitude.
# This procedure may be used to optain Greenwich Mean Sidereal Time (GMST)
# by setting the longitude to 0.

double procedure ast_mst (epoch, longitude)

double	epoch		# Epoch
double	longitude	# Longitude in degrees

double	jd, ut, t, st
double	ast_julday()

begin
	# Determine JD and UT, and T (JD in centuries from J2000.0).
	jd = ast_julday (epoch)
	ut = (jd - int (jd) - 0.5) * 24.
	t = (jd - 2451545.d0) / 36525.d0

	# The GMST at 0 UT in seconds is a power series in T.
	st = 24110.54841d0 +
	    t * (8640184.812866d0 + t * (0.093104d0 - t * 6.2d-6))

	# Correct for longitude and convert to standard hours.
	st = mod (st / 3600. + ut - longitude / 15., 24.0D0)

	if (st < 0)
	    st = st + 24

	return (st)
end
