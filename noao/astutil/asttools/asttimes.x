# AST_DATE_TO_EPOCH -- Convert Gregorian date and solar mean time to
# a Julian epoch.  A Julian epoch has 365.25 days per year and 24
# hours per day.

procedure ast_date_to_epoch (year, month, day, ut, epoch)

int	year			# Year
int	month			# Month (1-12)
int	day			# Day of month
double	ut			# Universal time for date (mean solar day)
double	epoch			# Julian epoch

int	yr
int	ast_day_of_year()

begin
	if (year < 100)
	    yr = 1900 + year
	else
	    yr = year

	ut = int (ut * 360000. + 0.5) / 360000.
	epoch = yr + (ast_day_of_year (yr, month, day)-1+ut/24.) / 365.25
end


# AST_EPOCH_TO_DATE -- Convert a Julian epoch to year, month, day, and time.

procedure ast_epoch_to_date (epoch, year, month, day, ut)

double	epoch			# Julian epoch
int	year			# Year
int	month			# Month (1-12)
int	day			# Day of month
double	ut			# Universal time for date

int	d
int	ast_day_of_year()

begin
	year = epoch
	d = (epoch - year) * 365.25
	ut = ((epoch - year) * 365.25 - d) * 24.
	ut = int (ut * 360000. + 0.5) / 360000.
	if (ut >= 24.) {
	    d = d + 1
	    ut = ut - 24.
	}

	d = d + 1
	for (month=1; d >= ast_day_of_year (year, month+1, 1); month=month+1)
	    ;
	day = d - ast_day_of_year (year, month, 1) + 1
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

int	year, century
double	jd

begin
      year = int (epoch) - 1
      century = year / 100
      jd = 1721425.5 + 365 * year - century + int (year / 4) + int (century / 4)
      jd = jd + (epoch - int(epoch)) * 365.25
      return (jd)
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
	t = (jd - 2451545.) / 36525.

	# The GMST at 0 UT in seconds is a power series in T.
	st = 24110.54841 + t * (8640184.812866 + t * (0.093104 - t * 6.2e-6))

	# Correct for longitude and convert to standard hours.
	st = mod (st / 3600. + ut - longitude / 15., 24.0D0)

	if (st < 0)
	    st = st + 24

	return (st)
end
