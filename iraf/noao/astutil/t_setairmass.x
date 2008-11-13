include <imhdr.h>
include <ctype.h>
include <error.h>

# SETAIRMASS -- Compute the airmass for a series of images and optionally
# store these in the image header.

# The exposure time is assumed to be in seconds.  There is no provision
# for observatories that save the begin and end integration times in
# the header but not the exposure duration.  Note that shutter closings
# (due to clouds) void the assumptions about effective airmass.

# Possible keyword input and output time stamps

define	AIR_TYPES	"|beginning|middle|end|effective|"

define	BEGINNING	1
define	MIDDLE		2
define	END		3
define	EFFECTIVE	4

define	UT_DEF		0D0	# for precession if the keyword is missing

define	SOLTOSID	(($1)*1.00273790935d0)


# T_SETAIRMASS -- Read the parameters, loop over the images using Stetson's
# rule, print out answers and update the header

procedure t_setairmass()

pointer	imlist, im, obs
pointer	sp, input, observatory, date_key, exp_key, air_key, utm_key, ut_hms
pointer	ra_key, dec_key, eqn_key, st_key, ut_key, datestr
int	intype, outtype, year, month, day, day1, fmt
bool	show, update, override, newobs, obshead

double	dec, latitude, exptime, scale, jd
double	ha, ha_beg, ha_mid, ha_end, ut, ut_mid
double	airm_beg, airm_end, airm_mid, airm_eff

bool	clgetb()
int	imtgetim(), clgwrd(), imaccf(), dtm_encode()
pointer	imtopenp(), immap()
double	clgetd(), airmassx(), obsgetd(), ast_date_to_julday()
errchk	obsobpen, obsgetd, sa_rheader, airmassx, obsimopen

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (observatory, SZ_FNAME, TY_CHAR)
	call salloc (ra_key, SZ_FNAME, TY_CHAR)
	call salloc (dec_key, SZ_FNAME, TY_CHAR)
	call salloc (eqn_key, SZ_FNAME, TY_CHAR)
	call salloc (st_key, SZ_FNAME, TY_CHAR)
	call salloc (ut_key, SZ_FNAME, TY_CHAR)
	call salloc (date_key, SZ_FNAME, TY_CHAR)
	call salloc (exp_key, SZ_FNAME, TY_CHAR)
	call salloc (air_key, SZ_FNAME, TY_CHAR)
	call salloc (utm_key, SZ_FNAME, TY_CHAR)
	call salloc (ut_hms, SZ_FNAME, TY_CHAR)
	call salloc (datestr, SZ_FNAME, TY_CHAR)

	# Get the parameters
	imlist = imtopenp ("images")
	intype = clgwrd ("intype", Memc[input], SZ_FNAME, AIR_TYPES)
	call clgstr ("observatory", Memc[observatory], SZ_FNAME)
	call clgstr ("ra", Memc[ra_key], SZ_FNAME)
	call clgstr ("dec", Memc[dec_key], SZ_FNAME)
	call clgstr ("equinox", Memc[eqn_key], SZ_FNAME)
	call clgstr ("st", Memc[st_key], SZ_FNAME)
	call clgstr ("ut", Memc[ut_key], SZ_FNAME)
	call clgstr ("date", Memc[date_key], SZ_FNAME)
	call clgstr ("exposure", Memc[exp_key], SZ_FNAME)
	call clgstr ("airmass", Memc[air_key], SZ_FNAME)
	call clgstr ("utmiddle", Memc[utm_key], SZ_FNAME)
	scale = clgetd ("scale")

	# just to be neat
	call strupr (Memc[date_key])
	call strupr (Memc[exp_key])
	call strupr (Memc[air_key])
	call strupr (Memc[utm_key])

	show = clgetb ("show")
	update = clgetb ("update")

	# Open observatory later.
	obs = NULL

	if (update) {
	    outtype = clgwrd ("outtype", Memc[input], SZ_FNAME, AIR_TYPES)
	    override = clgetb ("override")
	}

	# Print a header line (the # should imply a comment to another task)
	if (show) {
	    call printf ("#              Image    UT middle  ")
	    call printf ("effective  begin   middle     end   updated\n")
	} else if (!update)
	    call eprintf ("WARNING: Image headers are not updated\n")

	# Loop over all images
	while (imtgetim (imlist, Memc[input], SZ_FNAME) != EOF) {
	    iferr {
		if (update)
		    im = immap (Memc[input], READ_WRITE, 0)
		else
		    im = immap (Memc[input], READ_ONLY, 0)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    iferr {
		call sa_rheader (im, Memc[ra_key], Memc[dec_key],
		    Memc[eqn_key], Memc[st_key], Memc[ut_key], Memc[date_key],
		    Memc[exp_key], ha, dec, year, month, day, ut, exptime,
		    fmt)

		# Calculate the mid-UT and HA's for the various input types
		switch (intype) {
		case BEGINNING:
		    ha_beg = ha
		    ha_mid = ha + SOLTOSID(exptime) / 2.
		    ha_end = ha + SOLTOSID(exptime)

		    if (IS_INDEFD(ut))
			ut_mid = INDEFD
		    else
			ut_mid = ut + exptime / 2.

		case MIDDLE:
		    ha_beg = ha - SOLTOSID(exptime) / 2.
		    ha_mid = ha
		    ha_end = ha + SOLTOSID(exptime) / 2.

		    if (IS_INDEFD(ut))
			ut_mid = INDEFD
		    else
			ut_mid = ut

		case END:
		    ha_beg = ha - SOLTOSID(exptime)
		    ha_mid = ha - SOLTOSID(exptime) / 2.
		    ha_end = ha

		    if (IS_INDEFD(ut))
			ut_mid = INDEFD
		    else
			ut_mid = ut - exptime / 2.

		default:
		    call error (1, "Bad switch in t_setairmass")
		}

		# Adjust for possible change of date in ut_mid.
		day1 = day
		jd = ast_date_to_julday (year, month, day, ut_mid)
		call ast_julday_to_date (jd, year, month, day, ut_mid)

		# Save the mid-UT as a sexigesimal string for output
		call sprintf (Memc[ut_hms], SZ_FNAME, "%h")
		    call pargd (ut_mid)

		# Compute the beginning, middle and ending airmasses
		# First get the latitude from the observatory database.

		call obsimopen (obs, im, Memc[observatory], NO, newobs, obshead)
		if (newobs)
		    call obslog (obs, "SETAIRMASS", "latitude", STDOUT)
		latitude = obsgetd (obs, "latitude")
		airm_beg = airmassx (ha_beg, dec, latitude, scale)
		airm_mid = airmassx (ha_mid, dec, latitude, scale)
		airm_end = airmassx (ha_end, dec, latitude, scale)

		# Combine as suggested by P. Stetson (Simpson's rule)
		airm_eff = (airm_beg + 4.*airm_mid + airm_end) / 6.

	    } then {
		call erract (EA_WARN)
		call imunmap (im)
		next
	    }

	    if (show) {
		call printf ("%20s  %11s  %7.4f  %7.4f  %7.4f  %7.4f  %b\n")
		    call pargstr (Memc[input])
		    call pargstr (Memc[ut_hms])
		    call pargd (airm_eff)
		    call pargd (airm_beg)
		    call pargd (airm_mid)
		    call pargd (airm_end)
		    call pargb (update)
		call flush (STDOUT)
	    }

	    if (update) {
		if (imaccf (im, Memc[air_key]) == NO || override)
		    switch (outtype) {
		    case BEGINNING:
			call imaddr (im, Memc[air_key], real(airm_beg))
		    case MIDDLE:
			call imaddr (im, Memc[air_key], real(airm_mid))
		    case END:
			call imaddr (im, Memc[air_key], real(airm_end))
		    case EFFECTIVE:
			call imaddr (im, Memc[air_key], real(airm_eff))
		    default:
			call error (1, "Bad switch in t_setairmass")
		    }

		# Should probably update a date keyword as well
		if ((imaccf (im, Memc[utm_key]) == NO || override) &&
		    (! IS_INDEFD(ut_mid))) {
#		    if (fmt == NO && day == day1)
#			call imastr (im, Memc[utm_key], Memc[ut_hms])
#		    else if (dtm_encode (Memc[datestr], SZ_FNAME,
#			year, month, day, utmid, 2, 0) > 0)
#			call imastr (im, Memc[utm_key], Memc[datestr])
		    if (dtm_encode (Memc[datestr], SZ_FNAME,
			year, month, day, utmid, 2, 0) > 0)
			call imastr (im, Memc[utm_key], Memc[datestr])
		}
	    }

	    call imunmap (im)
	}

	call obsclose (obs)
	call sfree (sp)
end


# SA_RHEADER -- derive the ha, dec, ut, and exptime from the header.

define	SZ_TOKEN	2

procedure sa_rheader (im, ra_key, dec_key, eqn_key, st_key, ut_key, dkey, ekey,
	ha, dec, year, month, day, ut, exptime, fmt)

pointer	im		#I imio pointer
char	ra_key[ARB]	#I date keyword (hh.hhh or hh:mm:ss.s)
char	dec_key[ARB]	#I date keyword (dd.ddd or dd:mm:ss.s)
char	eqn_key[ARB]	#I date keyword (yyyy.yyy)
char	st_key[ARB]	#I date keyword (hh.hhh or hh:mm:ss.s)
char	ut_key[ARB]	#I date keyword (hh.hhh or hh:mm:ss.s)
char	dkey[ARB]	#I date keyword (YYYY-MM-DDTHH:MM:SS.S or DD/MM/YY)
char	ekey[ARB]	#I exposure keyword (seconds)

double	ha		#O hour angle
double	dec		#O current epoch declination
int	year		#O year
int	month		#O month
int	day		#O day
double	ut		#O universal time
double	exptime		#O exposure time (hours)
int	fmt		#O Date format?

pointer	date, sp
double	ra1, dec1, epoch1, ra2, dec2, epoch2, st2, ut2
int	ip, flags

double	imgetd()
int	dtm_decode(), strmatch()
bool	fp_equald()

errchk	imgetd, imgstr

begin
	call smark (sp)
	call salloc (date, SZ_LINE, TY_CHAR)

	iferr {
	    # `1' is the coordinate epoch, `2' is the observation epoch
	    ra1  = imgetd (im, ra_key)
	    ip = strmatch (ra_key, "^{CRVAL}")
	    if (ip > 0) {
		if (IS_DIGIT(ra_key[ip]) && TO_INTEG(ra_key[ip] > 0))
		    ra1 = ra1 / 15.0d0
	    }
	    dec1 = imgetd (im, dec_key)
	    st2  = imgetd (im, st_key)

	    # Parse UT keyword in either hours or date.
	    fmt = YES
	    call imgstr (im, ut_key, Memc[date], SZ_LINE)
	    if (dtm_decode (Memc[date],year,month,day,ut,flags) == ERR) {
		iferr (ut = imgetd (im, ut_key))
		    call error (1, "Error in ut keyword")
		fmt = NO
	    }

	    # Parse the date.
	    call imgstr (im, dkey, Memc[date], SZ_LINE)
	    if (dtm_decode (Memc[date],year,month,day,ut2,flags) == ERR)
		call error (1, "Error in date keyword")

	    iferr (epoch1 = imgetd (im, eqn_key))
		epoch1 = INDEFD
	    if (!(fp_equald (epoch1, double(0.)) || IS_INDEFD(epoch1))) {
		if (IS_INDEFD(ut))
		    call ast_date_to_epoch (year, month, day, UT_DEF, epoch2)
		else
		    call ast_date_to_epoch (year, month, day, ut, epoch2)
		call astprecess (ra1, dec1, epoch1, ra2, dec2, epoch2)
	    } else {
		ra2 = ra1
		dec2 = dec1
		call eprintf ("\tCoords not precessed for %s: check %s\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (eqn_key)
		call flush (STDERR)
	    }

	    # don't use the output arguments internally
	    ha		= st2 - ra2
	    dec		= dec2
	    exptime	= imgetd (im, ekey) / 3600.d0
	} then {
	    call sfree (sp)
	    call eprintf ("Problem reading header for %s:\n")
		call pargstr (IM_HDRFILE(im))
	    call flush (STDERR)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
end
