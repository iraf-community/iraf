include <imhdr.h>
include <error.h>
include <ctotok.h>
include	<math.h>

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

# Input keywords - the date and exposure time keywords are parameters.

define	RA_KEY		"RA"
define	DEC_KEY		"DEC"
define	EP_KEY		"EPOCH"
define	ST_KEY		"ST"
define	UT_KEY		"UT"

define	UT_DEF		0	# for precession if the keyword is missing

define	SOLTOSID	(($1)*1.00273790935d0)


# T_SETAIRMASS -- Read the parameters, loop over the images using
# Stetson's rule, print out answers and update the header

procedure t_setairmass()

pointer	imlist, im, obs
pointer	sp, input, observatory, date_key, exp_key, air_key, utm_key, ut_hms
int	intype, outtype
bool	show, update, override, newobs, obshead

double	dec, latitude, exptime
double	ha, ha_beg, ha_mid, ha_end, ut, ut_mid
double	airm_beg, airm_end, airm_mid, airm_eff

bool	clgetb()
int	imtgetim(), clgwrd(), imaccf()
pointer	imtopenp(), immap()
double	airmass(), obsgetd()
errchk	obsobpen, obsgetd, sa_rheader, airmass, obsimopen

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (observatory, SZ_FNAME, TY_CHAR)
	call salloc (date_key, SZ_FNAME, TY_CHAR)
	call salloc (exp_key, SZ_FNAME, TY_CHAR)
	call salloc (air_key, SZ_FNAME, TY_CHAR)
	call salloc (utm_key, SZ_FNAME, TY_CHAR)
	call salloc (ut_hms, SZ_FNAME, TY_CHAR)

	# Get the parameters
	imlist = imtopenp ("images")
	intype = clgwrd ("intype", Memc[input], SZ_FNAME, AIR_TYPES)
	call clgstr ("observatory", Memc[observatory], SZ_FNAME)

	call clgstr ("date", Memc[date_key], SZ_FNAME)
	call clgstr ("exposure", Memc[exp_key], SZ_FNAME)
	call clgstr ("airmass", Memc[air_key], SZ_FNAME)
	call clgstr ("utmiddle", Memc[utm_key], SZ_FNAME)

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
		call sa_rheader (im, Memc[date_key], Memc[exp_key],
		    ha, dec, ut, exptime)

		# Calculate the mid-UT and HA's for the various input types
		switch (intype) {
		case BEGINNING:
		    ha_beg = ha
		    ha_mid = ha + SOLTOSID(exptime) / 2.
		    ha_end = ha + SOLTOSID(exptime)

		    if (IS_INDEFD(ut))
			ut_mid = INDEFD
		    else
			ut_mid = mod (ut + exptime / 2., 24.0D0)

		case MIDDLE:
		    ha_beg = ha - SOLTOSID(exptime) / 2.
		    ha_mid = ha
		    ha_end = ha + SOLTOSID(exptime) / 2.

		    if (IS_INDEFD(ut))
			ut_mid = INDEFD
		    else
			ut_mid = mod (ut, 24.0D0)

		case END:
		    ha_beg = ha - SOLTOSID(exptime)
		    ha_mid = ha - SOLTOSID(exptime) / 2.
		    ha_end = ha

		    if (IS_INDEFD(ut))
			ut_mid = INDEFD
		    else
			ut_mid = mod (ut - exptime / 2., 24.0D0)

		default:
		    call error (1, "Bad switch in t_setairmass")
		}

		# Save the mid-UT as a sexigesimal string for output
		call sprintf (Memc[ut_hms], SZ_FNAME, "%h")
		    call pargd (ut_mid)

		# Compute the beginning, middle and ending airmasses
		# First get the latitude from the observatory database.

		call obsimopen (obs, im, Memc[observatory], NO, newobs, obshead)
		if (newobs)
		    call obslog (obs, "SETAIRMASS", "latitude", STDOUT)
		latitude = obsgetd (obs, "latitude")
		airm_beg = airmass (ha_beg, dec, latitude)
		airm_mid = airmass (ha_mid, dec, latitude)
		airm_end = airmass (ha_end, dec, latitude)

		# Combine as suggested by P. Stetson (Simpson's rule)
		airm_eff = (airm_beg + 4.*airm_mid + airm_end) / 6.

	    } then {
		call imunmap (im)
		call erract (EA_WARN)
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
		    (! IS_INDEFD(ut_mid)))
			call imastr (im, Memc[utm_key], Memc[ut_hms])
	    }

	    call imunmap (im)
	}

	call obsclose (obs)
	call sfree (sp)
end


# AIRMASS -- Compute airmass from DEC, LATITUDE and HA

# Airmass formulation from Allen "Astrophysical Quantities" 1973 p.125,133.
# and John Ball's book on Algorithms for the HP-45

double procedure airmass (ha, dec, lat)

double	ha, dec, lat, cos_zd, x

define	SCALE	750.0d0			# Atmospheric scale height

begin
	if (IS_INDEFD (ha) || IS_INDEFD (dec) || IS_INDEFD (lat))
	    call error (1, "Can't determine airmass")

	cos_zd = sin(DEGTORAD(lat)) * sin(DEGTORAD(dec)) +
		 cos(DEGTORAD(lat)) * cos(DEGTORAD(dec)) * cos(DEGTORAD(ha*15.))

	x  = SCALE * cos_zd

	return (sqrt (x**2 + 2*SCALE + 1) - x)
end


# SA_RHEADER -- derive the ha, dec, ut, and exptime from the header.

define	SZ_TOKEN	2

procedure sa_rheader (im, dkey, ekey, ha, dec, ut, exptime)

pointer	im		#I imio pointer
char	dkey[ARB]	#I date keyword ("DD/MM/YY")
char	ekey[ARB]	#I exposure keyword (seconds)

double	ha		#O hour angle
double	dec		#O current epoch declination
double	ut		#O universal time
double	exptime		#O exposure time (hours)

pointer	date, sp
double	ra1, dec1, epoch1, ra2, dec2, epoch2, st2, ut2
int	day, month, year, token1, token2
char	tokstr[SZ_TOKEN]
bool	precess, ut_ok

bool	fp_equald()
int	nscan()
double	imgetd()

errchk	imgetd, imgstr

data	token1	/NULL/
data	token2	/NULL/

begin
	call smark (sp)
	call salloc (date, SZ_LINE, TY_CHAR)

	iferr {
	    # `1' is the coordinate epoch, `2' is the observation epoch
	    ra1  = imgetd (im, RA_KEY)
	    dec1 = imgetd (im, DEC_KEY)
	    st2  = imgetd (im, ST_KEY)

	    iferr (ut2  = imgetd (im, UT_KEY)) {
		ut2 = UT_DEF
		ut_ok = false
	    } else
		ut_ok = true

	    iferr (epoch1 = imgetd (im, EP_KEY))
		epoch1 = INDEFD

	    # only calculate epoch2 if epoch1 is valid
	    # only precess coords if both are valid
	    precess = false
	    if (! (fp_equald (epoch1, double(0.)) || IS_INDEFD(epoch1))) {
		iferr (call imgstr (im, dkey, Memc[date], SZ_LINE))
		    Memc[date] = EOS

		call sscan (Memc[date])
		    call gargi (day)
		    call gargtok (token1, tokstr, SZ_TOKEN)
		    call gargi (month)
		    call gargtok (token2, tokstr, SZ_TOKEN)
		    call gargi (year)

		if (nscan() == 5 &&
		    (token1 == TOK_OPERATOR || token1 == TOK_PUNCTUATION) &&
		    (token2 == TOK_OPERATOR || token2 == TOK_PUNCTUATION)) {

		    call ast_date_to_epoch (year, month, day, ut2, epoch2)
		    precess = true
		}
	    }

	    if (precess)
		call astprecess (ra1, dec1, epoch1, ra2, dec2, epoch2)
	    else {
		ra2 = ra1
		dec2 = dec1
		call eprintf ("\tCoords not precessed for %s: check %s & %s\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (EP_KEY)
		    call pargstr (dkey)
		call flush (STDERR)
	    }

	    # don't use the output arguments internally
	    ha		= st2 - ra2
	    dec		= dec2
	    exptime	= imgetd (im, ekey) / 3600.d0

	    if (ut_ok)
		ut	= ut2
	    else
		ut	= INDEFD

	} then {
	    call sfree (sp)
	    call eprintf ("Problem reading header for %s:\n")
		call pargstr (IM_HDRFILE(im))
	    call flush (STDERR)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
end
