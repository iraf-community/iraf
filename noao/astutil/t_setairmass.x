include <imhdr.h>
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

# Input keywords (read the exposure time keyword from a parameter)

define	RA_KEY		"RA"
define	DEC_KEY		"DEC"
define	ST_KEY		"ST"
define	UT_KEY		"UT"

# Output keywords

define	AIR_KEY		"AIRMASS"
define	UTM_KEY		"UTMIDDLE"

# T_SETAIRMASS -- Read the parameters, loop over the images using
# Stetson's rule, print out answers and update the header

procedure t_setairmass()

pointer	imlist, input, im, sp, exp_key, ut_hms
int	intype, outtype
bool	show, update, override

real	ra, dec, st, latitude, exptime
real	ha, ha_beg, ha_mid, ha_end, ut, ut_mid
real	airm_beg, airm_end, airm_mid, airm_eff

bool	clgetb()
int	imtgetim(), clgwrd(), imaccf()
pointer	imtopenp(), immap()
real	airmass(), clgetr(), imgetr()
errchk	airmass

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (exp_key, SZ_FNAME, TY_CHAR)
	call salloc (ut_hms, SZ_FNAME, TY_CHAR)

	# Get the parameters

	imlist = imtopenp ("images")
	intype = clgwrd ("intype", Memc[input], SZ_FNAME, AIR_TYPES)

	call clgstr ("exposure", Memc[exp_key], SZ_FNAME)
	latitude = clgetr ("latitude")

	show = clgetb ("show")
	update = clgetb ("update")

	if (update) {
	    outtype = clgwrd ("outtype", Memc[input], SZ_FNAME, AIR_TYPES)
	    override = clgetb ("override")
	}

	# Print a header line (the # should imply a comment to another task)

	if (show) {
	    call printf ("#              Image  UT middle  ")
	    call printf ("effective  begin   middle     end\n")
	}

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
		ra	= imgetr (im, RA_KEY)
		dec	= imgetr (im, DEC_KEY)
		st	= imgetr (im, ST_KEY)
		ut	= imgetr (im, UT_KEY)
		exptime	= imgetr (im, Memc[exp_key]) / 3600.

		ha = st - ra

		# Calculate the mid-UT and HA's for the various input types

		switch (intype) {
		case BEGINNING:
		    ut_mid = ut + exptime / 2.
		    ha_beg = ha
		    ha_mid = ha + exptime / 2.
		    ha_end = ha + exptime
		case MIDDLE:
		    ut_mid = ut
		    ha_beg = ha - exptime / 2.
		    ha_mid = ha
		    ha_end = ha + exptime / 2.
		case END:
		    ut_mid = ut - exptime / 2.
		    ha_beg = ha - exptime
		    ha_mid = ha - exptime / 2.
		    ha_end = ha
		default:
		    call error (1, "Bad switch in t_airmass")
		}

		# Save the mid-UT as a sexigesimal string for output

		call sprintf (Memc[ut_hms], SZ_FNAME, "%h")
		    call pargr (mod (ut_mid, 24.))

		# Compute the beginning, middle and ending airmasses

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
		call printf ("%20s  %8s  %7.4f  %7.4f  %7.4f  %7.4f\n")
		    call pargstr (Memc[input])
		    call pargstr (Memc[ut_hms])
		    call pargr (airm_eff)
		    call pargr (airm_beg)
		    call pargr (airm_mid)
		    call pargr (airm_end)

		call flush (STDOUT)
	    }

	    if (update) {
		if (imaccf (im, AIR_KEY) == NO || override)
		    switch (outtype) {
		    case BEGINNING:
			call imaddr (im, AIR_KEY, airm_beg)
		    case MIDDLE:
			call imaddr (im, AIR_KEY, airm_mid)
		    case END:
			call imaddr (im, AIR_KEY, airm_end)
		    case EFFECTIVE:
			call imaddr (im, AIR_KEY, airm_eff)
		    default:
			call error (1, "Bad switch in t_airmass")
		    }

		# Should probably update a date keyword as well

		if (imaccf (im, UTM_KEY) == NO || override)
		    call imastr (im, UTM_KEY, Memc[ut_hms])
	    }

	    call imunmap (im)
	}

	call sfree (sp)
end


# AIRMASS -- Compute airmass from DEC, LATITUDE and HA

# Airmass formulation from Allen "Astrophysical Quantities" 1973 p.125,133.
# and John Ball's book on Algorithms for the HP-45

real procedure airmass (ha, dec, lat)

real	ha, dec, lat, cos_zd, x

define	RADS	57.29577951		# Degrees per radian
define	SCALE	750.0			# Atmospheric scale height

begin
	if (IS_INDEF (ha) || IS_INDEF (dec) || IS_INDEF (lat))
	    call error (1, "Can't determine airmass")

	cos_zd = sin (lat/RADS) * sin (dec/RADS) +
		 cos (lat/RADS) * cos (dec/RADS) * cos (15*ha/RADS)

	x  = SCALE * cos_zd

	return (sqrt (x**2 + 2*SCALE + 1) - x)
end
