include	<error.h>
include <imhdr.h>
include <ctype.h>


# T_SETJD -- Set Julian dates
# This task computes the Geocentric Julian date, the Helocentric Julian Date,
# and the local Julian day for a list of images.  Any set of these may
# be output.  The input keywords include the date of observation, the
# time of observation, the exposure time, and the RA/DEC/EPOCH of observation.
# If an exposure time specified the times are corrected to midexposure.

procedure t_setjd()

pointer	imlist			# List of images
pointer	date_key		# Date keyword
pointer	time_key		# Time keyword
pointer	exp_key			# Exposure keyword
pointer	ra_key			# RA keyword (hours)
pointer	dec_key			# DEC keyword (hours)
pointer	ep_key			# RA/DEC epoch keyword

pointer	ujd_key			# JD keyword
pointer	hjd_key			# HJD keyword
pointer	ljd_key			# Local JD keyword

bool	utdate			# UT date?
bool	uttime			# UT time?
bool	listonly		# List only?
pointer	observatory		# Observatory

bool	newobs, obshead
int	i, year, month, day
double	zone, exp, time, ra, dec, ep, epoch, ujd, hjd, ljd, lt
pointer	im, obs, sp, input, date

bool	clgetb()
int	nowhite(), imtgetim(), ctoi()
pointer	imtopenp(), immap()
double	imgetd(), obsgetd(), ast_julday()
errchk	immap, obsobpen, obsgetd, obsimopen, imgstr, imgetd

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (observatory, SZ_FNAME, TY_CHAR)
	call salloc (date_key, SZ_FNAME, TY_CHAR)
	call salloc (time_key, SZ_FNAME, TY_CHAR)
	call salloc (exp_key, SZ_FNAME, TY_CHAR)
	call salloc (ra_key, SZ_FNAME, TY_CHAR)
	call salloc (dec_key, SZ_FNAME, TY_CHAR)
	call salloc (ep_key, SZ_FNAME, TY_CHAR)
	call salloc (ujd_key, SZ_FNAME, TY_CHAR)
	call salloc (hjd_key, SZ_FNAME, TY_CHAR)
	call salloc (ljd_key, SZ_FNAME, TY_CHAR)
	call salloc (date, SZ_FNAME, TY_CHAR)

	# Get the parameters
	imlist = imtopenp ("images")
	call clgstr ("observatory", Memc[observatory], SZ_FNAME)
	obs = NULL

	call clgstr ("date", Memc[date_key], SZ_FNAME)
	call clgstr ("time", Memc[time_key], SZ_FNAME)
	call clgstr ("exposure", Memc[exp_key], SZ_FNAME)
	call clgstr ("ra", Memc[ra_key], SZ_FNAME)
	call clgstr ("dec", Memc[dec_key], SZ_FNAME)
	call clgstr ("epoch", Memc[ep_key], SZ_FNAME)

	call clgstr ("jd", Memc[ujd_key], SZ_FNAME)
	call clgstr ("hjd", Memc[hjd_key], SZ_FNAME)
	call clgstr ("ljd", Memc[ljd_key], SZ_FNAME)

	utdate = clgetb ("utdate")
	uttime = clgetb ("uttime")
	listonly = clgetb ("listonly")

	# Set log header
	call printf ("#%19s")
	    call pargstr ("Image")
	if (nowhite (Memc[ujd_key], Memc[ujd_key], SZ_LINE) != 0) {
	    call printf ("  %12s")
		call pargstr (Memc[ujd_key])
	}
	if (nowhite (Memc[hjd_key], Memc[hjd_key], SZ_LINE) != 0) {
	    call printf ("  %12s")
		call pargstr (Memc[hjd_key])
	}
	if (nowhite (Memc[ljd_key], Memc[ljd_key], SZ_LINE) != 0) {
	    call printf ("  %8s")
		call pargstr (Memc[ljd_key])
	}
	call printf ("\n")

	# Loop over all images
	while (imtgetim (imlist, Memc[input], SZ_FNAME) != EOF) {
	    iferr {
		im = NULL
		if (listonly)
		    i = immap (Memc[input], READ_ONLY, 0)
		else
		    i = immap (Memc[input], READ_WRITE, 0)
		im = i

		# Get time zone for the observatory.
		call obsimopen (obs, im, Memc[observatory], NO, newobs, obshead)
		if (newobs)
		    call obslog (obs, "SETJD", "timezone", STDOUT)
		zone = obsgetd (obs, "timezone")

		# Determine the date and time of observation.

		call imgstr (im, Memc[date_key], Memc[date], SZ_LINE)
		for (i=0; Memc[date+i]!=EOS; i=i+1)
		    if (!IS_DIGIT(Memc[date+i]))
			Memc[date+i] = ' '
		i = 1
		if (ctoi (Memc[date], i, day) == 0)
		    day = 0
		if (ctoi (Memc[date], i, month) == 0)
		    month = 0
		if (ctoi (Memc[date], i, year) == 0)
		    year = 0
		if (day == 0 || month == 0 || year == 0)
		    call error (1, "Error in date keyword")

		time = imgetd (im, Memc[time_key])

		# Correct to midexposure if desired.
		if (Memc[exp_key] != EOS) {
		    if (Memc[exp_key] == '-')
			exp = -imgetd (im, Memc[exp_key+1])
		    else
			exp = imgetd (im, Memc[exp_key])
		    time = time + exp / (2 * 3600.)
		}

		# Compute UJD and LJD.
		call ast_date_to_epoch (year, month, day, time, epoch)
		ujd = ast_julday (epoch)
		if (utdate) {
		    if (uttime)
			ljd = ujd - zone / 24.
		    else {
			ljd = ujd
			ujd = ljd + zone / 24.
		    }
		} else {
		    if (uttime) {
			if (time - zone < 0.)
			    ujd = ujd + 1
			if (time + zone >= 24.)
			    ujd = ujd - 1
			ljd = ujd - zone / 24.
		    } else {
			ljd = ujd
			ujd = ljd + zone / 24.
		    }
		}

		
		# Print results.  The HJD is only computed if needed.
		call printf ("%20s")
		    call pargstr (Memc[input])
		if (Memc[ujd_key] != EOS) {
		    call imaddd (im, Memc[ujd_key], ujd)
		    call printf ("  %12.4f")
			call pargd (ujd)
		}
		if (Memc[hjd_key] != EOS) {
		    ra = imgetd (im, Memc[ra_key]) 
		    dec = imgetd (im, Memc[dec_key]) 
		    iferr (ep = imgetd (im, Memc[ep_key]))
		        ep = INDEFD
		    call ast_precess (ra, dec, ep, ra, dec, epoch)
		    call ast_jd_to_hjd (ra, dec, ujd, lt, hjd)
		    call imaddd (im, Memc[hjd_key], hjd)
		    call printf ("  %12.4f")
			call pargd (hjd)
		}
		if (Memc[ljd_key] != EOS) {
		    ljd = int (ljd)
		    call imaddd (im, Memc[ljd_key], ljd)
		    call printf ("  %8d")
			call pargi (int (ljd))
		}
		call printf ("\n")
	    } then
		call erract (EA_WARN)

	    if (im != NULL)
		call imunmap (im)
	}

	call obsclose (obs)
	call sfree (sp)
end
