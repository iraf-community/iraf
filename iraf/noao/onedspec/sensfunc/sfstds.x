include "sensfunc.h"


# SF_STDS -- Get the standard observations for the specified apertures.
# If ignoring aperture set all apertures to 1.
# This routine knows the output of the task STANDARD.

procedure sf_stds (standards, aps, ignoreaps, stds, nstds)

char	standards	# Standard star data file
pointer	aps		# Aperture list
bool	ignoreaps	# Ignore apertures?
pointer	stds		# Pointer to standard observations (returned)
int	nstds		# Number of standard observations (returned)

int	i, j, fd, beam, npts, nwaves, nalloc
real	exptime, airmass, wstart, wend
real	wavelength, flux, dwave, count
pointer	sp, image, title, std
pointer	waves, fluxes, dwaves, counts, sens, fit, wts, iwts, x, y

bool	rng_elementi()
int	open(), fscan(), nscan(), stridxs()
errchk	open, malloc, realloc

begin
	call smark (sp)
	call salloc (image, SZ_STDIMAGE, TY_CHAR)
	call salloc (title, SZ_STDTITLE, TY_CHAR)

	# Open the standard observation data file.
	fd = open (standards, READ_ONLY, TEXT_FILE)

	# Read the standard observations and create a structure for each one.
	# The beginning of a new star is found by a line whose first word
	# begins with the character '['.  Otherwise the line is interpreted
	# as a data line.  All unrecognized formats are skipped.

	nwaves = 0
	nstds = 0
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[image], SZ_STDIMAGE)
	    if (Memc[image] == '[') {
	        call gargi (beam)
	        call gargi (npts)
		call gargr (exptime)
	        call gargr (airmass)
	        call gargr (wstart)
	        call gargr (wend)
	        call gargstr (Memc[title], SZ_STDTITLE)
	        if (nscan() < 7)
		    next
		if (!rng_elementi (aps, beam))
		    next
		if (IS_INDEF (exptime) || exptime <= 0.) {
		    call eprintf (
		"%s: Warning - exposure time missing or zero, using 1 second\n")
			call pargstr (Memc[image])
		    exptime = 1.
		}

		# For the first one create the pointer to the array of
		# structures.  For the following stars increase the size
		# of the pointer array and finish up the previous standard
		# star.

	        if (nstds == 0) {
		    nstds = nstds + 1
		    call calloc (stds, nstds, TY_INT)
		    call calloc (std, LEN_STD, TY_STRUCT)
		    Memi[stds+nstds-1] = std
	        } else {
	    	    if (nwaves > 0) {
			call realloc (waves, nwaves, TY_REAL)
			call realloc (fluxes, nwaves, TY_REAL)
			call realloc (dwaves, nwaves, TY_REAL)
			call realloc (counts, nwaves, TY_REAL)
			call realloc (wts, nwaves, TY_REAL)
			call malloc (sens, nwaves, TY_REAL)
			call malloc (fit, nwaves, TY_REAL)
			call malloc (iwts, nwaves, TY_REAL)
			call malloc (x, nwaves, TY_REAL)
			call malloc (y, nwaves, TY_REAL)
			call amovr (Memr[wts], Memr[iwts], nwaves)
			STD_NWAVES(std) = nwaves
			STD_WAVES(std) = waves
			STD_FLUXES(std) = fluxes
			STD_DWAVES(std) = dwaves
			STD_COUNTS(std) = counts
			STD_SENS(std) = sens
			STD_FIT(std) = fit
			STD_WTS(std) = wts
			STD_IWTS(std) = iwts
			STD_X(std) = x
			STD_Y(std) = y

			nstds = nstds + 1
			call realloc (stds, nstds, TY_INT)
			call calloc (std, LEN_STD, TY_STRUCT)
			Memi[stds+nstds-1] = std
		    }
	        }

		# Start a new standard star.
	        std = Memi[stds+nstds-1]
		if (ignoreaps)
	            STD_BEAM(std) = 1
		else
	            STD_BEAM(std) = beam
		STD_NPTS(std) = npts
	        STD_EXPTIME(std) = exptime
	        STD_AIRMASS(std) = airmass
		STD_WSTART(std) = wstart
		STD_WEND(std) = wend
		STD_SHIFT(std) = 0.
		STD_NWAVES(std) = 0

		# Decode the image and sky strings.
	        call strcpy (Memc[title], STD_TITLE(std), SZ_STDTITLE)
		i = stridxs ("]", Memc[image])
		if (Memc[image+i] == ']')
		    i = i + 1
		Memc[image+i-1] = EOS
	        call strcpy (Memc[image+1], STD_IMAGE(std), SZ_STDIMAGE)
		if (Memc[image+i] == '-') {
		    i = i + 2
		    j = stridxs ("]", Memc[image+i]) + i
		    Memc[image+j-1] = EOS
	            call strcpy (Memc[image+i], STD_SKY(std), SZ_STDIMAGE)
		} else
		    STD_SKY(std) = EOS
	        nwaves = 0

	    # Interprete the line as standard star wavelength point.
	    } else if (nstds > 0) {
		call reset_scan()
		call gargr (wavelength)
		call gargr (flux)
		call gargr (dwave)
		call gargr (count)
		if (nscan() < 3)
		    next
		if (wavelength < min (wstart, wend) ||
		    wavelength > max (wstart, wend) ||
		    flux<=0. || dwave<=0. || count<=0.)
		    next
		if (!rng_elementi (aps, beam))
		    next
		nwaves = nwaves + 1

		# Allocate in blocks to minimize the number of reallocations.
		if (nwaves == 1) {
		    nalloc = 100
		    call malloc (waves, nalloc, TY_REAL)
		    call malloc (fluxes, nalloc, TY_REAL)
		    call malloc (dwaves, nalloc, TY_REAL)
		    call malloc (counts, nalloc, TY_REAL)
		    call malloc (wts, nalloc, TY_REAL)
		} else if (nwaves > nalloc) {
		    nalloc = nalloc + 100
		    call realloc (waves, nalloc, TY_REAL)
		    call realloc (fluxes, nalloc, TY_REAL)
		    call realloc (dwaves, nalloc, TY_REAL)
		    call realloc (counts, nalloc, TY_REAL)
		    call realloc (wts, nalloc, TY_REAL)
		}

		# Record the data and compute the sensitivity.
		Memr[waves+nwaves-1] = wavelength
		Memr[fluxes+nwaves-1] = flux
		Memr[dwaves+nwaves-1] = dwave
		Memr[counts+nwaves-1] = count
		Memr[wts+nwaves-1] = 1.
	    }
	}

	# Finish up the last standard star and close the file.
	if (nstds > 0) {
	    STD_NWAVES(std) = nwaves
    	    if (nwaves > 0) {
		call realloc (waves, nwaves, TY_REAL)
		call realloc (fluxes, nwaves, TY_REAL)
		call realloc (dwaves, nwaves, TY_REAL)
		call realloc (counts, nwaves, TY_REAL)
		call realloc (wts, nwaves, TY_REAL)
		call malloc (sens, nwaves, TY_REAL)
		call malloc (fit, nwaves, TY_REAL)
		call malloc (iwts, nwaves, TY_REAL)
		call malloc (x, nwaves, TY_REAL)
		call malloc (y, nwaves, TY_REAL)
		call amovr (Memr[wts], Memr[iwts], nwaves)
		STD_WAVES(std) = waves
		STD_FLUXES(std) = fluxes
		STD_DWAVES(std) = dwaves
		STD_COUNTS(std) = counts
		STD_SENS(std) = sens
		STD_FIT(std) = fit
		STD_WTS(std) = wts
		STD_IWTS(std) = iwts
		STD_X(std) = x
		STD_Y(std) = y
	    }
	}
	call close (fd)
	call sfree (sp)

	# Add standard stars for any added points and composite points.
	nstds = nstds + 2
	call realloc (stds, nstds, TY_INT)
	call calloc (std, LEN_STD, TY_STRUCT)
	Memi[stds+nstds-2] = std
	call strcpy ("Added", STD_IMAGE(std), SZ_STDIMAGE)
	STD_BEAM(std) = STD_BEAM(Memi[stds])
	STD_NPTS(std) = STD_NPTS(Memi[stds])
	STD_EXPTIME(std) = 1.
	STD_AIRMASS(std) = 1.
	STD_WSTART(std) = STD_WSTART(Memi[stds])
	STD_WEND(std) = STD_WEND(Memi[stds])
	STD_SHIFT(std) = 0.
	STD_NWAVES(std) = 0
	call calloc (std, LEN_STD, TY_STRUCT)
	Memi[stds+nstds-1] = std
	call strcpy ("Composite", STD_IMAGE(std), SZ_STDIMAGE)
	STD_BEAM(std) = STD_BEAM(Memi[stds])
	STD_NPTS(std) = STD_NPTS(Memi[stds])
	STD_EXPTIME(std) = 1.
	STD_AIRMASS(std) = 1.
	STD_WSTART(std) = STD_WSTART(Memi[stds])
	STD_WEND(std) = STD_WEND(Memi[stds])
	STD_SHIFT(std) = 0.
	STD_NWAVES(std) = 0
end


# SF_FREE -- Free the standard observations and aperture array.

procedure sf_free (stds, nstds, apertures, napertures)

pointer	stds		# Pointer to standard observations
int	nstds		# Number of standard observations
pointer	apertures	# Pointer to apertures array
int	napertures	# Number of apertures

int	i
pointer	std

begin
	do i = 1, nstds {
	    std = Memi[stds+i-1]
	    if (STD_NWAVES(std) > 0) {
		call mfree (STD_WAVES(std), TY_REAL)
		call mfree (STD_FLUXES(std), TY_REAL)
		call mfree (STD_DWAVES(std), TY_REAL)
		call mfree (STD_COUNTS(std), TY_REAL)
		call mfree (STD_SENS(std), TY_REAL)
		call mfree (STD_FIT(std), TY_REAL)
		call mfree (STD_WTS(std), TY_REAL)
		call mfree (STD_IWTS(std), TY_REAL)
		call mfree (STD_X(std), TY_REAL)
		call mfree (STD_Y(std), TY_REAL)
	    }
	    call mfree (std, TY_STRUCT)
	}
	call mfree (stds, TY_INT)
	call mfree (apertures, TY_INT)
end
