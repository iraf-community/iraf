include	"sensfunc.h"

# SF_RESET -- Reset the standard star data to the original input.
# This is called cancel changes and start over.

procedure sf_reset (stds, nstds, wextn, extn, nextn, ecv, shift)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
real	wextn[nextn]		# Extinction table wavelengths
real	extn[nextn]		# Extinction table values
int	nextn			# Number of extinction values
pointer	ecv			# Residual extinction curve
int	shift			# Shift flag

int	i, j, n, err
real	exptime, airmass, ext
pointer	waves, fluxes, dwaves, counts, sens, iwts, wts

begin
	# Reset the flags, sensitivity, and weight values.
	shift = NO
        do i = 1, nstds - 2 {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next
	    STD_FLAG(stds[i]) = SF_INCLUDE
	    STD_SHIFT(stds[i]) = 0.
	    n = STD_NWAVES(stds[i])
	    exptime = STD_EXPTIME(stds[i])
	    airmass = STD_AIRMASS(stds[i])
	    waves = STD_WAVES(stds[i])
	    fluxes = STD_FLUXES(stds[i])
	    dwaves = STD_DWAVES(stds[i])
	    counts = STD_COUNTS(stds[i])
	    sens = STD_SENS(stds[i])
	    iwts = STD_IWTS(stds[i])
	    wts = STD_WTS(stds[i])
	    do j = 1, n {
	        call intrp (1, wextn, extn, nextn, Memr[waves], ext, err)
		Memr[sens] = Memr[counts] /
		    (Memr[fluxes] * Memr[dwaves] * exptime)
		Memr[sens] = 2.5 * log10 (Memr[sens]) + airmass * ext
		Memr[wts] = Memr[iwts]

		waves = waves + 1
		fluxes = fluxes + 1
		dwaves = dwaves + 1
		counts = counts + 1
		sens = sens + 1
		iwts = iwts + 1
		wts = wts + 1
	    }
	}

	# Reset the added and composite stars.
	STD_NWAVES(stds[nstds-1]) = 0
	STD_FLAG(stds[nstds-1]) = SF_DELETE
	STD_FLAG(stds[nstds]) = SF_EXCLUDE

	# Clear the residual extinction curve.
	call cvfree (ecv)
end
