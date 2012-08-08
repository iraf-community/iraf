include	"sensfunc.h"

# SF_COMPOSITE -- Create a composite standard structure.
# The composite star is the last of the standard stars.
# When the composite star is created the other stars are turned off.
# The function toggles.

procedure sf_composite (stds, nstds, cv)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity pointer

int	i, j, k, n, nwaves
pointer	std, waves, sens, fit, wts, iwts, x, y, z
errchk	malloc, realloc, xt_sort3

begin
	# If data is already composite toggle back to original data.
	# Delete data points if composite point is deleted.
	std = stds[nstds]
	if (STD_FLAG(std) == SF_INCLUDE) {
            do i = 1, nstds - 2 {
	        if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		    next
	        STD_FLAG(stds[i]) = SF_INCLUDE
	    }
	    STD_FLAG(std) = SF_EXCLUDE

	    n = STD_NWAVES(std)
	    x = STD_WAVES(std)
	    z = STD_WTS(std)
	    do i = 1, n {
		if (Memr[z] == 0.) {
		    do j = 1, nstds - 2 {
		        if (STD_FLAG(stds[j]) == SF_EXCLUDE)
			    next
		        nwaves = STD_NWAVES(stds[j])
			waves = STD_WAVES(stds[j])
		        wts = STD_WTS(stds[j])
			do k = 1, nwaves {
			    if (Memr[waves] == Memr[x])
		                Memr[wts] = 0.
			    waves = waves + 1
			    wts = wts + 1
			}
		    }
		}
		x = x + 1
		z = z + 1
	    }
	    call printf ("Individual star data")
	    return
	}

	# Initialize
	if (STD_WAVES(std) != NULL) {
	    call mfree (STD_WAVES(std), TY_REAL)
	    call mfree (STD_SENS(std), TY_REAL)
	    call mfree (STD_WTS(std), TY_REAL)
	    call mfree (STD_IWTS(std), TY_REAL)
	    call mfree (STD_X(std), TY_REAL)
	    call mfree (STD_Y(std), TY_REAL)
	}

	# To bin the data we collect all the data and then sort by wavelength.
	nwaves = 0
	do i = 1, nstds - 2
	    if (STD_FLAG(stds[i]) == SF_INCLUDE)
	        nwaves = nwaves + STD_NWAVES(stds[i])

	call malloc (waves, nwaves, TY_REAL)
	call malloc (sens, nwaves, TY_REAL)
	call malloc (wts, nwaves, TY_REAL)
	    
	nwaves = 0
	do i = 1, nstds - 2 {
	    if (STD_FLAG(stds[i]) != SF_INCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    x = STD_WAVES(stds[i])
	    y = STD_SENS(stds[i])
	    z = STD_WTS(stds[i])
	    do j = 1, n {
		if (Memr[z] != 0.) {
		    Memr[waves+nwaves] = Memr[x]
		    Memr[sens+nwaves] = Memr[y]
		    Memr[wts+nwaves] = Memr[z]
		    nwaves = nwaves + 1
		}
		x = x + 1
		y = y + 1
		z = z + 1
	    }
	    STD_FLAG(stds[i]) = SF_DELETE
	    STD_BEAM(std) = STD_BEAM(stds[i])
	    STD_WSTART(std) = STD_WSTART(stds[i])
	    STD_WEND(std) = STD_WEND(stds[i])
	}
#	STD_NWAVES(stds[nstds-1]) = 0

	call xt_sort3 (Memr[waves], Memr[sens], Memr[wts], nwaves)

	# Go through the wavelength sorted data and composite all points
	# with the same wavelength.

	n = 0
	Memr[sens] = Memr[wts] * Memr[sens]
	do i = 1, nwaves-1 {
	    if (Memr[waves+i] == Memr[waves+n]) {
		Memr[sens+n] = Memr[sens+n] + Memr[wts+i] * Memr[sens+i]
		Memr[wts+n] = Memr[wts+n] + Memr[wts+i]
	    } else {
		n = n + 1
		Memr[waves+n] = Memr[waves+i]
		Memr[sens+n] = Memr[wts+i] * Memr[sens+i]
		Memr[wts+n] = Memr[wts+i]
	    }
	}

	nwaves = n + 1
	do i = 0, nwaves-1
	    Memr[sens+i] = Memr[sens+i] / Memr[wts+i]

	# Store the composite data in the standard star structure.
	call realloc (waves, nwaves, TY_REAL)
	call realloc (sens, nwaves, TY_REAL)
	call realloc (wts, nwaves, TY_REAL)
	call malloc (iwts, nwaves, TY_REAL)
	call malloc (fit, nwaves, TY_REAL)
	call malloc (x, nwaves, TY_REAL)
	call malloc (y, nwaves, TY_REAL)
	call amovr (Memr[wts], Memr[iwts], nwaves)
	call cvvector (cv, Memr[waves], Memr[fit], nwaves)

	STD_FLAG(std) = SF_INCLUDE
	STD_NWAVES(std) = nwaves
	STD_WAVES(std) = waves
	STD_SENS(std) = sens
	STD_FIT(std) = fit
	STD_WTS(std) = wts
	STD_IWTS(std) = iwts
	STD_X(std) = x
	STD_Y(std) = y

	call printf ("Composite star data")
end
