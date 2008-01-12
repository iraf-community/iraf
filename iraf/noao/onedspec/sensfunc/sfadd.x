include	<gset.h>
include	"sensfunc.h"

# SF_ADD -- Add a point to the added point observation structure.
# The added star is the next to last of the standard stars.

procedure sf_add (gp, stds, nstds, cv, wx, wy, wc)

pointer	gp			# Graphics structure
pointer	stds[nstds]		# Standard star structures
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
real	wx			# Cursor X value
real	wy			# Cursor Y value
int	wc			# Cursor WCS

int	nwaves
real	wave, sen, fit, cveval()
pointer	std, waves, sens, fits, wts, iwts, x, y
errchk	malloc, realloc

begin
	# Convert from particular WCS to wavelength and sensitivity.  In
	# order to add a point the graph must be either sensitivity or
	# residual verses wavelength.  If not then return without adding
	# a point.

	switch (GP_GRAPHS(gp,wc)) {
	case 's':
	    wave = wx
	    fit = cveval (cv, wx)
	    sen = wy
	case 'r':
	    wave = wx
	    fit = cveval (cv, wx)
	    sen = wy + fit
	default:
	    return
	}

	# Add the point to the next to last standard star.  Allocate
	# or reallocate memory as needed.  Turn the added star on by
	# setting INCLUDE flag.

	std = stds[nstds-1]
	nwaves = STD_NWAVES(std) + 1
	waves = STD_WAVES(std)
	if (waves == NULL) {
	    call malloc (waves, nwaves, TY_REAL)
	    call malloc (sens, nwaves, TY_REAL)
	    call malloc (fits, nwaves, TY_REAL)
	    call malloc (wts, nwaves, TY_REAL)
	    call malloc (iwts, nwaves, TY_REAL)
	    call malloc (x, nwaves, TY_REAL)
	    call malloc (y, nwaves, TY_REAL)
	} else {
	    sens = STD_SENS(std)
	    fits = STD_FIT(std)
	    wts = STD_WTS(std)
	    iwts = STD_IWTS(std)
	    x = STD_X(std)
	    y = STD_Y(std)
	    call realloc (waves, nwaves, TY_REAL)
	    call realloc (sens, nwaves, TY_REAL)
	    call realloc (fits, nwaves, TY_REAL)
	    call realloc (wts, nwaves, TY_REAL)
	    call realloc (iwts, nwaves, TY_REAL)
	    call realloc (x, nwaves, TY_REAL)
	    call realloc (y, nwaves, TY_REAL)
	}
	STD_FLAG(std) = SF_INCLUDE
	STD_NWAVES(std) = nwaves
	STD_WAVES(std) = waves
	STD_SENS(std) = sens
	STD_FIT(std) = fits
	STD_WTS(std) = wts
	STD_IWTS(std) = iwts
	STD_X(std) = x
	STD_Y(std) = y

	Memr[waves+nwaves-1] = wave
	Memr[sens+nwaves-1] = sen
	Memr[fits+nwaves-1] = fit
	Memr[wts+nwaves-1] = 1
	Memr[iwts+nwaves-1] = 1

	# Mark the added point on all graphs.
	for (wc = 1; GP_GRAPHS(gp,wc) != EOS; wc=wc+1) {
	    call gseti (GP_GIO(gp), G_WCS, wc)
	    call gseti (GP_GIO(gp), G_PLCOLOR, GP_CADD(gp))
	    switch (GP_GRAPHS(gp,wc)) {
	    case 's':
 		call gmark (GP_GIO(gp), wave, sen, GP_MADD(gp), GP_SZMARK(gp),
		    GP_SZMARK(gp))
	    case 'r':
		wy = sen - cveval (cv, wave)
 		call gmark (GP_GIO(gp), wave, wy, GP_MADD(gp), GP_SZMARK(gp),
		    GP_SZMARK(gp))
	    case 'a':
		wy = sen - cveval (cv, wave)
 		call gmark (GP_GIO(gp), STD_AIRMASS(std), wy, GP_MADD(gp),
		    GP_SZMARK(gp), GP_SZMARK(gp))
	    }
	}
end
