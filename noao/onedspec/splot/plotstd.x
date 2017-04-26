include	<error.h>
include	<gset.h>
include	<smw.h>

define	VLIGHT	2.997925e18

# PLOT_STD -- Plot the flux values for a standard star on current screen

procedure plot_std (sh, gfd, fnu)

pointer	sh
int	gfd
bool	fnu

pointer	waves, bands, mags
int	i, nwaves
real	w1, w2
real	fnuzero, clgetr()
double	shdr_lw()

begin
	# Get calibration data.
	iferr (call getcalib (waves, bands, mags, nwaves)) {
	    call erract (EA_WARN)
	    return
	}

	# Convert to fnu or flambda
	fnuzero = clgetr ("fnuzero")
	do i = 1, nwaves {
	    Memr[mags+i-1] = fnuzero * 10.0**(-0.4 * Memr[mags+i-1])
	    if (!fnu)
		Memr[mags+i-1] = Memr[mags+i-1] * VLIGHT / Memr[waves+i-1]**2
	}

	# Overplot boxes on current plot
	w1 = shdr_lw (sh, double(1))
	w2 = shdr_lw (sh, double(SN(sh)))

	do i = 1, nwaves
	    if (Memr[waves+i-1] > w1 && Memr[waves+i-1] < w2)
		call plbox2 (gfd, Memr[waves+i-1]-Memr[bands+i-1]/2,
		    Memr[mags+i-1], Memr[waves+i-1]+Memr[bands+i-1]/2, .015)

	call freecalib (waves, bands, mags)
end

# PLBOX2 -- Plot a box of given height and width

procedure plbox2 (gfd, x1, y1, x2, ndcy)

int	gfd
real	x1, x2, y1, ndcy

real	ya1, ya2
real	wx1, wx2, wy1, wy2

begin
	# Get current WCS range
	call ggwind (gfd, wx1, wx2, wy1, wy2)

	# Adjust vertical spacing 
	ya1 = y1 - ndcy * (wy2 - wy1)
	ya2 = y1 + ndcy * (wy2 - wy1)

	call gline (gfd, x1, ya1, x2, ya1)
	call gline (gfd, x2, ya1, x2, ya2)
	call gline (gfd, x2, ya2, x1, ya2)
	call gline (gfd, x1, ya2, x1, ya1)
end
