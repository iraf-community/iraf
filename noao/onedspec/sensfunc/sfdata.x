include	"sensfunc.h"

# SF_DATA -- Compute the X and Y values for the particular graph.

procedure sf_data (stds, nstds, graph)

pointer	stds[nstds]		# Standard star structures
int	nstds			# Number of standard stars
char	graph			# Graph type

real	a
int	i, n
pointer	wp, sp, fp, xp, yp

begin
	switch (graph) {
	case 's':	# Sensitivity vs. Wavelength
	    do i = 1, nstds {
	        if (STD_FLAG(stds[i]) != SF_INCLUDE)
		    next
		n = STD_NWAVES(stds[i])
		a = STD_AIRMASS(stds[i])
		wp = STD_WAVES(stds[i])
		sp = STD_SENS(stds[i])
		xp = STD_X(stds[i])
		yp = STD_Y(stds[i])
		call amovr (Memr[wp], Memr[xp], n)
		call amovr (Memr[sp], Memr[yp], n)
	    }
	case 'a':	# Residuals vs. Airmass
	    do i = 1, nstds {
	        if (STD_FLAG(stds[i]) != SF_INCLUDE)
		    next
		n = STD_NWAVES(stds[i])
		a = STD_AIRMASS(stds[i])
		wp = STD_WAVES(stds[i])
		sp = STD_SENS(stds[i])
		fp = STD_FIT(stds[i])
		xp = STD_X(stds[i])
		yp = STD_Y(stds[i])
		call amovkr (a, Memr[xp], n)
		call asubr (Memr[sp], Memr[fp], Memr[yp], n)
	    }
	case 'r':	# Residuals vs. Wavelength
	    do i = 1, nstds {
	        if (STD_FLAG(stds[i]) != SF_INCLUDE)
		    next
		n = STD_NWAVES(stds[i])
		a = STD_AIRMASS(stds[i])
		wp = STD_WAVES(stds[i])
		sp = STD_SENS(stds[i])
		fp = STD_FIT(stds[i])
		xp = STD_X(stds[i])
		yp = STD_Y(stds[i])
		call amovr (Memr[wp], Memr[xp], n)
		call asubr (Memr[sp], Memr[fp], Memr[yp], n)
	    }
	}
end
