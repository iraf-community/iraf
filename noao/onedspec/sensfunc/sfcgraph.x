include	<gset.h>
include	"sensfunc.h"

# SF_CGRAPH -- Graph of composite points and errors

procedure sf_cgraph (gp, stds, nstds, cv)

pointer	gp		# Graphics structure
pointer	stds[nstds]	# Standard star data
int	nstds		# Number of standard stars
pointer	cv		# Sensitivity function curve

int	i, j, n, nwaves
real	w, s, ymin, ymax, cveval()
double	sum, sum2
pointer	sp, waves, sens, errors, xp, yp, zp, gio

begin
	nwaves = 0
	do i = 1, nstds-2
	    if (STD_FLAG(stds[i]) != SF_EXCLUDE)
	        nwaves = nwaves + STD_NWAVES(stds[i])

	call smark (sp)
	call salloc (waves, nwaves, TY_REAL)
	call salloc (sens, nwaves, TY_REAL)
	call salloc (errors, nwaves, TY_REAL)
	    
	nwaves = 0
	do i = 1, nstds-2 {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    xp = STD_WAVES(stds[i])
	    yp = STD_SENS(stds[i])
	    zp = STD_WTS(stds[i])
	    do j = 1, n {
		if (Memr[zp] != 0.) {
		    Memr[waves+nwaves] = Memr[xp]
		    Memr[sens+nwaves] = Memr[yp]
		    nwaves = nwaves + 1
		}
		xp = xp + 1
		yp = yp + 1
		zp = zp + 1
	    }
	}
	call xt_sort2 (Memr[waves], Memr[sens], nwaves)

	n = 0
	sum = 0.
	sum2 = 0.
	ymin = 0.
	ymax = 0.
	j = 0
	do i = 1, nwaves {
	    w = Memr[waves+i-1]
	    s = Memr[sens+i-1]
	    n = n + 1
	    sum = sum + s
	    sum2 = sum2 + s * s

	    if ((i < nwaves) && (w == Memr[waves+i]))
		next

	    if (n > 2) {
	        sum = sum / n
	        sum2 = sum2 / n - sum * sum
	        if (sum2 > 0)
	            sum2 = sqrt (sum2 / n)
	        else
		    sum2 = 0.
	        sum = sum - cveval (cv, w)

		Memr[waves+j] = w
		Memr[sens+j] = sum
		Memr[errors+j] = sum2
		j = j + 1

		if (sum + sum2 > ymax)
		    ymax = sum + sum2
		if (sum - sum2 < ymin)
		    ymin = sum - sum2
	    }
	    n = 0
	    sum = 0.
	    sum2 = 0.
	}
	nwaves = j

	if (j == 0) {
	    call printf ("No wavelength overlap for composite points")
	} else {
	    gio = GP_GIO(gp)
	    call gswind (gio, GP_WSTART(gp), GP_WEND(gp), ymin, ymax)
	    call glabax (gio, "Composite Points vs Wavelength", "", "")
	    call gseti (gio, G_PLCOLOR, GP_CMARK(gp))
	    do i = 0, nwaves-1
	        call gmark (gio, Memr[waves+i], Memr[sens+i], GM_VEBAR,
		    1., -Memr[errors+i])
	}

	call sfree (sp)
end
