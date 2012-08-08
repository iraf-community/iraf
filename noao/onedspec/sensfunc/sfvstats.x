include	"sensfunc.h"

# SF_VSTATS -- Verbose statistics output.

procedure sf_vstats (fd, stds, nstds, cv, wextn, extn, nextn, ecv)

int	fd		# Output file descriptor (may be STDOUT)
pointer	stds[nstds]	# Standard star data
int	nstds		# Number of standard stars
pointer	cv		# Sensitivity function curve
real	wextn[nextn]	# Extinction table wavelength
real	extn[nextn]	# Extinction table values
int	nextn		# Number of extinction table values
pointer	ecv		# Residual extinction curve

int	i, j, n, nwaves
real	w, fit, ext, dext, cveval()
double	sum, sum2, s
pointer	sp, waves, sens, xp, yp, zp

begin
	nwaves = 0
	do i = 1, nstds-1
	    if (STD_FLAG(stds[i]) != SF_EXCLUDE)
	        nwaves = nwaves + STD_NWAVES(stds[i])

	call smark (sp)
	call salloc (waves, nwaves, TY_REAL)
	call salloc (sens, nwaves, TY_REAL)
	    
	nwaves = 0
	do i = 1, nstds-1 {
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

	call fprintf (fd, "%8s %7s %7s %7s %7s %5s %7s %7s\n")
	    call pargstr ("Lambda")
	    call pargstr ("Fit")
	    call pargstr ("Avg")
	    call pargstr ("Resid")
	    call pargstr ("SD Avg")
	    call pargstr ("N")
	    call pargstr ("Ext")
	    call pargstr ("Dext")

	dext = 0.
	n = 0
	sum = 0.
	sum2 = 0.
	do i = 0, nwaves-1 {
	    w = Memr[waves+i]
	    s = Memr[sens+i]
	    n = n + 1
	    sum = sum + s
	    sum2 = sum2 + s * s

	    if ((i < nwaves-1) && (w == Memr[waves+i+1]))
		next

	    sum = sum / n
	    sum2 = sum2 / n - sum * sum
	    if (sum2 > 0)
	        sum2 = sqrt (sum2 / n)
	    else
		sum2 = 0.
	    fit = cveval (cv, w)
	    call intrp (1, wextn, extn, nextn, w, ext, j)
	    if (ecv != NULL)
	        dext = cveval (ecv, w)
	    call fprintf (fd, "%8.2f %7.3f %7.3f %7.4f %7.4f %5d %7.4f %7.4f\n")
		call pargr (w)
		call pargr (fit)
		call pargd (sum)
		call pargd (sum - fit)
		call pargd (sum2)
		call pargi (n)
		call pargr (ext)
		call pargr (dext)
	    n = 0
	    sum = 0.
	    sum2 = 0.
	}

	# Trailing spacer
	call fprintf (fd, "\n")

	call sfree (sp)
end
