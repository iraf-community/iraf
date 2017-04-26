include <mach.h>
include <ctype.h>
include <error.h>
include <pkg/rg.h>
include "pdm.h"

# PDM_AUTORANG -- Calculate the ranges division of the data automatically.

int procedure pdm_autorang (pdmp)

pointer pdmp			# PDM structure pointer

int	npt, i, nrng
double	sumsq, sum, var, sd, maxdif, meansep, rbegin, rend
int	rngstrt
pointer	sep, command, sp

begin
	call smark (sp)
	call salloc (command, SZ_LINE, TY_CHAR)

	npt = PDM_NPT(pdmp)

	# Calculate the mean and standard deviation of the x-axis
	# separation of the data points.  (time intervals)
	# Allocate an array of separations and fill it.

	call salloc (sep, npt-1, TY_DOUBLE)
	do i = 1, npt-1 {
	    Memd[sep+i-1] = PDM_X(pdmp,i+1) - PDM_X(pdmp,i)
	}

	sumsq = 0.0
	sum = 0.0
	for (i=1; i<=(npt-1); i=i+1) {
	    sumsq = sumsq + Memd[sep+i-1]**2	# Sum of squares.
	    sum = sum + Memd[sep+i-1]
	}
	if (npt != 1) {
	    var = (sumsq - sum**2/npt)/(npt - 1)	# Variance.
	    sd = var**.5				# Standard Deviation.
	}

	# Mean separation, maximum time diff.
	if (npt != 1) {
	    meansep = (PDM_X(pdmp,npt) - PDM_X(pdmp,1)) / double(npt-1)
	    maxdif = meansep + sd * PDM_NSIGMA(pdmp)
	}

	# Look through the separations and if we find one that is more
	# than nsigma away from the mean on the plus side, divide the
	# data at this point into another range.
	
	nrng = 0
	rngstrt = 1
	PDM_SAMPLE(pdmp) = EOS
	do i = 1, npt - 1 {
	    if (Memd[sep+i-1] > maxdif) {
		nrng = nrng + 1
		if (nrng > MAX_RANGES) {
		    call sfree (sp)
		    call error (0,"Max num ranges exceeded in autorange")
		    break
		}

		# End of last range = x(i)
		# If (i+1 != npts) beginning of next range = x(i+1)
		# Remember where the next range starts.

		rbegin = PDM_X(pdmp,rngstrt)
		rend = PDM_X(pdmp,i)
		if ((i+1) < npt)
		    rngstrt = i+1

		# Sprintf range info at end of string.
		call sprintf (Memc[command], SZ_LINE, " %g:%g")
		    call pargd (rbegin)
		    call pargd (rend)
		call strcat (Memc[command], PDM_SAMPLE(pdmp), SZ_LINE)
	    }
	}

	# Finish up last range if needed.
	If (rngstrt <= npt && rngstrt > 1) {
	    rbegin = PDM_X(pdmp,rngstrt)
	    rend = PDM_X(pdmp,i)

	    # Sprintf range info at end of string.
	    call sprintf (Memc[command], SZ_LINE, " %g:%g")
		call pargd (rbegin)
		call pargd (rend)
	    call strcat (Memc[command], PDM_SAMPLE(pdmp), SZ_LINE)
	}

	# If no ranges found, set the sample string to '*', all data.
	if (nrng == 0)
	    call sprintf (PDM_SAMPLE(pdmp), SZ_LINE, "*")

	call sfree (sp)
	return (nrng)
end
