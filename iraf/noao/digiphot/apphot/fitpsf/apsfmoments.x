include <math.h>
include "../lib/fitpsf.h"

define	NPARAMETERS	7

# APSFMOMENTS -- Procedure to compute the 0, 1st and second moments of an 
# image and estimate the x,y center, the ellipticity and the position angle.

int procedure apsfmoments (ctrpix, nx, ny, lthreshold, uthreshold, positive,
	par, perr, npar)

real	ctrpix[nx, ny]		# object to be centered
int	nx, ny			# dimensions of subarray
real	lthreshold		# lower threshold for moment computation
real	uthreshold		# upper threshold for moment computation
int	positive		# emission feature
real	par[ARB]		# parameters
real	perr[ARB]		# errors in parameters
int	npar			# number of parameters

int	i, j
real	temp, sumi, sumxi, sumyi, sumx2i, sumy2i, sumxyi, r2, varx, vary, varxy
bool	fp_equalr()

begin
	# Initialize the sums.
	sumi = 0.0
	sumxi = 0.0
	sumyi = 0.0
	sumxyi = 0.0
	sumx2i = 0.0
	sumy2i = 0.0

	# Accumulate the moments.
	if (positive == YES) {
	    do j = 1, ny {
	        do i = 1, nx {
		    if (ctrpix[i,j] > uthreshold)
			next
		    temp = ctrpix[i,j] - lthreshold
		    if (temp <= 0.0)
		        next
		    sumi = sumi + temp
		    sumxi = sumxi + i * temp
		    sumyi = sumyi + j * temp
		    sumxyi = sumxyi + i * j * temp
		    sumx2i = sumx2i + i * i * temp
		    sumy2i = sumy2i + j * j * temp
	        }
	    }
	} else {
	    do j = 1, ny {
	        do i = 1, nx {
		    if (ctrpix[i,j] < uthreshold)
			next
		    temp = lthreshold - ctrpix[i,j]
		    if (temp <= 0.0)
		        next
		    sumi = sumi + temp
		    sumxi = sumxi + i * temp
		    sumyi = sumyi + j * temp
		    sumxyi = sumxyi + i * j * temp
		    sumx2i = sumx2i + i * i * temp
		    sumy2i = sumy2i + j * j * temp
	        }
	    }
	}

	# Compute the parameters.
	if (fp_equalr (sumi, 0.0)) {
	    par[1] = 0.0
	    par[2] = (1 + nx) / 2.0
	    par[3] = (1 + ny) / 2.0
	    par[4] = 0.0
	    par[5] = 0.0
	    par[6] = 0.0
	    par[7] = lthreshold
	} else {
	    par[1] = sumi
	    par[2] = sumxi / sumi
	    par[3] = sumyi / sumi
	    varx = max (0.0, sumx2i / sumi - par[2] ** 2)
	    vary = max (0.0, sumy2i / sumi - par[3] ** 2)
	    r2 = varx + vary
	    if (r2 <= 0.0) {
		par[4] = 0.0
		par[5] = 0.0
		par[6] = 0.0
	    } else {
	        par[4] = sqrt (r2)
	        varxy = sumxyi / sumi - par[2] * par[3]
	        par[5] = sqrt ((varx - vary) ** 2 + 4.0 * varxy ** 2) / r2
		par[6] = (0.5 * r2 * (1.0 + par[5]) - vary)
		if (fp_equalr (par[6], 0.0))
		    par[6] = 90.0
		else
		    par[6] = RADTODEG (atan (varxy / par[6]))
	    }
	    par[7] = lthreshold
	}

	# Compute the errors.
	npar = NPARAMETERS
	call amovkr (0.0, perr, NPARAMETERS)
	return (AP_OK)
end
