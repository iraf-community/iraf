include <mach.h>
include <ctype.h>
include <error.h>
include <pkg/rg.h>
include "pdm.h"

# PDM_THETA -- Calculate the theta statistic for this period.
# Theta is a measure of the dispersion of data phases about a mean
# light curve.

double procedure pdm_theta (pdmp, rg, period)

pointer	pdmp			# pointer to PDM data structure
pointer	rg			# segments structure pointer
double	period			# period at which to find theta

int	i, j, k, l, m
double	s2
int	ndof, segst, segend
bool	bin10
double	theta
pointer	sumbin, sum2bin, numbin, sp
errchk	binem

begin
	# Allocate bin storage.
	call smark (sp)
	call salloc (sumbin, 10, TY_DOUBLE)
	call salloc (sum2bin, 10, TY_DOUBLE)
	call salloc (numbin, 10, TY_INT)

	ndof = 0
	s2 = 0

	# Do loop on the segments.
	do i = 1, RG_NRGS(rg) {

	    # Calculate segst, segend, bin10.
	    segst = min(RG_X2(rg,i),RG_X1(rg,i))
	    segend = max(RG_X2(rg,i),RG_X1(rg,i))
	    bin10 = ((segend - segst) >= BIN10)

	    # Calculate the number of points in each bin and the sum of 
	    # the bins.

	    call binem (period, bin10, PDM_XP(pdmp), PDM_DYP(pdmp),
		segst, segend, PDM_INUSEP(pdmp), sumbin, sum2bin, numbin)

	    # Calculate sigma**2 for this period.
	    for (j=0; j<=9; j=j+1) {
	        k = numbin+j
	        l = sumbin+j
	        m = sum2bin+j
	        if (Memi[k] > 1) {
		    ndof = ndof + Memi[k] - 1
		    s2 = s2 +  (Memd[m] - Memd[l]**2 / Memi[k])
	        }
	    }
	}

	# Calculate theta.
	theta = (s2 / double(ndof)) / PDM_DVAR(pdmp)

	call sfree (sp)

	return (theta)

end


# BINEM -- Put the data points into the appropriate bins.

procedure binem(incper, bin10, x, y, segst, segend, inuse, sumbin, sum2bin,
	numbin)

double	incper			# period increment
bool	bin10			# use 5 or 10 bins flag
pointer	x			# ordinates
pointer	y			# abcissas
pointer	inuse			# PDM in-use array
int	segst, segend		# segment start and segment end
pointer	sumbin, sum2bin, numbin	# pointers to bins of sum, sum2, and number

int	bin1, bin2, j, k, l, m
double	p, phase, p0

begin
	do j = 1, 10 {
	    Memi[numbin+j-1] = 0
	    Memd[sumbin+j-1] = 0.0
	    Memd[sum2bin+j-1] = 0.0
	}

	#p0 = Memd[x]
	call alimd (Memd[x+segst-1], segend-segst+1, p0, p)
	do j = segst, segend {
	    if (Memi[inuse+j-1] == 0)
		next
	    p = (Memd[x+j-1] - p0)/incper
	    phase = double(p - int(p))
	    if (bin10) {
		bin1 = mod(int(10.*phase+0.5), 10)
	    } else {
		bin1 = 2 * int(5. * phase) + 1
		bin2 = 2 * (mod(int(5. * phase + 0.5), 5))
		k = numbin+bin2
		l = sumbin+bin2
		m = sum2bin+bin2
		Memi[k] = Memi[k] + 1
		Memd[l] = Memd[l] + Memd[y+j-1]
		Memd[m] = Memd[m] + Memd[y+j-1] ** 2
	    }
	    k = numbin+bin1
	    l = sumbin+bin1
	    m = sum2bin+bin1
	    Memi[k] = Memi[k] + 1
	    Memd[l] = Memd[l] + Memd[y+j-1]
	    Memd[m] = Memd[m] + Memd[y+j-1] ** 2
	}
end
