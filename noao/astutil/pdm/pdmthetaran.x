include <mach.h>
include <ctype.h>
include <error.h>
include <pkg/rg.h>
include "pdm.h"

# PDMTHETARAN -- This program is a copy of pdmtheta but can be used on
# scrambled data.

double procedure pdm_thetaran (pdmp, y, inuse, rg, period)

pointer	pdmp			# pointer to PDM data structure
pointer	y			# pointer to abcissas
pointer	inuse			# pointer to PDM in-use array 
pointer	rg			# pointer to ranges structure
double	period			# period to calculate theta for

int	i, j, k, l, m
double	s2
int	ndof, segst, segend
bool	bin10
double	theta
pointer	sumbin, sum2bin, numbin, sp
errchk	binemran

begin
	# Allocate bin storage.
	call smark (sp)
	call salloc (sumbin, 10, TY_DOUBLE)
	call salloc (sum2bin, 10, TY_DOUBLE)
	call salloc (numbin, 10, TY_INT)

	s2 = 0
	ndof = 0

	# Do loop on the segments.
	do i = 1, RG_NRGS(rg) {

	    # Calculate segst, segend, bin10.
	    segst = min(RG_X2(rg,i),RG_X1(rg,i))
	    segend = max(RG_X2(rg,i),RG_X1(rg,i))
	    bin10 = ((segend - segst) >= BIN10)

	    # Calculate the number of points in each bin and the sum of 
	    # the bins.

	    call binemran (period, bin10, PDM_XP(pdmp), y, segst,
		segend, inuse, sumbin, sum2bin, numbin)

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


# BINEMRAN -- Put the data points into the appropriate bins (scrambled data).

procedure binemran (incper, bin10, x, y, segst, segend, inuse, sumbin, sum2bin,
	numbin)

double	incper
bool	bin10
pointer	x
pointer	y
pointer	inuse
int	segst, segend
pointer	sumbin, sum2bin, numbin

int	bin1, bin2, j, k, l, m
double	p, phase, p0

begin
	do j = 1, 10 {
	    Memi[numbin+j-1] = 0
	    Memd[sumbin+j-1] = 0.0
	    Memd[sum2bin+j-1] = 0.0
	}

	p0 = Memd[x]
	do j = segst, segend {
	    if (Memi[inuse+j-1] == 0)
		next
	    p = (Memd[x+j-1] - p0)/incper
	    phase = double(p - int(p))
	    if (bin10) {
		bin1 = mod(int(10.*phase+0.5d+0), 10)
	    } else {
		bin1 = 2 * int(5.0d+0 * phase) + 1
		bin2 = 2 * (mod(int(5.0d+0 * phase + 0.5d+0), 5))
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
