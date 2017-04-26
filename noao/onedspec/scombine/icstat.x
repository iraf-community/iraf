include	<smw.h>
include	"icombine.h"


# IC_STATR -- Compute image statistics within spectrum.

procedure ic_statr (sh, lflag, rg, domode, domedian, domean, mode, median, mean)

pointer	sh			 # Spectrum structure
int	lflag			 # Data flag
pointer	rg			 # Wavelength ranges
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

int	i, n, npts
real	a, w
pointer	sp, data, dp, lp, mp
real	ic_moder(), asumr()
bool	ic_wisinrange()
double	shdr_lw()

include	"icombine.com"

begin
	mp = SX(sh)
	lp = SY(sh)
	npts = SN(sh)

	call smark (sp)
	call salloc (data, npts, TY_REAL)

	dp = data
	if (lflag == D_ALL && rg == NULL) {
	    if (dothresh) {
		do i = 1, npts {
		    a = Memr[lp]
		    if (a >= lthresh && a <= hthresh) {
			Memr[dp] = a
			dp = dp + 1
		    }
		    lp = lp + 1
		}
	    } else {
		do i = 1, npts {
		    Memr[dp] = Memr[lp]
		    dp = dp + 1
		    lp = lp + 1
		}
	    }
	} else if (lflag == D_MIX || rg != NULL) {
	    if (dothresh) {
		do i = 1, npts {
		    if (Memr[mp] == 0) {
			a = Memr[lp]
			if (a >= lthresh && a <= hthresh) {
			    w = shdr_lw (sh, double (i))
			    if (ic_wisinrange (rg, w)) {
				Memr[dp] = a
				dp = dp + 1
			    }
			}
		    }
		    mp = mp + 1
		    lp = lp + 1
		}
	    } else {
		do i = 1, npts {
		    if (Memr[mp] == 0) {
			w = shdr_lw (sh, double (i))
			if (ic_wisinrange (rg, w)) {
			    Memr[dp] = Memr[lp]
			    dp = dp + 1
			}
		    }
		    mp = mp + 1
		    lp = lp + 1
		}
	    }
	}

	n = dp - data
	if (n > 0) {
	    # Compute only statistics needed.
	    if (domode || domedian) {
		call asrtr (Memr[data], Memr[data], n)
		mode = ic_moder (Memr[data], n)
		median = Memr[data+n/2-1]
	    }
	    if (domean)
		mean = asumr (Memr[data], n) / n
	} else {
	    mode = INDEF
	    median = INDEF
	    mean = INDEF
	}

	call sfree (sp)
end


define	NMIN	10	# Minimum number of pixels for mode calculation
define	ZRANGE	0.8	# Fraction of pixels about median to use
define	ZSTEP	0.01	# Step size for search for mode
define	ZBIN	0.1	# Bin size for mode.

# IC_MODE -- Compute mode of an array.  The mode is found by binning
# with a bin size based on the data range over a fraction of the
# pixels about the median and a bin step which may be smaller than the
# bin size.  If there are too few points the median is returned.
# The input array must be sorted.

real procedure ic_moder (a, n)

real	a[n]			# Data array
int	n			# Number of points

int	i, j, k, nmax
real	z1, z2, zstep, zbin
real	mode
bool	fp_equalr()

begin
	if (n < NMIN)
	    return (a[n/2])

	# Compute the mode.  The array must be sorted.  Consider a
	# range of values about the median point.  Use a bin size which
	# is ZBIN of the range.  Step the bin limits in ZSTEP fraction of
	# the bin size.

	i = 1 + n * (1. - ZRANGE) / 2.
	j = 1 + n * (1. + ZRANGE) / 2.
	z1 = a[i]
	z2 = a[j]
	if (fp_equalr (z1, z2)) {
	    mode = z1
	    return (mode)
	}

	zstep = ZSTEP * (z2 - z1)
	zbin = ZBIN * (z2 - z1)

	z1 = z1 - zstep
	k = i
	nmax = 0
	repeat {
	    z1 = z1 + zstep
	    z2 = z1 + zbin
	    for (; i < j && a[i] < z1; i=i+1)
		;
	    for (; k < j && a[k] < z2; k=k+1)
		;
	    if (k - i > nmax) {
	        nmax = k - i
	        mode = a[(i+k)/2]
	    }
	} until (k >= j)

	return (mode)
end
