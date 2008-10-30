# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_BIN -- Average or median of data.
#
# The ranges are broken up into subranges of at most abs (nbin) points.  The
# subranges are averaged if nbin > 1 and medianed if nbin < 1.
# The output array must be large enough to contain the desired points.
# If the ranges are merged then the input and output arrays may be the same.

procedure rg_bind (rg, nbin, in, nin, out, nout)

pointer	rg				# Ranges
long	nbin				# Maximum points in average or median
double	in[nin]				# Input array
size_t	nin				# Number of input points
double	out[ARB]			# Output array
size_t	nout				# Number of output points

long	i, j, k
size_t	n, npts, ntemp

double	asumd(), amedd()
long	absl()

errchk	rg_packd

begin
	# Error check the range pointer.

	if (rg == NULL)
	    call error (0, "Range descriptor undefined")

	# If the bin size is exactly one then move the selected input points
	# to the output array.

	if (absl (nbin) == 1) {
	    call rg_packd (rg, in, out)
	    return
	}

	# Determine the subranges and take the median or average.

	npts = absl (nbin)
	ntemp = 0

	do i = 1, RG_NRGS(rg) {
	    if (RG_X1(rg, i) > RG_X2(rg, i)) {
		j = min (nin, RG_X1(rg, i))
		k = max (1, RG_X2(rg, i))
		while (j >= k) {
		    n = max (0, min (npts, j - k + 1))
		    k = k - n
		    ntemp = ntemp + 1
		    if (nbin > 0)
		        out[ntemp] = asumd (in[k + 1], n) / n
		    else
			out[ntemp] = amedd (in[k+1], n)
		}
	    } else {
		j = max (1, RG_X1(rg, i))
		k = min (nin, RG_X2(rg, i))
	        while (j <= k) {
		    n = max (0, min (npts, k - j + 1))
		    ntemp = ntemp + 1
		    if (nbin > 0)
		        out[ntemp] = asumd (in[j], n) / n
		    else
			out[ntemp] = amedd (in[j], n)
		    j = j + n
		}
	    }
	}

	nout = ntemp
end
