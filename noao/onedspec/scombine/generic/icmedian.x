# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"


# IC_MEDIAN -- Median of lines

procedure ic_medianr (d, n, npts, median)

pointer	d[ARB]			# Input data line pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j1, j2, j3, k, n1
bool	even
real	val1, val2, val3

include	"../icombine.com"

begin
	if (dflag == D_NONE) {
	    do i = 1, npts
		median[i]= blank
	    return
	}

	# Check for previous sorting
	if (mclip) {
	    if (dflag == D_ALL) {
		n1 = n[1]
		even = (mod (n1, 2) == 0)
		j1 = n1 / 2 + 1
		j2 = n1 / 2
		do i = 1, npts {
		    k = i - 1
		    if (even)
			median[i] = (Memr[d[j1]+k] + Memr[d[j2]+k]) / 2.
		    else
			median[i] = Memr[d[j1]+k]
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    n1 = n[i]
		    if (n1 > 0) {
			j1 = n1 / 2 + 1
			if (mod (n1, 2) == 0) {
			    j2 = n1 / 2
			    median[i] = (Memr[d[j1]+k] + Memr[d[j2]+k]) / 2.
			} else
			    median[i] = Memr[d[j1]+k]
		    } else
			median[i] = blank
		}
	    }
	    return
	}

	# Repeatedly eliminate the extreme values until there are three
	# or fewer pixels.

	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    while (n1 > 3) {
		j1 = 1
		j2 = 1
	        val1 = Memr[d[j1]+k]
	        val2 = val1
	        do j3 = 2, n1 {
		    val3 = Memr[d[j3]+k]
	            if (val3 > val1) {
		        j1 = j3
		        val1 = val3
		    } else if (val3 < val2) {
		        j2 = j3
		        val2 = val3
		    }
	        }
		j3 = n1 - 1
		if (j1 < j3 && j2 < j3) {
		    Memr[d[j1]+k] = val3
		    Memr[d[j2]+k] = Memr[d[j3]+k]
		} else if (j1 < j3) {
		    if (j2 == j3)
			Memr[d[j1]+k] = val3
		    else
			Memr[d[j1]+k] = Memr[d[j3]+k]
		} else if (j2 < j3) {
		    if (j1 == j3)
			Memr[d[j2]+k] = val3
		    else
			Memr[d[j2]+k] = Memr[d[j3]+k]
		}
		n1 = n1 - 2
	    }

	    if (n1 == 3) {
	        val1 = Memr[d[1]+k]
	        val2 = Memr[d[2]+k]
	        val3 = Memr[d[3]+k]
	        if (val1 < val2) {
		    if (val2 < val3)		# abc
		        median[i] = val2
		    else if (val1 < val3)	# acb
		        median[i] = val3
		    else			# cab
		        median[i] = val1
	        } else {
		    if (val2 > val3)		# cba
		        median[i] = val2
		    else if (val1 > val3)	# bca
		        median[i] = val3
		    else			# bac
		        median[i] = val1
	        }
	    } else if (n1 == 2)
		median[i] = (Memr[d[1]+k] + Memr[d[2]+k]) / 2
	    else if (n1 == 1)
		median[i] = Memr[d[1]+k]
	    else
		median[i] = blank
	}
end
