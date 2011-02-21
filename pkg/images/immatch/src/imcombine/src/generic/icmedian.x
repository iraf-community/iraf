# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"


# IC_MEDIAN -- Median of lines

procedure ic_medians (d, n, npts, doblank, median)

pointer	d[ARB]			# Input data line pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line
int	doblank			# Set blank values?
real	median[npts]		# Median

int	i, j, k, j1, j2, n1, lo, up, lo1, up1
bool	even
real	val1, val2, val3
short	temp, wtemp

include	"../icombine.com"

begin
	# If no data return after possibly setting blank values.
	if (dflag == D_NONE) {
	    if (doblank == YES) {
		do i = 1, npts
		    median[i]= blank
	    }
	    return
	}

	# If the data were previously sorted then directly compute the median.
	if (mclip) {
	    if (dflag == D_ALL) {
		n1 = n[1]
		j1 = n1 / 2 + 1
		j2 = n1 / 2
		even = (mod(n1,2)==0 && (medtype==MEDAVG || n1>2))
		do i = 1, npts {
		    k = i - 1
		    if (even) {
			val1 = Mems[d[j1]+k]
			val2 = Mems[d[j2]+k]
			median[i] = (val1 + val2) / 2.
		    } else
			median[i] = Mems[d[j1]+k]
		}
		return
	    } else {
	        # Check for negative n values.  If found then there are
		# pixels with no good values but with values we want to
		# use as a substitute median.  In this case ignore that
		# the good pixels have been sorted.
		do i = 1, npts {
		    if (n[i] < 0)
		        break
		}

		if (n[i] >= 0) {
		    do i = 1, npts {
			k = i - 1
			n1 = n[i]
			if (n1 > 0) {
			    j1 = n1 / 2 + 1
			    if (mod(n1,2)==0 && (medtype==MEDAVG || n1>2)) {
				j2 = n1 / 2
				val1 = Mems[d[j1]+k]
				val2 = Mems[d[j2]+k]
				median[i] = (val1 + val2) / 2.
			    } else
				median[i] = Mems[d[j1]+k]
			} else if (doblank == YES)
			    median[i] = blank
		    }
		    return
		}
	    }
	}

	# Compute the median.
	do i = 1, npts {
	    k = i - 1
	    n1 = abs(n[i])

	    # If there are more than 3 points use Wirth algorithm.  This
	    # is the same as vops$amed.gx except for an even number of
	    # points it selects the middle two and averages.
	    if (n1 > 3) {
		lo = 1
		up = n1
		j  = max (lo, min (up, (up+1)/2))

		while (lo < up) {
		    if (! (lo < up))
			break

		    temp = Mems[d[j]+k];  lo1 = lo;  up1 = up

		    repeat {
			while (Mems[d[lo1]+k] < temp)
			    lo1 = lo1 + 1
			while (temp < Mems[d[up1]+k])
			    up1 = up1 - 1
			if (lo1 <= up1) {
			    wtemp = Mems[d[lo1]+k]
			    Mems[d[lo1]+k] = Mems[d[up1]+k]
			    Mems[d[up1]+k] = wtemp
			    lo1 = lo1 + 1;  up1 = up1 - 1
			}
		    } until (lo1 > up1)

		    if (up1 < j)
			lo = lo1
		    if (j < lo1)
			up = up1
		}

		median[i] = Mems[d[j]+k]

		if (mod(n1,2)==0 && (medtype==MEDAVG || n1 > 2)) {
		    lo = 1
		    up = n1
		    j  = max (lo, min (up, (up+1)/2)+1)

		    while (lo < up) {
			if (! (lo < up))
			    break

			temp = Mems[d[j]+k];  lo1 = lo;  up1 = up

			repeat {
			    while (Mems[d[lo1]+k] < temp)
				lo1 = lo1 + 1
			    while (temp < Mems[d[up1]+k])
				up1 = up1 - 1
			    if (lo1 <= up1) {
				wtemp = Mems[d[lo1]+k]
				Mems[d[lo1]+k] = Mems[d[up1]+k]
				Mems[d[up1]+k] = wtemp
				lo1 = lo1 + 1;  up1 = up1 - 1
			    }
			} until (lo1 > up1)

			if (up1 < j)
			    lo = lo1
			if (j < lo1)
			    up = up1
		    }
		    median[i] = (median[i] + Mems[d[j]+k]) / 2
		}

	    # If 3 points find the median directly.
	    } else if (n1 == 3) {
	        val1 = Mems[d[1]+k]
	        val2 = Mems[d[2]+k]
	        val3 = Mems[d[3]+k]
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

	    # If 2 points average.
	    } else if (n1 == 2) {
		val1 = Mems[d[1]+k]
		val2 = Mems[d[2]+k]
		if (medtype == MEDAVG)
		    median[i] = (val1 + val2) / 2
		else
		    median[i] = min (val1, val2)

	    # If 1 point return the value.
	    } else if (n1 == 1)
		median[i] = Mems[d[1]+k]

	    # If no points return with a possibly blank value.
	    else if (doblank == YES)
		median[i] = blank
	}
end

# IC_MEDIAN -- Median of lines

procedure ic_mediani (d, n, npts, doblank, median)

pointer	d[ARB]			# Input data line pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line
int	doblank			# Set blank values?
real	median[npts]		# Median

int	i, j, k, j1, j2, n1, lo, up, lo1, up1
bool	even
real	val1, val2, val3
int	temp, wtemp

include	"../icombine.com"

begin
	# If no data return after possibly setting blank values.
	if (dflag == D_NONE) {
	    if (doblank == YES) {
		do i = 1, npts
		    median[i]= blank
	    }
	    return
	}

	# If the data were previously sorted then directly compute the median.
	if (mclip) {
	    if (dflag == D_ALL) {
		n1 = n[1]
		j1 = n1 / 2 + 1
		j2 = n1 / 2
		even = (mod(n1,2)==0 && (medtype==MEDAVG || n1>2))
		do i = 1, npts {
		    k = i - 1
		    if (even) {
			val1 = Memi[d[j1]+k]
			val2 = Memi[d[j2]+k]
			median[i] = (val1 + val2) / 2.
		    } else
			median[i] = Memi[d[j1]+k]
		}
		return
	    } else {
	        # Check for negative n values.  If found then there are
		# pixels with no good values but with values we want to
		# use as a substitute median.  In this case ignore that
		# the good pixels have been sorted.
		do i = 1, npts {
		    if (n[i] < 0)
		        break
		}

		if (n[i] >= 0) {
		    do i = 1, npts {
			k = i - 1
			n1 = n[i]
			if (n1 > 0) {
			    j1 = n1 / 2 + 1
			    if (mod(n1,2)==0 && (medtype==MEDAVG || n1>2)) {
				j2 = n1 / 2
				val1 = Memi[d[j1]+k]
				val2 = Memi[d[j2]+k]
				median[i] = (val1 + val2) / 2.
			    } else
				median[i] = Memi[d[j1]+k]
			} else if (doblank == YES)
			    median[i] = blank
		    }
		    return
		}
	    }
	}

	# Compute the median.
	do i = 1, npts {
	    k = i - 1
	    n1 = abs(n[i])

	    # If there are more than 3 points use Wirth algorithm.  This
	    # is the same as vops$amed.gx except for an even number of
	    # points it selects the middle two and averages.
	    if (n1 > 3) {
		lo = 1
		up = n1
		j  = max (lo, min (up, (up+1)/2))

		while (lo < up) {
		    if (! (lo < up))
			break

		    temp = Memi[d[j]+k];  lo1 = lo;  up1 = up

		    repeat {
			while (Memi[d[lo1]+k] < temp)
			    lo1 = lo1 + 1
			while (temp < Memi[d[up1]+k])
			    up1 = up1 - 1
			if (lo1 <= up1) {
			    wtemp = Memi[d[lo1]+k]
			    Memi[d[lo1]+k] = Memi[d[up1]+k]
			    Memi[d[up1]+k] = wtemp
			    lo1 = lo1 + 1;  up1 = up1 - 1
			}
		    } until (lo1 > up1)

		    if (up1 < j)
			lo = lo1
		    if (j < lo1)
			up = up1
		}

		median[i] = Memi[d[j]+k]

		if (mod(n1,2)==0 && (medtype==MEDAVG || n1 > 2)) {
		    lo = 1
		    up = n1
		    j  = max (lo, min (up, (up+1)/2)+1)

		    while (lo < up) {
			if (! (lo < up))
			    break

			temp = Memi[d[j]+k];  lo1 = lo;  up1 = up

			repeat {
			    while (Memi[d[lo1]+k] < temp)
				lo1 = lo1 + 1
			    while (temp < Memi[d[up1]+k])
				up1 = up1 - 1
			    if (lo1 <= up1) {
				wtemp = Memi[d[lo1]+k]
				Memi[d[lo1]+k] = Memi[d[up1]+k]
				Memi[d[up1]+k] = wtemp
				lo1 = lo1 + 1;  up1 = up1 - 1
			    }
			} until (lo1 > up1)

			if (up1 < j)
			    lo = lo1
			if (j < lo1)
			    up = up1
		    }
		    median[i] = (median[i] + Memi[d[j]+k]) / 2
		}

	    # If 3 points find the median directly.
	    } else if (n1 == 3) {
	        val1 = Memi[d[1]+k]
	        val2 = Memi[d[2]+k]
	        val3 = Memi[d[3]+k]
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

	    # If 2 points average.
	    } else if (n1 == 2) {
		val1 = Memi[d[1]+k]
		val2 = Memi[d[2]+k]
		if (medtype == MEDAVG)
		    median[i] = (val1 + val2) / 2
		else
		    median[i] = min (val1, val2)

	    # If 1 point return the value.
	    } else if (n1 == 1)
		median[i] = Memi[d[1]+k]

	    # If no points return with a possibly blank value.
	    else if (doblank == YES)
		median[i] = blank
	}
end

# IC_MEDIAN -- Median of lines

procedure ic_medianr (d, n, npts, doblank, median)

pointer	d[ARB]			# Input data line pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line
int	doblank			# Set blank values?
real	median[npts]		# Median

int	i, j, k, j1, j2, n1, lo, up, lo1, up1
bool	even
real	val1, val2, val3
real	temp, wtemp

include	"../icombine.com"

begin
	# If no data return after possibly setting blank values.
	if (dflag == D_NONE) {
	    if (doblank == YES) {
		do i = 1, npts
		    median[i]= blank
	    }
	    return
	}

	# If the data were previously sorted then directly compute the median.
	if (mclip) {
	    if (dflag == D_ALL) {
		n1 = n[1]
		j1 = n1 / 2 + 1
		j2 = n1 / 2
		even = (mod(n1,2)==0 && (medtype==MEDAVG || n1>2))
		do i = 1, npts {
		    k = i - 1
		    if (even) {
			val1 = Memr[d[j1]+k]
			val2 = Memr[d[j2]+k]
			median[i] = (val1 + val2) / 2.
		    } else
			median[i] = Memr[d[j1]+k]
		}
		return
	    } else {
	        # Check for negative n values.  If found then there are
		# pixels with no good values but with values we want to
		# use as a substitute median.  In this case ignore that
		# the good pixels have been sorted.
		do i = 1, npts {
		    if (n[i] < 0)
		        break
		}

		if (n[i] >= 0) {
		    do i = 1, npts {
			k = i - 1
			n1 = n[i]
			if (n1 > 0) {
			    j1 = n1 / 2 + 1
			    if (mod(n1,2)==0 && (medtype==MEDAVG || n1>2)) {
				j2 = n1 / 2
				val1 = Memr[d[j1]+k]
				val2 = Memr[d[j2]+k]
				median[i] = (val1 + val2) / 2.
			    } else
				median[i] = Memr[d[j1]+k]
			} else if (doblank == YES)
			    median[i] = blank
		    }
		    return
		}
	    }
	}

	# Compute the median.
	do i = 1, npts {
	    k = i - 1
	    n1 = abs(n[i])

	    # If there are more than 3 points use Wirth algorithm.  This
	    # is the same as vops$amed.gx except for an even number of
	    # points it selects the middle two and averages.
	    if (n1 > 3) {
		lo = 1
		up = n1
		j  = max (lo, min (up, (up+1)/2))

		while (lo < up) {
		    if (! (lo < up))
			break

		    temp = Memr[d[j]+k];  lo1 = lo;  up1 = up

		    repeat {
			while (Memr[d[lo1]+k] < temp)
			    lo1 = lo1 + 1
			while (temp < Memr[d[up1]+k])
			    up1 = up1 - 1
			if (lo1 <= up1) {
			    wtemp = Memr[d[lo1]+k]
			    Memr[d[lo1]+k] = Memr[d[up1]+k]
			    Memr[d[up1]+k] = wtemp
			    lo1 = lo1 + 1;  up1 = up1 - 1
			}
		    } until (lo1 > up1)

		    if (up1 < j)
			lo = lo1
		    if (j < lo1)
			up = up1
		}

		median[i] = Memr[d[j]+k]

		if (mod(n1,2)==0 && (medtype==MEDAVG || n1 > 2)) {
		    lo = 1
		    up = n1
		    j  = max (lo, min (up, (up+1)/2)+1)

		    while (lo < up) {
			if (! (lo < up))
			    break

			temp = Memr[d[j]+k];  lo1 = lo;  up1 = up

			repeat {
			    while (Memr[d[lo1]+k] < temp)
				lo1 = lo1 + 1
			    while (temp < Memr[d[up1]+k])
				up1 = up1 - 1
			    if (lo1 <= up1) {
				wtemp = Memr[d[lo1]+k]
				Memr[d[lo1]+k] = Memr[d[up1]+k]
				Memr[d[up1]+k] = wtemp
				lo1 = lo1 + 1;  up1 = up1 - 1
			    }
			} until (lo1 > up1)

			if (up1 < j)
			    lo = lo1
			if (j < lo1)
			    up = up1
		    }
		    median[i] = (median[i] + Memr[d[j]+k]) / 2
		}

	    # If 3 points find the median directly.
	    } else if (n1 == 3) {
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

	    # If 2 points average.
	    } else if (n1 == 2) {
		val1 = Memr[d[1]+k]
		val2 = Memr[d[2]+k]
		if (medtype == MEDAVG)
		    median[i] = (val1 + val2) / 2
		else
		    median[i] = min (val1, val2)

	    # If 1 point return the value.
	    } else if (n1 == 1)
		median[i] = Memr[d[1]+k]

	    # If no points return with a possibly blank value.
	    else if (doblank == YES)
		median[i] = blank
	}
end

# IC_MEDIAN -- Median of lines

procedure ic_mediand (d, n, npts, doblank, median)

pointer	d[ARB]			# Input data line pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line
int	doblank			# Set blank values?
double	median[npts]		# Median

int	i, j, k, j1, j2, n1, lo, up, lo1, up1
bool	even
double	val1, val2, val3
double	temp, wtemp

include	"../icombine.com"

begin
	# If no data return after possibly setting blank values.
	if (dflag == D_NONE) {
	    if (doblank == YES) {
		do i = 1, npts
		    median[i]= blank
	    }
	    return
	}

	# If the data were previously sorted then directly compute the median.
	if (mclip) {
	    if (dflag == D_ALL) {
		n1 = n[1]
		j1 = n1 / 2 + 1
		j2 = n1 / 2
		even = (mod(n1,2)==0 && (medtype==MEDAVG || n1>2))
		do i = 1, npts {
		    k = i - 1
		    if (even) {
			val1 = Memd[d[j1]+k]
			val2 = Memd[d[j2]+k]
			median[i] = (val1 + val2) / 2.
		    } else
			median[i] = Memd[d[j1]+k]
		}
		return
	    } else {
	        # Check for negative n values.  If found then there are
		# pixels with no good values but with values we want to
		# use as a substitute median.  In this case ignore that
		# the good pixels have been sorted.
		do i = 1, npts {
		    if (n[i] < 0)
		        break
		}

		if (n[i] >= 0) {
		    do i = 1, npts {
			k = i - 1
			n1 = n[i]
			if (n1 > 0) {
			    j1 = n1 / 2 + 1
			    if (mod(n1,2)==0 && (medtype==MEDAVG || n1>2)) {
				j2 = n1 / 2
				val1 = Memd[d[j1]+k]
				val2 = Memd[d[j2]+k]
				median[i] = (val1 + val2) / 2.
			    } else
				median[i] = Memd[d[j1]+k]
			} else if (doblank == YES)
			    median[i] = blank
		    }
		    return
		}
	    }
	}

	# Compute the median.
	do i = 1, npts {
	    k = i - 1
	    n1 = abs(n[i])

	    # If there are more than 3 points use Wirth algorithm.  This
	    # is the same as vops$amed.gx except for an even number of
	    # points it selects the middle two and averages.
	    if (n1 > 3) {
		lo = 1
		up = n1
		j  = max (lo, min (up, (up+1)/2))

		while (lo < up) {
		    if (! (lo < up))
			break

		    temp = Memd[d[j]+k];  lo1 = lo;  up1 = up

		    repeat {
			while (Memd[d[lo1]+k] < temp)
			    lo1 = lo1 + 1
			while (temp < Memd[d[up1]+k])
			    up1 = up1 - 1
			if (lo1 <= up1) {
			    wtemp = Memd[d[lo1]+k]
			    Memd[d[lo1]+k] = Memd[d[up1]+k]
			    Memd[d[up1]+k] = wtemp
			    lo1 = lo1 + 1;  up1 = up1 - 1
			}
		    } until (lo1 > up1)

		    if (up1 < j)
			lo = lo1
		    if (j < lo1)
			up = up1
		}

		median[i] = Memd[d[j]+k]

		if (mod(n1,2)==0 && (medtype==MEDAVG || n1 > 2)) {
		    lo = 1
		    up = n1
		    j  = max (lo, min (up, (up+1)/2)+1)

		    while (lo < up) {
			if (! (lo < up))
			    break

			temp = Memd[d[j]+k];  lo1 = lo;  up1 = up

			repeat {
			    while (Memd[d[lo1]+k] < temp)
				lo1 = lo1 + 1
			    while (temp < Memd[d[up1]+k])
				up1 = up1 - 1
			    if (lo1 <= up1) {
				wtemp = Memd[d[lo1]+k]
				Memd[d[lo1]+k] = Memd[d[up1]+k]
				Memd[d[up1]+k] = wtemp
				lo1 = lo1 + 1;  up1 = up1 - 1
			    }
			} until (lo1 > up1)

			if (up1 < j)
			    lo = lo1
			if (j < lo1)
			    up = up1
		    }
		    median[i] = (median[i] + Memd[d[j]+k]) / 2
		}

	    # If 3 points find the median directly.
	    } else if (n1 == 3) {
	        val1 = Memd[d[1]+k]
	        val2 = Memd[d[2]+k]
	        val3 = Memd[d[3]+k]
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

	    # If 2 points average.
	    } else if (n1 == 2) {
		val1 = Memd[d[1]+k]
		val2 = Memd[d[2]+k]
		if (medtype == MEDAVG)
		    median[i] = (val1 + val2) / 2
		else
		    median[i] = min (val1, val2)

	    # If 1 point return the value.
	    } else if (n1 == 1)
		median[i] = Memd[d[1]+k]

	    # If no points return with a possibly blank value.
	    else if (doblank == YES)
		median[i] = blank
	}
end

