# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"


# IC_MM --  Reject a specified number of high and low pixels

procedure ic_mms (d, m, n, npts)

pointer	d[ARB]		# Data pointers
pointer	m[ARB]		# Image ID pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line

int	n1, ncombine, npairs, nlow, nhigh, np
int	i, i1, j, jmax, jmin
pointer	k, kmax, kmin
short	d1, d2, dmin, dmax

include	"../icombine.com"

begin
	if (dflag == D_NONE)
	    return

	if (dflag == D_ALL) {
	    n1 = max (0, n[1])
	    nlow = flow * n1 + 0.001
	    nhigh = fhigh * n1 + 0.001
	    ncombine = n1 - nlow -  nhigh
	    npairs = min (nlow, nhigh)
	    nlow = nlow - npairs
	    nhigh = nhigh - npairs
	}

	do i = 1, npts {
	    i1 = i - 1
	    n1 = max (0, n[i])
	    if (dflag == D_MIX) {
		nlow = flow * n1 + 0.001
		nhigh = fhigh * n1 + 0.001
		ncombine = max (ncombine, n1 - nlow - nhigh)
		npairs = min (nlow, nhigh)
		nlow = nlow - npairs
		nhigh = nhigh - npairs
	    }

	    # Reject the npairs low and high points.
	    do np = 1, npairs {
		k = d[1] + i1
		d1 = Mems[k]
		dmax = d1; dmin = d1; jmax = 1; jmin = 1; kmax = k; kmin = k
		do j = 2, n1 {
		    d2 = d1
		    k = d[j] + i1
		    d1 = Mems[k]
		    if (d1 > dmax) {
			dmax = d1; jmax = j; kmax = k
		    } else if (d1 < dmin) {
			dmin = d1; jmin = j; kmin = k
		    }
		}
		j = n1 - 1
		if (keepids) {
		    if (jmax < j) {
			if (jmin != j) {
			    Mems[kmax] = d2
			    k = Memi[m[jmax]+i1]
			    Memi[m[jmax]+i1] = Memi[m[j]+i1]
			    Memi[m[j]+i1] = k
			} else {
			    Mems[kmax] = d1
			    k = Memi[m[jmax]+i1]
			    Memi[m[jmax]+i1] = Memi[m[n1]+i1]
			    Memi[m[n1]+i1] = k
			}
		    }
		    if (jmin < j) {
			if (jmax != n1) {
			    Mems[kmin] = d1
			    k = Memi[m[jmin]+i1]
			    Memi[m[jmin]+i1] = Memi[m[n1]+i1]
			    Memi[m[n1]+i1] = k
			} else {
			    Mems[kmin] = d2
			    k = Memi[m[jmin]+i1]
			    Memi[m[jmin]+i1] = Memi[m[j]+i1]
			    Memi[m[j]+i1] = k
			}
		    }
		} else {
		    if (jmax < j) {
			if (jmin != j)
			    Mems[kmax] = d2
			else
			    Mems[kmax] = d1
		    }
		    if (jmin < j) {
			if (jmax != n1)
			    Mems[kmin] = d1
			else
			    Mems[kmin] = d2
		    }
		}
		n1 = n1 - 2
	    }

	    # Reject the excess low points.
	    do np = 1, nlow {
		k = d[1] + i1
		d1 = Mems[k]
		dmin = d1; jmin = 1; kmin = k
		do j = 2, n1 {
		    k = d[j] + i1
		    d1 = Mems[k]
		    if (d1 < dmin) {
			dmin = d1; jmin = j; kmin = k
		    }
		}
		if (keepids) {
		    if (jmin < n1) {
			Mems[kmin] = d1
			k = Memi[m[jmin]+i1]
			Memi[m[jmin]+i1] = Memi[m[n1]+i1]
			Memi[m[n1]+i1] = k
		    }
		} else {
		    if (jmin < n1)
			Mems[kmin] = d1
		}
		n1 = n1 - 1 
	    }

	    # Reject the excess high points.
	    do np = 1, nhigh {
		k = d[1] + i1
		d1 = Mems[k]
		dmax = d1; jmax = 1; kmax = k
		do j = 2, n1 {
		    k = d[j] + i1
		    d1 = Mems[k]
		    if (d1 > dmax) {
			dmax = d1; jmax = j; kmax = k
		    }
		}
		if (keepids) {
		    if (jmax < n1) {
			Mems[kmax] = d1
			k = Memi[m[jmax]+i1]
			Memi[m[jmax]+i1] = Memi[m[n1]+i1]
			Memi[m[n1]+i1] = k
		    }
		} else {
		    if (jmax < n1)
			Mems[kmax] = d1
		}
		n1 = n1 - 1 
	    }
	    n[i] = n1
	}

	if (dflag == D_ALL && npairs + nlow + nhigh > 0)
		dflag = D_MIX
end

# IC_MM --  Reject a specified number of high and low pixels

procedure ic_mmi (d, m, n, npts)

pointer	d[ARB]		# Data pointers
pointer	m[ARB]		# Image ID pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line

int	n1, ncombine, npairs, nlow, nhigh, np
int	i, i1, j, jmax, jmin
pointer	k, kmax, kmin
int	d1, d2, dmin, dmax

include	"../icombine.com"

begin
	if (dflag == D_NONE)
	    return

	if (dflag == D_ALL) {
	    n1 = max (0, n[1])
	    nlow = flow * n1 + 0.001
	    nhigh = fhigh * n1 + 0.001
	    ncombine = n1 - nlow -  nhigh
	    npairs = min (nlow, nhigh)
	    nlow = nlow - npairs
	    nhigh = nhigh - npairs
	}

	do i = 1, npts {
	    i1 = i - 1
	    n1 = max (0, n[i])
	    if (dflag == D_MIX) {
		nlow = flow * n1 + 0.001
		nhigh = fhigh * n1 + 0.001
		ncombine = max (ncombine, n1 - nlow - nhigh)
		npairs = min (nlow, nhigh)
		nlow = nlow - npairs
		nhigh = nhigh - npairs
	    }

	    # Reject the npairs low and high points.
	    do np = 1, npairs {
		k = d[1] + i1
		d1 = Memi[k]
		dmax = d1; dmin = d1; jmax = 1; jmin = 1; kmax = k; kmin = k
		do j = 2, n1 {
		    d2 = d1
		    k = d[j] + i1
		    d1 = Memi[k]
		    if (d1 > dmax) {
			dmax = d1; jmax = j; kmax = k
		    } else if (d1 < dmin) {
			dmin = d1; jmin = j; kmin = k
		    }
		}
		j = n1 - 1
		if (keepids) {
		    if (jmax < j) {
			if (jmin != j) {
			    Memi[kmax] = d2
			    k = Memi[m[jmax]+i1]
			    Memi[m[jmax]+i1] = Memi[m[j]+i1]
			    Memi[m[j]+i1] = k
			} else {
			    Memi[kmax] = d1
			    k = Memi[m[jmax]+i1]
			    Memi[m[jmax]+i1] = Memi[m[n1]+i1]
			    Memi[m[n1]+i1] = k
			}
		    }
		    if (jmin < j) {
			if (jmax != n1) {
			    Memi[kmin] = d1
			    k = Memi[m[jmin]+i1]
			    Memi[m[jmin]+i1] = Memi[m[n1]+i1]
			    Memi[m[n1]+i1] = k
			} else {
			    Memi[kmin] = d2
			    k = Memi[m[jmin]+i1]
			    Memi[m[jmin]+i1] = Memi[m[j]+i1]
			    Memi[m[j]+i1] = k
			}
		    }
		} else {
		    if (jmax < j) {
			if (jmin != j)
			    Memi[kmax] = d2
			else
			    Memi[kmax] = d1
		    }
		    if (jmin < j) {
			if (jmax != n1)
			    Memi[kmin] = d1
			else
			    Memi[kmin] = d2
		    }
		}
		n1 = n1 - 2
	    }

	    # Reject the excess low points.
	    do np = 1, nlow {
		k = d[1] + i1
		d1 = Memi[k]
		dmin = d1; jmin = 1; kmin = k
		do j = 2, n1 {
		    k = d[j] + i1
		    d1 = Memi[k]
		    if (d1 < dmin) {
			dmin = d1; jmin = j; kmin = k
		    }
		}
		if (keepids) {
		    if (jmin < n1) {
			Memi[kmin] = d1
			k = Memi[m[jmin]+i1]
			Memi[m[jmin]+i1] = Memi[m[n1]+i1]
			Memi[m[n1]+i1] = k
		    }
		} else {
		    if (jmin < n1)
			Memi[kmin] = d1
		}
		n1 = n1 - 1 
	    }

	    # Reject the excess high points.
	    do np = 1, nhigh {
		k = d[1] + i1
		d1 = Memi[k]
		dmax = d1; jmax = 1; kmax = k
		do j = 2, n1 {
		    k = d[j] + i1
		    d1 = Memi[k]
		    if (d1 > dmax) {
			dmax = d1; jmax = j; kmax = k
		    }
		}
		if (keepids) {
		    if (jmax < n1) {
			Memi[kmax] = d1
			k = Memi[m[jmax]+i1]
			Memi[m[jmax]+i1] = Memi[m[n1]+i1]
			Memi[m[n1]+i1] = k
		    }
		} else {
		    if (jmax < n1)
			Memi[kmax] = d1
		}
		n1 = n1 - 1 
	    }
	    n[i] = n1
	}

	if (dflag == D_ALL && npairs + nlow + nhigh > 0)
		dflag = D_MIX
end

# IC_MM --  Reject a specified number of high and low pixels

procedure ic_mmr (d, m, n, npts)

pointer	d[ARB]		# Data pointers
pointer	m[ARB]		# Image ID pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line

int	n1, ncombine, npairs, nlow, nhigh, np
int	i, i1, j, jmax, jmin
pointer	k, kmax, kmin
real	d1, d2, dmin, dmax

include	"../icombine.com"

begin
	if (dflag == D_NONE)
	    return

	if (dflag == D_ALL) {
	    n1 = max (0, n[1])
	    nlow = flow * n1 + 0.001
	    nhigh = fhigh * n1 + 0.001
	    ncombine = n1 - nlow -  nhigh
	    npairs = min (nlow, nhigh)
	    nlow = nlow - npairs
	    nhigh = nhigh - npairs
	}

	do i = 1, npts {
	    i1 = i - 1
	    n1 = max (0, n[i])
	    if (dflag == D_MIX) {
		nlow = flow * n1 + 0.001
		nhigh = fhigh * n1 + 0.001
		ncombine = max (ncombine, n1 - nlow - nhigh)
		npairs = min (nlow, nhigh)
		nlow = nlow - npairs
		nhigh = nhigh - npairs
	    }

	    # Reject the npairs low and high points.
	    do np = 1, npairs {
		k = d[1] + i1
		d1 = Memr[k]
		dmax = d1; dmin = d1; jmax = 1; jmin = 1; kmax = k; kmin = k
		do j = 2, n1 {
		    d2 = d1
		    k = d[j] + i1
		    d1 = Memr[k]
		    if (d1 > dmax) {
			dmax = d1; jmax = j; kmax = k
		    } else if (d1 < dmin) {
			dmin = d1; jmin = j; kmin = k
		    }
		}
		j = n1 - 1
		if (keepids) {
		    if (jmax < j) {
			if (jmin != j) {
			    Memr[kmax] = d2
			    k = Memi[m[jmax]+i1]
			    Memi[m[jmax]+i1] = Memi[m[j]+i1]
			    Memi[m[j]+i1] = k
			} else {
			    Memr[kmax] = d1
			    k = Memi[m[jmax]+i1]
			    Memi[m[jmax]+i1] = Memi[m[n1]+i1]
			    Memi[m[n1]+i1] = k
			}
		    }
		    if (jmin < j) {
			if (jmax != n1) {
			    Memr[kmin] = d1
			    k = Memi[m[jmin]+i1]
			    Memi[m[jmin]+i1] = Memi[m[n1]+i1]
			    Memi[m[n1]+i1] = k
			} else {
			    Memr[kmin] = d2
			    k = Memi[m[jmin]+i1]
			    Memi[m[jmin]+i1] = Memi[m[j]+i1]
			    Memi[m[j]+i1] = k
			}
		    }
		} else {
		    if (jmax < j) {
			if (jmin != j)
			    Memr[kmax] = d2
			else
			    Memr[kmax] = d1
		    }
		    if (jmin < j) {
			if (jmax != n1)
			    Memr[kmin] = d1
			else
			    Memr[kmin] = d2
		    }
		}
		n1 = n1 - 2
	    }

	    # Reject the excess low points.
	    do np = 1, nlow {
		k = d[1] + i1
		d1 = Memr[k]
		dmin = d1; jmin = 1; kmin = k
		do j = 2, n1 {
		    k = d[j] + i1
		    d1 = Memr[k]
		    if (d1 < dmin) {
			dmin = d1; jmin = j; kmin = k
		    }
		}
		if (keepids) {
		    if (jmin < n1) {
			Memr[kmin] = d1
			k = Memi[m[jmin]+i1]
			Memi[m[jmin]+i1] = Memi[m[n1]+i1]
			Memi[m[n1]+i1] = k
		    }
		} else {
		    if (jmin < n1)
			Memr[kmin] = d1
		}
		n1 = n1 - 1 
	    }

	    # Reject the excess high points.
	    do np = 1, nhigh {
		k = d[1] + i1
		d1 = Memr[k]
		dmax = d1; jmax = 1; kmax = k
		do j = 2, n1 {
		    k = d[j] + i1
		    d1 = Memr[k]
		    if (d1 > dmax) {
			dmax = d1; jmax = j; kmax = k
		    }
		}
		if (keepids) {
		    if (jmax < n1) {
			Memr[kmax] = d1
			k = Memi[m[jmax]+i1]
			Memi[m[jmax]+i1] = Memi[m[n1]+i1]
			Memi[m[n1]+i1] = k
		    }
		} else {
		    if (jmax < n1)
			Memr[kmax] = d1
		}
		n1 = n1 - 1 
	    }
	    n[i] = n1
	}

	if (dflag == D_ALL && npairs + nlow + nhigh > 0)
		dflag = D_MIX
end

# IC_MM --  Reject a specified number of high and low pixels

procedure ic_mmd (d, m, n, npts)

pointer	d[ARB]		# Data pointers
pointer	m[ARB]		# Image ID pointers
int	n[npts]			# Number of good pixels
int	npts			# Number of output points per line

int	n1, ncombine, npairs, nlow, nhigh, np
int	i, i1, j, jmax, jmin
pointer	k, kmax, kmin
double	d1, d2, dmin, dmax

include	"../icombine.com"

begin
	if (dflag == D_NONE)
	    return

	if (dflag == D_ALL) {
	    n1 = max (0, n[1])
	    nlow = flow * n1 + 0.001
	    nhigh = fhigh * n1 + 0.001
	    ncombine = n1 - nlow -  nhigh
	    npairs = min (nlow, nhigh)
	    nlow = nlow - npairs
	    nhigh = nhigh - npairs
	}

	do i = 1, npts {
	    i1 = i - 1
	    n1 = max (0, n[i])
	    if (dflag == D_MIX) {
		nlow = flow * n1 + 0.001
		nhigh = fhigh * n1 + 0.001
		ncombine = max (ncombine, n1 - nlow - nhigh)
		npairs = min (nlow, nhigh)
		nlow = nlow - npairs
		nhigh = nhigh - npairs
	    }

	    # Reject the npairs low and high points.
	    do np = 1, npairs {
		k = d[1] + i1
		d1 = Memd[k]
		dmax = d1; dmin = d1; jmax = 1; jmin = 1; kmax = k; kmin = k
		do j = 2, n1 {
		    d2 = d1
		    k = d[j] + i1
		    d1 = Memd[k]
		    if (d1 > dmax) {
			dmax = d1; jmax = j; kmax = k
		    } else if (d1 < dmin) {
			dmin = d1; jmin = j; kmin = k
		    }
		}
		j = n1 - 1
		if (keepids) {
		    if (jmax < j) {
			if (jmin != j) {
			    Memd[kmax] = d2
			    k = Memi[m[jmax]+i1]
			    Memi[m[jmax]+i1] = Memi[m[j]+i1]
			    Memi[m[j]+i1] = k
			} else {
			    Memd[kmax] = d1
			    k = Memi[m[jmax]+i1]
			    Memi[m[jmax]+i1] = Memi[m[n1]+i1]
			    Memi[m[n1]+i1] = k
			}
		    }
		    if (jmin < j) {
			if (jmax != n1) {
			    Memd[kmin] = d1
			    k = Memi[m[jmin]+i1]
			    Memi[m[jmin]+i1] = Memi[m[n1]+i1]
			    Memi[m[n1]+i1] = k
			} else {
			    Memd[kmin] = d2
			    k = Memi[m[jmin]+i1]
			    Memi[m[jmin]+i1] = Memi[m[j]+i1]
			    Memi[m[j]+i1] = k
			}
		    }
		} else {
		    if (jmax < j) {
			if (jmin != j)
			    Memd[kmax] = d2
			else
			    Memd[kmax] = d1
		    }
		    if (jmin < j) {
			if (jmax != n1)
			    Memd[kmin] = d1
			else
			    Memd[kmin] = d2
		    }
		}
		n1 = n1 - 2
	    }

	    # Reject the excess low points.
	    do np = 1, nlow {
		k = d[1] + i1
		d1 = Memd[k]
		dmin = d1; jmin = 1; kmin = k
		do j = 2, n1 {
		    k = d[j] + i1
		    d1 = Memd[k]
		    if (d1 < dmin) {
			dmin = d1; jmin = j; kmin = k
		    }
		}
		if (keepids) {
		    if (jmin < n1) {
			Memd[kmin] = d1
			k = Memi[m[jmin]+i1]
			Memi[m[jmin]+i1] = Memi[m[n1]+i1]
			Memi[m[n1]+i1] = k
		    }
		} else {
		    if (jmin < n1)
			Memd[kmin] = d1
		}
		n1 = n1 - 1 
	    }

	    # Reject the excess high points.
	    do np = 1, nhigh {
		k = d[1] + i1
		d1 = Memd[k]
		dmax = d1; jmax = 1; kmax = k
		do j = 2, n1 {
		    k = d[j] + i1
		    d1 = Memd[k]
		    if (d1 > dmax) {
			dmax = d1; jmax = j; kmax = k
		    }
		}
		if (keepids) {
		    if (jmax < n1) {
			Memd[kmax] = d1
			k = Memi[m[jmax]+i1]
			Memi[m[jmax]+i1] = Memi[m[n1]+i1]
			Memi[m[n1]+i1] = k
		    }
		} else {
		    if (jmax < n1)
			Memd[kmax] = d1
		}
		n1 = n1 - 1 
	    }
	    n[i] = n1
	}

	if (dflag == D_ALL && npairs + nlow + nhigh > 0)
		dflag = D_MIX
end

