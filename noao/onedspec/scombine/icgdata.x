include	<smw.h>
include "icombine.h"


# IC_GDATAR - Apply threshold, scaling, and masking

procedure ic_gdatar (sh, d, id, n, m, lflag, scales, zeros, nimages, npts)

pointer	sh[nimages]		# Input spectra structures
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Empty mask flags
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
int	nimages			# Number of spectra
int	npts			# NUmber of output points

int	i, j, k, l, nused
real	a, b
pointer	dp, ip, mp

include	"icombine.com"
	
begin
	# Set data vectors
	do i = 1, nimages {
	    d[i] = SY(sh[i])
	    m[i] = SX(sh[i])
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		dp = d[i]
		if (lflag[i] == D_ALL) {
		    do j = 1, npts {
			a = Memr[dp]
			if (a < lthresh || a > hthresh) {
			    Memr[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
		    }
		} else if (lflag[i] == D_MIX) {
		    mp = m[i]
		    do j = 1, npts {
			if (Memr[mp] == 0) {
			    a = Memr[dp]
			    if (a < lthresh || a > hthresh) {
				Memr[m[i]+j-1] = 1
				dflag = D_MIX
			    }
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}

		# Check for completely empty lines
		if (lflag[i] == D_MIX) {
		    lflag[i] = D_NONE
		    mp = m[i]
		    do j = 1, npts {
			if (Memr[mp] == 0) {
			    lflag[i] = D_MIX
			    break
			}
			mp = mp + 1
		    }
		}
	    }
	}

	# Apply scaling (avoiding masked pixels which might overflow?)
	if (doscale) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    do j = 1, npts {
			Memr[dp] = Memr[dp] / a + b
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			do j = 1, npts {
			    Memr[dp] = Memr[dp] / a + b
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			mp = m[i]
			do j = 1, npts {
			    if (Memr[mp] == 0)
				Memr[dp] = Memr[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
		    }
		}
	    }
	}

	# Sort pointers to exclude unused images.
	# Use the lflag array to keep track of the image index.

	if (dflag == D_ALL)
	    nused = nimages
	else {
	    nused = 0
	    do i = 1, nimages
		if (lflag[i] != D_NONE) {
		    nused = nused + 1
		    d[nused] = d[i]
		    m[nused] = m[i]
		    lflag[nused] = i
		}
	    if (nused == 0)
		dflag = D_NONE
	}

	# Compact data to remove bad pixels
	# Keep track of the image indices if needed
	# If growing mark the end of the included image indices with zero

	if (dflag == D_ALL) {
	    call amovki (nused, n, npts)
	    if (keepids)
		do i = 1, nimages
		    call amovki (i, Memi[id[i]], npts)
	} else if (dflag == D_NONE)
	    call aclri (n, npts)
	else {
	    call aclri (n, npts)
	    if (keepids) {
		do i = 1, nused {
		    l = lflag[i]
		    dp = d[i]
		    ip = id[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memr[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i) {
				Memr[d[k]+j-1] = Memr[dp]
				Memi[id[k]+j-1] = l
			    } else
				Memi[ip] = l
			}
			dp = dp + 1
			ip = ip + 1
			mp = mp + 1
		    }
		}
		if (grow > 0) {
		    do j = 0, npts-1 {
			do i = n[i]+1, nimages
			    Memi[id[i]+j] = 0
		    }
		}
	    } else {
		do i = 1, nused {
		    dp = d[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memr[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i)
				Memr[d[k]+j-1] = Memr[dp]
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}
	    }
	}

	# Sort the pixels and IDs if needed
	if (mclip) {
	    call malloc (dp, nimages, TY_REAL)
	    if (keepids) {
		call malloc (ip, nimages, TY_INT)
		call ic_2sortr (d, Memr[dp], id, Memi[ip], n, npts)
		call mfree (ip, TY_INT)
	    } else
		call ic_sortr (d, Memr[dp], n, npts)
	    call mfree (dp, TY_REAL)
	}
end
