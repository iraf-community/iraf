define	MAX_FILTER	4225	# maximum filter length

# MEDIAN common block

int	xwindow			# Requested  x median filtering window
int	ywindow			# Requested y median filtering window
int	xbox			# Actual y median filtering window
int	ybox			# Actual y median filtering window
int	npts			# Number of points in median filter
int	nhalf			# (npts - 1) / 2
int	nptsp1			# (npts + 1)
int	mp			# Median counter
int	start			# Index of first list element
int	finish			# Index of last list element
real	filter[MAX_FILTER+1]	# Filter kernel array
int	left[MAX_FILTER]	# Array of back pointers
int	right[MAX_FILTER]	# Array of forward pointers


common	/medparams/ xwindow, ywindow, xbox, ybox, start, finish, npts,
		    mp, nhalf, nptsp1
common  /medfilter/ filter, left, right
