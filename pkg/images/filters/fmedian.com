# FMEDIAN common block 


int	xwindow		# Requested  x median filtering window
int	ywindow		# Requested y median filtering window
int	xbox		# Actual y median filtering window
int	ybox		# Actual y median filtering window
int	hmin		# Histogram minimum
int	hmax		# Histogram maximum
real	zmin		# Data minimum
real	zmax		# Data maximum
real	z1		# Requested data minimum
real	z2		# Requested data maximum
int	unmap		# rescale the digitized values
int	map		# Map image to histogram
int	median		# Current median
int	nmedian		# Number less than the median
int	nltmedian	# Number less than the current median

common /fmedian/ xwindow, ywindow, xbox, ybox, hmin, hmax, zmin, zmax,
	z1, z2, unmap, map, median, nmedian, nltmedian
