# FMEDIAN common block 


int	xwindow		# requested  x median filtering window
int	ywindow		# requested y median filtering window
int	xbox		# actual y median filtering window
int	ybox		# actual y median filtering window
int	hmin		# histogram minimum
int	hmax		# histogram maximum
real	zmin		# data minimum
real	zmax		# data maximum
real	z1		# requested data minimum
real	z2		# requested data maximum
int	unmap		# rescale the digitized values
int	map		# map image to histogram
int	median		# current median
int	nmedian		# number less than the median
int	nltmedian	# number less than the current median

common /fmedian/ xwindow, ywindow, xbox, ybox, hmin, hmax, zmin, zmax,
	z1, z2, unmap, map, median, nmedian, nltmedian
