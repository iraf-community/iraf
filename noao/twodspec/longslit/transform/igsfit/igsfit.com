# Common parameters.

char	function[SZ_LINE]	# Surface function
int	xorder			# X order of surface function
int	yorder			# Y order of surface function
real	xmin, xmax		# X range
real	ymin, ymax		# Y range
real	mean, rms		# Mean and RMS of fit

common	/igscom/ xmin, xmax, ymin, ymax, xorder, yorder, function, mean, rms
