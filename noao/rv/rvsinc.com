# Common values for the sinc interpolation.
pointer	sx				# Pointer to X array (real)
pointer	sy				# Pointer to Y array (real)
pointer	splx				# Pointer to X plot array (real)
pointer	sply				# Pointer to Y plot array (real)
int	snfit				# Number of points being fit

common	/sinccom/ sx, sy, splx, sply, snfit
