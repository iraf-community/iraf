# Common parameters.

char	function[SZ_FNAME]	# Fitting function
char	ecfstr[SZ_LINE]		# Working char string
int	gstype			# Surface function type
int	xorder			# X order of surface function
int	yorder			# Y order of surface function
int	xtype			# X axis type
int	ytype			# Y axis type
int	slope			# Slope of order
int	offset			# Order offset of fit
double	xmin, xmax		# X range
double	ymin, ymax		# Y range
double	shift			# First order shift
double	rms			# RMS of fit


common	/ecfcom/ xmin, xmax, ymin, ymax, shift, rms, gstype, xorder, yorder,
	xtype, ytype, slope, offset, function, ecfstr
