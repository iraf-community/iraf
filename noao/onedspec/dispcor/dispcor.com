# Common dispersion function parameters.

double	wt1		# First reference spectrum weight
double	shift1		# First reference spectrum shift
pointer	cv1		# First reference spectrum dispersion curve
double	wt2		# Second reference spectrum weight
double	shift2		# Second reference spectrum shift
pointer	cv2		# Second reference spectrum dispersion curve
double	crval		# Linear coordinate reference value
double	cdelt		# Linear coordinate interval
int	dcflag		# Linear dispersion flag

common	/dispcom/ wt1, wt2, shift1, shift2, crval, cdelt, cv1, cv2, dcflag
