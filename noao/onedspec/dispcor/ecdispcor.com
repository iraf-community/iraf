double	order		# Order to evaluate
double	wt1, wt2	# Function weights
double	shift1, shift2	# Function shifts
pointer	ecf1, ecf2	# Function pointers
int	offset		# Aperture number to order offset
int	slope		# Aperture number to order slope
pointer	crval		# Pointer to linear coordinate values
pointer	cdelt		# Pointer to linear intervals
int	dcflag		# Dispersion correction flag
pointer	stp		# Dispersion solution symbol table

common	/ecdisp/ order, wt1, wt2, shift1, shift2, ecf1, ecf2, offset, slope,
		crval, cdelt, dcflag, stp
