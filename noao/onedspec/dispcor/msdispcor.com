# Common dispersion function parameters.
 
int	line			# Line to evaluate
double	wt1, wt2		# Function weights
pointer	shift1, shift2		# Function shifts
pointer	cv1, cv2		# Function pointers
pointer	crval			# Linear coordinate reference value
pointer	cdelt			# Linear coordinate interval
int	dcflag			# Linear dispersion flag
pointer	stp			# Dispersion solution symbol table
 
common	/msdisp/ wt1, wt2, line, dcflag, shift1, shift2, cv1, cv2, crval,
		cdelt, stp

