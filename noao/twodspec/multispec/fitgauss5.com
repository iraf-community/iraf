# Common for fitting model GAUSS5.

real	factor				# Convergence factor
int	spectra[3, MAX_RANGES]		# Spectra to fit
int	parameters[MS_NGAUSS5]		# Parameters to be fit
int	smooth[MS_NGAUSS5]		# Smooth parameters?
int	algorithm			# Fitting algorithm

common /g5_fitcom/ factor, spectra, parameters, smooth, algorithm
