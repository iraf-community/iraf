# Common fitting result parameters.

real	mresid				# Mean residual of fit
real	sresid				# Sigma of residuals
real	ccfvar				# Variance of CCF
real	chisqr				# Chis squared of fit
int	nfit				# Number of points fit
int	nfitpars			# Number of fitted parameters
int	niter				# Number of iterations
int	binshift			# Bin of peak shift in CCF

common	/fitcom/ mresid, sresid, ccfvar, chisqr, nfit, nfitpars, niter, binshift
