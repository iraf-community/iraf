# definitions for the non-linear fitting package

# define parameters for nlstat

define	NLNPARAMS	1
define	NLNFPARAMS	2
define	NLITMAX		3
define	NLITER		4
define	NLSUMSQ		5
define	NLOLDSQ		6
define	NLLAMBDA	7
define	NLTOL		8

# define the weighting flags

define	WTS_USER	1	# user enters weights
define	WTS_UNIFORM	2	# equal weights
define	WTS_SPACING	3	# weight proportional to spacing of data points
define	WTS_CHISQ	4	# Chi-squared weights (input data in photons)

# error conditions

define	DONE			0	# Solution converged
define	SINGULAR		1	# Singular matrix
define	NO_DEG_FREEDOM		2	# Too few points
define	NOT_DONE		3	# Solution did not converge
