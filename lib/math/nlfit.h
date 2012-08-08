# Definitions for the non-linear least-squares fitting package NLFIT

# define parameters for nlstat[ird]

define	NLNPARAMS	1
define	NLNFPARAMS	2
define	NLITMAX		3
define	NLITER		4
define	NLSUMSQ		5
define	NLOLDSQ		6
define	NLLAMBDA	7
define	NLTOL		8
define	NLNPTS		9
define	NLSCATTER	10

# define the weighting flags

define	WTS_USER	1	# User enters weights
define	WTS_UNIFORM	2	# Equal weights
define	WTS_CHISQ 	3	# Chi-squared weights (input data in photons)
define	WTS_SCATTER	4	# Weights include an adjustable scatter term

# define the error conditions

define	DONE			0	# Solution converged
define	SINGULAR		1	# Singular matrix
define	NO_DEG_FREEDOM		2	# Too few points
define	NOT_DONE		3	# Solution did not converge
