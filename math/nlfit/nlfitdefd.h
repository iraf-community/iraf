# The NLFIT data structure.

# Structure length
define	LEN_NLSTRUCT	35


# Structure definition
define  NL_TOL          Memd[P2D($1+0)]   # Tolerance for convergence
define  NL_LAMBDA       Memd[P2D($1+2)]   # Damping factor
define	NL_OLDSQ	Memd[P2D($1+4)]   # Sum resid. squared last iter.
define	NL_SUMSQ	Memd[P2D($1+6)]   # Sum of residuals squared 
define	NL_REFSQ	Memd[P2D($1+8)]   # Reference sum of squares
define	NL_SCATTER	Memd[P2D($1+10)]  # Additional scatter

define	NL_FUNC		Memi[$1+12]	# Fitting function
define	NL_DFUNC	Memi[$1+13]	# Derivative function
define	NL_NPARAMS	Memi[$1+14]	# Number of parameters
define	NL_NFPARAMS	Memi[$1+15]	# Number of fitted parameters
define	NL_ITMAX	Memi[$1+16]	# Max number of iterations
define	NL_ITER		Memi[$1+17]	# Iteration counter
define	NL_NPTS		Memi[$1+18]	# Number of points in fit

define	NL_PARAM	Memi[$1+19]	# Pointer to parameter vector
define	NL_OPARAM	Memi[$1+20]	# Pointer to orignal parameter vector
define	NL_DPARAM	Memi[$1+21]	# Pointer to parameter change vector
define	NL_DELPARAM	Memi[$1+22]	# Pointer to delta param vector
define	NL_PLIST	Memi[$1+23]	# Pointer to parameter list
define	NL_ALPHA	Memi[$1+24]	# Pointer to alpha matrix
define	NL_COVAR	Memi[$1+25]	# Pointer to covariance matrix
define	NL_BETA		Memi[$1+26]	# Pointer to beta matrix
define	NL_TRY		Memi[$1+27]	# Pointer to trial vector
define	NL_DERIV	Memi[$1+28]	# Pointer to derivatives
define	NL_CHOFAC	Memi[$1+29]	# Pointer to Cholesky factorization

# next free location	($1 + 30)

# Access to buffers
define	PLIST		Memi[$1]	# Parameter list
define	OPARAM		Memd[$1]	# Original parameter vector
define	PARAM		Memd[$1]	# Parameter vector
define	DPARAM		Memd[$1]	# Parameter change vector
define	ALPHA		Memd[$1]	# Alpha matrix
define	BETA		Memd[$1]	# Beta matrix
define	TRY		Memd[$1]	# Trial vector
define	DERIV		Memd[$1]	# Derivatives
define	CHOFAC		Memd[$1]	# Cholesky factorization
define	COVAR		Memd[$1]	# Covariance matrix


# Defined constants alter for tricky problems
define	LAMBDAMAX	1000.0
define	MINITER		3
