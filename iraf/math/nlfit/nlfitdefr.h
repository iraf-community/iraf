# The NLFIT data structure.


# Structure length
define	LEN_NLSTRUCT	35


# Structure definition
define	NL_TOL		Memr[P2R($1+0)]	# Tolerance for convergence
define	NL_LAMBDA	Memr[P2R($1+2)]	# Damping factor
define	NL_OLDSQ	Memr[P2R($1+4)]	# Sum resid. squared last iter.
define	NL_SUMSQ	Memr[P2R($1+6)]	# Sum of residuals squared 
define	NL_REFSQ	Memr[P2R($1+8)]	# Reference sum of squares
define	NL_SCATTER	Memr[P2R($1+10)]	# Additional scatter

define	NL_FUNC		Memp[$1+12]	# Fitting function
define	NL_DFUNC	Memp[$1+13]	# Derivative function
define	NL_NPARAMS	Memz[P2Z($1+14)]	# Number of parameters
define	NL_NFPARAMS	Memz[P2Z($1+15)]	# Number of fitted parameters
define	NL_ITMAX	Memi[P2I($1+16)]	# Max number of iterations
define	NL_ITER		Memi[P2I($1+17)]	# Iteration counter
define	NL_NPTS		Memz[P2Z($1+18)]	# Number of points in fit

define	NL_PARAM	Memp[$1+19]	# Pointer to parameter vector
define	NL_OPARAM	Memp[$1+20]	# Pointer to orignal parameter vector
define	NL_DPARAM	Memp[$1+21]	# Pointer to parameter change vector
define	NL_DELPARAM	Memp[$1+22]	# Pointer to delta param vector
define	NL_PLIST	Memp[$1+23]	# Pointer to parameter list
define	NL_ALPHA	Memp[$1+24]	# Pointer to alpha matrix
define	NL_COVAR	Memp[$1+25]	# Pointer to covariance matrix
define	NL_BETA		Memp[$1+26]	# Pointer to beta matrix
define	NL_TRY		Memp[$1+27]	# Pointer to trial vector
define	NL_DERIV	Memp[$1+28]	# Pointer to derivatives
define	NL_CHOFAC	Memp[$1+29]	# Pointer to Cholesky factorization

# next free location	($1 + 30)

# Access to buffers
define	PARAM		Memr[$1]	# Parameter vector
define	DPARAM		Memr[$1]	# Parameter change vector
define	OPARAM		Memr[$1]	# Original parameter vector
define	PLIST		Meml[$1]	# Parameter list
define	ALPHA		Memr[$1]	# Alpha matrix
define	BETA		Memr[$1]	# Beta matrix
define	TRY		Memr[$1]	# Trial vector
define	DERIV		Memr[$1]	# Derivatives
define	CHOFAC		Memr[$1]	# Cholesky factorization
define	COVAR		Memr[$1]	# Covariance matrix


# Defined constants alter for tricky problems
define	LAMBDAMAX	1000.0
define	MINITER		3
