# Header file for non-linear curve fitting package

define	LEN_NLSTRUCT	50

define	NL_FUNC		Memi[$1]	# Fitting function
define	NL_DFUNC	Memi[$1+1]	# Derivative function
define	NL_NPARAMS	Memi[$1+2]	# Number of parameters
define	NL_NFPARAMS	Memi[$1+3]	# Number of fitted parameters
define	NL_ITMAX	Memi[$1+4]	# Max number of iterations
define	NL_TOL		Memr[$1+5]	# Tolerance for convergence
define	NL_LAMBDA	Memr[$1+6]	# Damping factor
define	NL_OLDSQ	Memr[$1+7]	# Sum residuals squared last iteration
define	NL_SUMSQ	Memr[$1+8]	# Sum of residuals squared 
define	NL_ITER		Memi[$1+9]	# Iteration counter
define	NL_NPTS		Memi[$1+10]	# Number of points in fit

define	NL_PARAM	Memi[$1+11]	# Pointer to parameter vector
define	NL_DPARAM	Memi[$1+12]	# Pointer to parameter change vector
define	NL_PLIST	Memi[$1+13]	# Parameter list
define	NL_ALPHA	Memi[$1+14]	# Alpha matrix
define	NL_COVAR	Memi[$1+15]	# Covariance matrix
define	NL_BETA		Memi[$1+16]	# Beta matrix
define	NL_TRY		Memi[$1+17]	# Trial vector
define	NL_DERIV	Memi[$1+18]	# Derivatives
define	NL_CHOFAC	Memi[$1+19]	# Cholesky factorization
define	NL_REFSQ	Memr[$1+20]	# Reference sum of squares

define	PARAM		Memr[$1]	# Parameter vector
define	DPARAM		Memr[$1]	# Parameter change vector
define	PLIST		Memi[$1]	# Parameter list
define	ALPHA		Memr[$1]	# Alpha matrix
define	BETA		Memr[$1]	# Beta matrix
define	TRY		Memr[$1]	# Trial vector
define	DERIV		Memr[$1]	# Derivatives
define	CHOFAC		Memr[$1]	# Cholesky factorization
define	COVAR		Memr[$1]	# Covariance matrix

# Defined constants alter for tricky problems

define	LAMBDAMAX	1000.0
