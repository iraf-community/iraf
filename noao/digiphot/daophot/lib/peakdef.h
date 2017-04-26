# The PEAK task definitions files

# The fitting algorithm error codes

define	PKERR_OK		0
define	PKERR_INDEFSKY		1
define	PKERR_NOPIX		2
define	PKERR_SINGULAR		3
define	PKERR_FAINT		4
#define	PKERR_NOCONVERGE	5

# The PEAK fitting structure 

define	LEN_PKSTRUCT	10

define	DP_PKNTERM	Memi[$1]	# the number of terms to be fit
define	DP_PKCLAMP	Memi[$1+1]	# pointer to the clamp array
define	DP_PKNORMAL	Memi[$1+2]	# pointer to the norm array
define	DP_PKRESID	Memi[$1+3]	# pointer to the residuals vector
define	DP_PKDERIV	Memi[$1+4]	# pointer to the derivative array
define	DP_PKRESULT	Memi[$1+5]	# pointer to the results vector
define	DP_PKOLDRESULT	Memi[$1+6]	# pointer to the old results vector

# Definitions controlling input / output variables

define	DELTA_MAG	5.0	# mag difference assigned to input INDEF stars
define	PK_NOUTCOL	11	# the number of columns in the output table

# Various PEAK fitting constants

#define	SIGN_CHECK	     -1.2e-38  # check for change of sign
define	MAX_DELTA_BRIGHTER   5.25      # max permitted brightness increase
define	MAX_DELTA_FAINTER    0.84      # max permitted brightness decrease
define	MAX_DELTA_PIX	     0.4       # max +/- change in x/y positions
define	MAX_NEW_ERRMAG	     0.05      # convergenge check on magnitude error
define	MIN_REL_BRIGHT	     1.0e-5    # minimum relative brightness
define	MAX_NEW_RELBRIGHT1   0.0001    # convergence check on brightness
define	MAX_NEW_RELBRIGHT2   0.002     # convergence check on brightness
define	MAX_PIXERR1	     0.001     # convergence check on x/y positions
define	MAX_PIXERR2	     0.02      # convergence check on x/y positions
define	MAX_CLAMPFACTOR	     0.25      # maximum clamping factor
define	MIN_SHARP	     -99.99    # min sharpness value
define	MAX_SHARP	     99.99     # max sharpness value
define	PEAK_EPSILONR	     2.0e-6    # test for inclusion inside fitrad
define	NCORE_SIGMASQ	     36.0      # max gsigma-sq for sharpness value
define	CHI_NORM	     1.2533141 # sqrt (PI / 2.0)
define	MIN_SUMWT	     3.0       # min value of the radial weight sum
define	MIN_NPIX	     4	       # min number of pixels for fit

define	WCRIT_MAX	     0.5       # max noise / signal ratio	
define	WCRIT_MED	     0.6667    # median noise / signal ratio	
define	WCRIT_MIN	     1.0       # min noise / signal ratio	
define	WCRIT_NMAX	     15	       # max number of iterations	
define	WCRIT_NMED	     10	       # median number of iterations	
define	WCRIT_NMIN	      5	       # minimum number of iterations	
