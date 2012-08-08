# NSTAR Structure

define	LEN_NSTARSTRUCT (20)

define	DP_NGNUM	Memi[$1]	# current group number 
define	DP_NNUM		Memi[$1+1]	# number of stars in current group
define	DP_NSTARCOLS	Memi[$1+2]	# pointer to NSTAR table columns
define	DP_NNPIX	Memi[$1+3]	# pointer to number of pixels
define	DP_NNUMER	Memi[$1+4]	# pointer to NUMER in curr group
define	DP_NDENOM	Memi[$1+5]	# pointer to DENOM stat in curr group
define	DP_NSKIP	Memi[$1+6]	# pointer to SKIP in curr group
define	DP_NXCLAMP	Memi[$1+7]	# pointer to CLAMP stat in curr group
define	DP_NXOLD	Memi[$1+8]	# pointer to XOLD stat in curr group
define  DP_NX		Memi[$1+9]	# pointer to X array in curr group
define  DP_NV		Memi[$1+10]	# pointer to V array in curr group
define  DP_NSUMWT	Memi[$1+11]	# pointer to SUMWT array in curr group
define  DP_NC	        Memi[$1+12]	# pointer to C array in curr group
define	DP_NRPIXSQ	Memi[$1+13]	# pointer to RPIXSQ array in curr group
define	DP_NIER		Memi[$1+14]	# pointer to NSTAR error codes

# Definitions controlling the input / output format

define	NST_NOUTCOL	12

# NSTAR fitting constants

define	CUT_FACTOR	   0.999998    # the pixel cutoff radius in fitrad ** 2
define	FRACTION_MINSEP    0.14	       # min sep in fwhmpsf for merging
define	NITER_MAX	   15          # iteration limit for difficult stars
define	NITER_MED	   10          # iteration limit for moderate stars
define	NITER_MIN	   5	       # iteration limit for easy stars
define	WCRIT_MAX	   0.5	       # max N/S for difficult stars
define	WCRIT_MED	   0.66667     # max N/S for moderate stars 
define	WCRIT_MIN	   1.0         # max N/S for easy stars
define	NCORE_SIGMASQ	   36.0	       # max number of hwidths for sharpness
define 	MIN_NPIX	   4	       # min pixels per star for fit
define	CHI_NORM	   1.2533141   # sqrt (PI / 2.0)
define	MIN_SUMWT	   3.0	       # min value for radial weight sum
define	MAX_DELTA_FAINTER  0.84        # max permitted brightness decrease
define	MAX_DELTA_BRIGHTER 5.25        # max permitted brightness increase
define	MAX_NEW_ERRMAG	   0.1         # 1st convergence check on mag error
define	MAX_NEW_RELBRIGHT1 0.005       # 1st convergence check on magnitude
define	MAX_NEW_RELBRIGHT2 0.0005      # 2nd convergence check on magnitude
define	MAX_PIXERR1	   4.0e-4      # 1st convergence check on x/y positions
define	MAX_PIXERR2	   4.0e-6      # 2nd convergence check on x/y positions
define	MAX_DELTA_PIX	   0.4         # max +/- change in x/y positions
define	MAX_PIX_INCREMENT  0.001       # test for nearness to edge of image
define	MIN_REL_BRIGHT	   1.0e-5      # minimum relative brightness
define	MIN_FAINT	   0.5         # min N/S
define	MIN_ITER	   4           # min number of iterations
define  MIN_SHARP	   -99.9       # min sharpness value
define  MAX_SHARP	   99.9	       # max sharpness value

# List of NSTAR error codes

define	NSTERR_OK		0
define	NSTERR_BIGGROUP		1
define	NSTERR_INDEFSKY		2
define	NSTERR_NOPIX		3
define	NSTERR_SINGULAR		4
define	NSTERR_FAINT		5
define	NSTERR_MERGE		6
define	NSTERR_OFFIMAGE		7
