# ICOMBINE Common

int	combine			# Combine algorithm
int	medtype			# Median type
int	reject			# Rejection algorithm
bool	project			# Combine across the highest dimension?
real	blank			# Blank value
pointer	expkeyword		# Exposure time keyword
pointer	statsec			# Statistics section
pointer	rdnoise			# CCD read noise
pointer	gain			# CCD gain
pointer	snoise			# CCD sensitivity noise
real	lthresh			# Low threshold
real	hthresh			# High threshold
int	nkeep			# Minimum to keep
real	lsigma			# Low sigma cutoff
real	hsigma			# High sigma cutoff
real	pclip			# Number or fraction of pixels from median
real	flow			# Fraction of low pixels to reject
real	fhigh			# Fraction of high pixels to reject
real	grow			# Grow radius
bool	mclip			# Use median in sigma clipping?
real	sigscale		# Sigma scaling tolerance
int	logfd			# Log file descriptor

# These flags allow special conditions to be optimized.

int	dflag			# Data flag (D_ALL, D_NONE, D_MIX)
bool	aligned			# Are the images aligned?
bool	doscale			# Do the images have to be scaled?
bool	doscale1		# Do the sigma calculations have to be scaled?
bool	dothresh		# Check pixels outside specified thresholds?
bool	dowts			# Does the final average have to be weighted?
bool	keepids			# Keep track of the image indices?
bool	docombine		# Call the combine procedure?
bool	sort			# Sort data?
bool	verbose			# Verbose?

pointer	icm			# Mask data structure

common	/imccom/ combine, medtype, reject, blank, expkeyword, statsec, rdnoise,
		 gain, snoise, lsigma, hsigma, lthresh, hthresh, nkeep,
		 pclip, flow, fhigh, grow, logfd, dflag, sigscale, project,
		 mclip, aligned, doscale, doscale1, dothresh, dowts,
		 keepids, docombine, sort, verbose, icm
