# SCOMBINE Common

int	combine			# Combine algorithm
int	reject			# Rejection algorithm
real	blank			# Blank value
pointer	rdnoise			# CCD read noise
pointer	gain			# CCD gain
real	lthresh			# Low threshold
real	hthresh			# High threshold
real	lsigma			# Low sigma cutoff
real	hsigma			# High sigma cutoff
real	pclip			# Number or fraction of pixels from median
real	flow			# Fraction of low pixels to reject
real	fhigh			# Fraction of high pixels to reject
int	grow			# Grow radius
bool	mclip			# Use median in sigma clipping?
real	sigscale		# Sigma scaling tolerance
int	logfd			# Log file descriptor

# These flags allow special conditions to be optimized.

int	dflag			# Data flag (D_ALL, D_NONE, D_MIX)
bool	doscale			# Do the images have to be scaled?
bool	doscale1		# Do the sigma calculations have to be scaled?
bool	dothresh		# Check pixels outside specified thresholds?
bool	dowts			# Does the final average have to be weighted?
bool	keepids			# Keep track of the image indices?
bool	docombine		# Call the combine procedure?
bool	sort			# Sort data?

common	/scbcom/ combine, reject, blank, rdnoise, gain, lsigma, hsigma,
		 lthresh, hthresh, pclip, flow, fhigh, grow, logfd,
		 dflag, sigscale, mclip, doscale, doscale1,
		 dothresh, dowts, keepids, docombine, sort
