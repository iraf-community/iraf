# constants for cv package...should come from a graphcap entry

# These are one based.
define	CV_XCEN	     257
define	CV_YCEN	     256

define	CV_XRES	     512
define	CV_YRES	     512
define	CV_ZRES	     256

define	CV_MAXF	       4
define	CV_MAXG	       7

define	CV_GRCHNUM    16

# CVLEN is just the *estimated* never to be exceeded amount of storage needed
# to set up the escape sequence.  It could be determined dynamically by
# changing cv_move to count elements instead of moving them.  Then the known
# counts would be used with amovs to hustle the elements into the "salloc'ed"
# space.  Instead, with a static count, we can salloc once upon entering
# the cv program and free up at exit.

define	CVLEN	128

# Following are from "display.h"... only SAMPLE_SIZE and MAXLOG needed
# as of May, 1985.  But we might incorporate other programs from "tv",
# so leave them.

# Size limiting parameters.

define	MAXCHAN		2
define	SAMPLE_SIZE	600

# If a logarithmic greyscale transformation is desired, the input range Z1:Z2
# will be mapped into the range 1.0 to 10.0 ** MAXLOG before taking the log
# to the base 10.

define	MAXLOG		3

# The following parameter is used to compare display pixel coordinates for
# equality.  It determines the maximum permissible magnification.  The machine
# epsilon is not used because the computations are nontrivial and accumulation
# of error is a problem.

define	DS_TOL		(1E-4)

# These parameters are needed for user defined transfer functions.

define	SZ_BUF		4096
define	STARTPT		0.0E0
define	ENDPT		4095.0E0
