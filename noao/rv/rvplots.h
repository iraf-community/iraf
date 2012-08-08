# Include file for the FFT Plot structure.  A pointer is allocated in
# the main RV structure into this one.  This sub-structure contains the
# parameters used for data filter while in Fourier space as well as
# the option flags for filter function types

define	SZ_PLOTSTRUCT	     10

define	RVP_PLOT	    Memi[RV_PLOTP($1)]	      # Plot type
define	RVP_OVERLAY	    Memi[RV_PLOTP($1)+1]      # Overlay filter?
define	RVP_SPLIT_PLOT	    Memi[RV_PLOTP($1)+2]      # Make a split-plot?
define	RVP_ONE_IMAGE	    Memi[RV_PLOTP($1)+3]      # What's in one plot?
define	RVP_WHEN	    Memi[RV_PLOTP($1)+4]      # Before/after filtering?
define	RVP_LOG_SCALE	    Memi[RV_PLOTP($1)+5]      # Log scale it?
define	RVP_FFT_ZOOM	    Memr[P2R(RV_PLOTP($1)+6)] # FFT Zoom factor

######################  END  OF  STRUCTURE  DEFINITIONS  ######################

#  Plot type flags
define	RV_PTYPES	"|amplitude|phase|power|"
define  AMPLITUDE_PLOT		1		# Plot type
define	PHASE_PLOT		2		# Plot type
define	POWER_PLOT		3		# Plot type

define	BEFORE			1		# Plot before filtering
define	AFTER			2		# Plot after filtering
