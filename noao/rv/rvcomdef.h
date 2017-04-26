# RVCOMDEF.H  - Include file for colon command definitions for each task. 

# RVXCOR Colon Commands
define	RVX_KEYWORDS	"|apertures|apnum|apodize|autowrite|autodraw|background\
			 |ccftype|comment|continuum|correction|deltav|disp\
			 |filter|function|height|imupdate|line_color\
			 |maxwidth|minwidth|n!|next!|objects|output|osample\
			 |p!|peak|pixcorr|previous!|printz|rebin|results\
			 |rsample|show|templates|tempvel|text_color|tnum\
			 |unlearn|update|version|verbose|wccf|weights|width\
			 |wincenter|window|ymin|ymax|"

define	RVX_APERTURES		1	# List of apertures to process
define	RVX_APNUM		2	# Specific aperture to process
define	RVX_APODIZE		3	# Fraction of endpoints to apodize
define	RVX_AUTOWRITE		4	# Autowrite results?
define	RVX_AUTODRAW		5	# Autodraw results?
define	RVX_BACKGROUND		6	# Background fitting level
define	RVX_CCFTYPE		7	# Type of CCF output
define	RVX_COMMENT		8	# Add a comment to the output logs
define	RVX_CONTINUUM		9	# Which spectra to normalize
define	RVX_CORRECTION		10	# Convert a pixel shift to a velocity
define	RVX_DELTAV		11	# Print out the velocity dispersion
define	RVX_DISP		12	# Print rebinned dispersion info
define	RVX_FILTER		13	# Which spectra to filter
define	RVX_FUNCTION		14	# CCF peak fitting function
define	RVX_HEIGHT		15	# CCF peak fit height
define	RVX_IMUPDATE		16	# Update image with results?
define	RVX_LINECOLOR		17	# Set/Show overlay vector color
define	RVX_MAXWIDTH		18	# Min fitting width
define	RVX_MINWIDTH		19	# Max fitting width
define	RVX_NBANG		20	# Explicit next command
define	RVX_NEXT		21	# Explicit next command
define	RVX_OBJECTS		22	# Reset object list
define	RVX_OUTPUT		23	# Rename output logfile
define	RVX_OSAMPLE		24	# Regions to correlate
define	RVX_PBANG		25	# Explicit previous command
define	RVX_PEAK		26	# Peak height flag
define	RVX_PIXCORR		27	# Pixel-correlation only flag
define	RVX_PREVIOUS		28	# Explicit previous command
define	RVX_PRINTZ		29	# Toggle output of Z values
define	RVX_REBIN		30	# Set/Show rebin param
define	RVX_RESULTS		31	# Page a logfile of results
define	RVX_RSAMPLE		32	# Regions to correlate
define	RVX_SHOW		33	# Show current parameter settings
define	RVX_TEMPLATES		34	# Reset template list
define	RVX_TEMPVEL		35	# Reset template list
define	RVX_TEXTCOLOR		36	# Set/Show greaphics text color
define	RVX_TNUM		37	# Skip to specifi template number
define	RVX_UNLEARN		38	# Unlearn task parameters
define	RVX_UPDATE		39	# Update task parameters
define	RVX_VERSION		40	# Update task parameters
define	RVX_VERBOSE		41	# Verbose output flag
define	RVX_WCCF		42	# Write CCF to text|image
define	RVX_WEIGHTS		43	# Fitting weights
define	RVX_WIDTH		44	# Fitting width about peak
define	RVX_WINCENTER		45	# Peak window center
define	RVX_WINDOW		46	# Size of window
define	RVX_YMIN		47	# Bottom of ccf plot
define	RVX_YMAX		48	# Top of ccf plot


################################################################################
##									      ##
##       The following define statements are for common colon commands.  The  ##
##  psets shall all be available from each task that uses them, thus ensuring ##
##  that filter parameters, keyword translation, and continuum parameters     ##
##  can all be changed interactively if needed.				      ##
##									      ##
################################################################################

# Continuum Subtraction Parameter Commands
define	CONT_KEYWORDS	"|c_interactive|c_sample|naverage|c_function|cn_order\
			 |replace|low_reject|high_reject|niterate|grow|"

# Continuum normalization parameters
define	CNT_INTERACTIVE		1	# Do it interactively?
define	CNT_SAMPLE		2	# Sample string to use
define	CNT_NAVERAGE		3	# Npts to average in sample
define	CNT_FUNCTION		4	# Fitting function
define	CNT_CN_ORDER		5	# Order of function
define	CNT_REPLACE		6	# Replace spectrum with fit ?
define	CNT_LOW_REJECT		7	# Low rejection in sigma of fit
define	CNT_HIGH_REJECT		8	# High rejection in sigma of fit
define	CNT_NITERATE		9	# Number of rejection iterations
define	CNT_GROW		10	# Rejection growing radius

# Keywords translation parameters
define  KEY_KEYWORDS  	"|ra|dec|ut|utmiddle|exptime|epoch|date_obs\
			 |hjd|mjd_obs|vobs|vrel|vhelio|vlsr|vsun|"

define	KEY_RA			1	# Right ascension keyword
define	KEY_DEC			2	# Declination keyword
define	KEY_UT			3	# Universal time of observation keyword
define	KEY_UTMID		4	# Universal time of observation keyword
define	KEY_EXPTIME		5	# Frame exposure time keyword
define	KEY_EPOCH		6	# Epoch of observation keyword
define	KEY_DATE_OBS		7	# Date of observation keyword
define	KEY_HJD			8	# Heliocentric Julian Date Keyword
define	KEY_MJD_OBS		9	# Modified Julian Data Keyword
define	KEY_VOBS		10	# Observed RV keyword
define	KEY_VREL		11	# Relative RV keyword
define	KEY_VHELIO		12	# Heliocentric RV keyword
define	KEY_VLSR		13	# LSR RV keyword
define	KEY_VSUN		14	# Solar motion keyword

# Filter parameters
define  FILT_KEYWORDS	"|f_type|cuton|cutoff|fullon|fulloff|"

define  FILT_FILT_TYPE		1	# Function type of filter
define  FILT_CUTON		2	# Cuton frequency component
define  FILT_CUTOFF		3	# Cutoff frequency component
define  FILT_FULLON		4	# Fullon frequency component
define  FILT_FULLOFF		5	# Fulloff frequency component

# FFT Plotting Parameters
define	PLOT_KEYWORDS	"|filter|plot|overlay|split_plot|one_image|when\
			 |wpc|log_scale|zoom|"

define	PLT_FILTER		1	# Set/Show the filter flag
define	PLT_PLOT		2	# What type of plot to draw
define	PLT_OVERLAY		3	# Overlay filter function?
define	PLT_SPLIT_PLOT		4	# Make a split-plot?
define	PLT_ONE_IMAGE		5 	# What to put in single screen
define	PLT_WHEN		6	# Plot before or after filtering?
define	PLT_WPC			7	# Print rebinned WPC
define	PLT_LOG_SCALE		8	# Plot on a Log scale?
define	PLT_FFT_ZOOM		9	# FFT zoom parameter

# Debugging commands
define	DEBUG_KEYWORDS	"|debug|d_on|d_off|dbg_file|dbg_level|dbg_quick\
			 |dbg_other|"

define	DEBUG_DEBUG		1	# Debug toggle flag
define	DEBUG_D_ON		2	# Debug toggle
define	DEBUG_D_OFF		3	# Debug toggle
define	DEBUG_FILE		4	# File name for output
define	DEBUG_LEVEL		5	# Level of debugging information
define	DEBUG_QUICK		6	# Quickdraw flag toggle
define  DEBUG_OTHER		7	# Compare algorithms?
