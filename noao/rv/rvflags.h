# Flag definition file for the Radial Velocity Package

# Velocity Constants (for Heliocentric corrections, relative to LSR)
define	VSD			20.0d0		# Solar velocity (Km/sec)
define	RASD			18.0d0		# Solar RA (Hours)
define  DECSD			30.0d0		# Solar DEC (Degrees)
define 	EPSD			1900.0d0	# Epoch of above (years)

# Generic debug flag
define	DEBUG			(DBG_DEBUG($1)==YES||RV_APODIZE($1)==0.116)

# Misc. constants
define	SPEED_OF_LIGHT	    299792.5d0	    	# in Km/sec
define	CLN10		    690297.74149142d0   # in Km/sec
define	C		    SPEED_OF_LIGHT      # short-hand form

# Fitting function flags
define	PARABOLA		1		# Fit a parabola
define	GAUSSIAN		2		# Fit a gaussian (w/ background)
define	LORENTZIAN		3		# Fit a Lorentzian profile
define	CENTER1D		4		# Fit with center1d()
define	DEBLEND			5		# Fit with deblending code
define	SINC			6		# Fit with a sin(x)/x code
define	RV_CFTYPES	"|parabola|gaussian|lorentzian|center1d|deblend|sinc|"

# Which spectra to process
define	OBJ_ONLY		1		# Do only object
define	TEMP_ONLY		2		# Do only template
define	BOTH			3		# Do both spectra
define	NONE			4		# Do neither
define	RV_SPTODO	"|object|template|both|none|"

# Data rebinning flags
define	RB_OBJ			1		# Rebin to object dispersion
define	RB_TEMP			2		# Rebin to template dispersion
define	RB_SMALL		3		# Rebin to smaller dispersion
define	RB_BIG			4		# Rebin to larger dispersion
define	RB_WHICH	"|object|template|smallest|largest|"

# Output CCF types
define	OUTPUT_IMAGE		1		# Write CCF as an image
define	OUTPUT_TEXT		2		# Write CCF as text file
define	LAG			3		# Lag x-axis
define	VELOCITY		4		# Velocity x-axis
define	CCF_TYPES	"|image|text|lag|velocity|"

# Output file flags
define	OF_SHORT	        1		# Write a short .txt file
define	OF_LONG		        2		# Write a long .txt file
define	OF_NOLOG	        3		# Don't write a .log file
define	OF_NOGKI	        4		# Don't write a .gki file
define	OF_TXTONLY	        5		# Write only a .txt file
define	OF_STXTONLY	        6		# Write a short .txt file
define	RV_OFTYPES	"|short|long|nolog|nogki|txtonly|stxtonly|"

# Data rebinning functions.
define  IN_NEAREST      	1       	# Nearest neighbour
define  IN_LINEAR       	2       	# Linear
define  IN_POLY3        	3       	# 3rd order polynomial
define  IN_POLY5        	4       	# 5th order polynomial
define  IN_SPLINE3      	5       	# Cubic spline
define  IN_SINC         	6       	# Sinc
define  IN_FUNCTIONS    "|nearest|linear|poly3|poly5|spline3|sinc|"

# Define color constants
define  C_BACKGROUND	        0
define  C_FOREGROUND	        1
define  C_RED	                2
define  C_GREEN	        	3
define  C_BLUE	                4
define  C_CYAN	                5
define  C_YELLOW	        6
define  C_MAGENTA	        7
define  C_PUPLE	                8
define  C_DARKSLATEGREY	        9
define	C_COLOR_NAMES	"|background|foreground|red|green|blue|cyan|yellow \
			 |magenta|purple|slategrey|"

# Miscellaneous flags
define	ALL_SPECTRUM		0		# No samples selected
define	MAXIMUM			1		# Find max point
define	MINIMUM			2		# Find min point
define	LEFT			3		# Find left side
define	RIGHT			4		# Find right side
define	OBJECT_SPECTRUM		5		# Which type of data
define	REFER_SPECTRUM		6		# Which type of data
define	QUIT			7		# Task flag
define	MOVE			8		# Move flag

# Data unit flags
define	PIXELS			10		# No dispersion info
define	LAMBDA			11		# Lambda dispersion
define	LOGLAMBDA		12		# Log-Lambda dispersion
define	NONLINEAR		13		# Non-linear dispersion

# Data format flags
define	ONEDSPEC		15		# Onedspec format image
define	TWODSPEC		16		# Twodspec (logslit?) images
define	ECHELLE			17		# Echelle format image
define	MULTISPEC		18		# Multispec format image
define	LONGSLIT		19		# Longslit format image

# Plot flags
define	SPECTRUM_PLOT		20		# Overplot both spectra
define	CONVOLUTION_PLOT	21		# Plot convolved spectra
define	CORRELATION_PLOT	22		# Plot the CCF
define	VCORRELATION_PLOT	23		# Plot the CCF w/ velocity axes
define	ACORRELATION_PLOT	24		# Plot the CCF w/ angstrom axes
define	ANTISYM_PLOT		25		# Plot Antisymmetric noise 
define  SPLIT_PLOT		26		# Plot a split screen plot
define  SINGLE_PLOT		27		# Plot a single screen splot
define  FOURIER_PLOT		28		# Plot a Fourier transform
define  PS_PLOT			29		# Plot a power spectrum
define  NORM_PLOT		30		# Plot a spectrum normalization
define	FILTER_PLOT		31		# Plot of filtered spectrum
define	RESIDUAL_PLOT		32		# Plot residuals of the fit
define	SUMMARY_PLOT		33		# Plot the summary
define	OBJ_PLOT		34		# Plot object spectrum
define	TEMP_PLOT		35		# Plot template spectrum
define	PREPARED_PLOT		36		# Plot FFT prepared spectrum
define	BINARY_PLOT		37		# Binary star summary plot
define	ZOOM			38		# Plot flag
define	FILTER			39		# Plot the actual filter
define  TOP			40		# Split screen plot flag
define	MIDDLE			41		# Split screen plot flag
define  BOTTOM			42		# Split screen plot flag

# Command mode flags
define	CCF_MODE		1		# Correlation mode
define	FFT_MODE		2		# FFT mode	
define	SPEC_MODE		3		# Spectrum mode
define	CONT_MODE		4		# Continuum mode

# Error Codes
define  ERR_SIDE		 -1	# Trouble finding a side to line
define  ERR_NOPEAK		 -2	# Couldn't find a peak in CCF
define	ERR_OBPEAK		 -3	# Peak found out of bounds
define  ERR_DOUBLE		 -4 	# Possible double star	
define  ERR_RVCOR	         -5	# Error in RV correction
define	ERR_FIT		         -6	# Error in fitting function
define  ERR_CORREL	         -7	# Error in correlation
define	ERR_READ	         -8	# Error in reading data
define	ERR_KEYW	         -9	# Error getting image header keyword
define	ERR_GENERIC	        -10	# Generic error - need a code?
define	ERR_PARAM	        -11     # generic parameter error
define	ERR_REAL	    -INDEFR	# Generic real valued error code

# Help files
define	XC_HELP	"noao$lib/scr/fxcor.key"	# Help key - FXCOR
define	FM_HELP	"noao$lib/scr/fftmode.key"	# Help key - FFT Mode
define	SM_HELP	"noao$lib/scr/specmode.key"	# Help key - Spectrum Mode
