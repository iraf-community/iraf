# CENTER header file

define	LEN_CENSTRUCT		(36 + SZ_FNAME + 1)

# centering parameters

define	AP_CENTERFUNCTION   Memi[$1]		# Centering algorithm
define	AP_CAPERT	    Memr[P2R($1+1)]	# Centering box half-width
define	AP_CTHRESHOLD	    Memr[P2R($1+2)]  	# Threshold above cdatalimit
define	AP_MAXSHIFT	    Memr[P2R($1+3)]	# Maximum center shift
define	AP_MINSNRATIO	    Memr[P2R($1+4)]	# Minimum s/n ratio 
define	AP_CLEAN	    Memi[$1+5]		# Clean before centering
define	AP_RCLEAN	    Memr[P2R($1+6)]	# Cleaning radius
define  AP_RCLIP	    Memr[P2R($1+7)]	# Clipping radius
define	AP_SIGMACLEAN	    Memr[P2R($1+8)]	# Sky sigma for cleaning
define	AP_CMAXITER	    Memi[$1+9]		# Maximum number of iterations

# centering buffers

define	AP_CTRPIX	    Memi[$1+10]		# Pointer to pixels
define	AP_XCTRPIX	    Memi[$1+11]		# Pointer to x coords (not used)
define	AP_YCTRPIX	    Memi[$1+12]		# Pointer to y coords (not used)
define	AP_NCTRPIX	    Memi[$1+13]		# Number of pixels (not used)
define	AP_LENCTRBUF	    Memi[$1+14]		# Centering buffer sz (not used)
define	AP_CXCUR	    Memr[P2R($1+15)]	# Centering buffer x center
define	AP_CYCUR	    Memr[P2R($1+16)]	# Centering buffer y center
define	AP_CXC		    Memr[P2R($1+17)]	# X center of subraster
define	AP_CYC		    Memr[P2R($1+18)]	# Y center of subraster
define	AP_CNX		    Memi[$1+19]		# Y dimension of subraster
define	AP_CNY		    Memi[$1+20]		# Y dimension of subraster

# center fitting output

define	AP_OXINIT	    Memr[P2R($1+21)]	# initial output x center
define	AP_OYINIT	    Memr[P2R($1+22)]	# initial output y center
define	AP_XCENTER	    Memr[P2R($1+23)]	# computed x center
define	AP_YCENTER	    Memr[P2R($1+24)]	# computed y center
define	AP_OXCENTER	    Memr[P2R($1+25)]	# computed output x center
define	AP_OYCENTER	    Memr[P2R($1+26)]	# computed output y center
define	AP_XSHIFT	    Memr[P2R($1+27)]	# total x shift
define	AP_YSHIFT	    Memr[P2R($1+28)]	# total y shift
define	AP_OXSHIFT	    Memr[P2R($1+29)]	# total output x shift
define	AP_OYSHIFT	    Memr[P2R($1+30)]	# total output y shift
define	AP_XERR		    Memr[P2R($1+31)]	# x error
define	AP_YERR		    Memr[P2R($1+32)]	# y error
define	AP_CDATALIMIT	    Memr[P2R($1+33)]	# min (max) of subraster
define	AP_CSTRING	    Memc[P2C($1+34)]# centering algorithm id

# default setup values for centering parameters

define	DEF_CENTERFUNCTION	AP_CENTROID1D
define	DEF_CAPERT		2.5
define	DEF_CTHRESHOLD		0.0
define	DEF_MINSNRATIO		1.0
define	DEF_CMAXITER		10
define	DEF_MAXSHIFT		1.0
define	DEF_CLEAN		NO
define	DEF_RCLEAN		1.0
define	DEF_RCLIP		2.0
define	DEF_CLEANSIGMA		3.0
