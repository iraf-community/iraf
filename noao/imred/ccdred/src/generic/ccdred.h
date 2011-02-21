# CCDRED Data Structures and Definitions

# The CCD structure:  This structure is used to communicate processing
# parameters between the package procedures.  It contains pointers to
# data, calibration image IMIO pointers, scaling parameters, and the
# correction flags.  The corrections flags indicate which processing
# operations are to be performed.  The subsection parameters do not
# include a step size.  A step size is assumed.  If arbitrary subsampling
# is desired this would be the next generalization.

define	LEN_CCD		131		# Length of CCD structure

# CCD data coordinates
define	CCD_C1		Memi[$1]	# CCD starting column
define	CCD_C2		Memi[$1+1]	# CCD ending column
define	CCD_L1		Memi[$1+2]	# CCD starting line
define	CCD_L2		Memi[$1+3]	# CCD ending line

# Input data
define	IN_IM		Memi[$1+10]	# Input image pointer
define	IN_C1		Memi[$1+11]	# Input data starting column
define	IN_C2		Memi[$1+12]	# Input data ending column
define	IN_L1		Memi[$1+13]	# Input data starting line
define	IN_L2		Memi[$1+14]	# Input data ending line

# Output data
define	OUT_IM		Memi[$1+20]	# Output image pointer
define	OUT_C1		Memi[$1+21]	# Output data starting column
define	OUT_C2		Memi[$1+22]	# Output data ending column
define	OUT_L1		Memi[$1+23]	# Output data starting line
define	OUT_L2		Memi[$1+24]	# Output data ending line

# Mask data
define  MASK_IM         Memi[$1+30]     # Mask image pointer
define  MASK_C1         Memi[$1+31]     # Mask data starting column
define  MASK_C2         Memi[$1+32]     # Mask data ending column
define  MASK_L1         Memi[$1+33]     # Mask data starting line
define  MASK_L2         Memi[$1+34]     # Mask data ending line
define  MASK_PM         Memi[$1+35]     # Mask pointer
define  MASK_FP         Memi[$1+36]     # Mask fixpix data

# Zero level data
define	ZERO_IM		Memi[$1+40]	# Zero level image pointer
define	ZERO_C1		Memi[$1+41]	# Zero level data starting column
define	ZERO_C2		Memi[$1+42]	# Zero level data ending column
define	ZERO_L1		Memi[$1+43]	# Zero level data starting line
define	ZERO_L2		Memi[$1+44]	# Zero level data ending line

# Dark count data
define	DARK_IM		Memi[$1+50]	# Dark count image pointer
define	DARK_C1		Memi[$1+51]	# Dark count data starting column
define	DARK_C2		Memi[$1+52]	# Dark count data ending column
define	DARK_L1		Memi[$1+53]	# Dark count data starting line
define	DARK_L2		Memi[$1+54]	# Dark count data ending line

# Flat field data
define	FLAT_IM		Memi[$1+60]	# Flat field image pointer
define	FLAT_C1		Memi[$1+61]	# Flat field data starting column
define	FLAT_C2		Memi[$1+62]	# Flat field data ending column
define	FLAT_L1		Memi[$1+63]	# Flat field data starting line
define	FLAT_L2		Memi[$1+64]	# Flat field data ending line

# Illumination data
define	ILLUM_IM	Memi[$1+70]	# Illumination image pointer
define	ILLUM_C1	Memi[$1+71]	# Illumination data starting column
define	ILLUM_C2	Memi[$1+72]	# Illumination data ending column
define	ILLUM_L1	Memi[$1+73]	# Illumination data starting line
define	ILLUM_L2	Memi[$1+74]	# Illumination data ending line

# Fringe data
define	FRINGE_IM	Memi[$1+80]	# Fringe image pointer
define	FRINGE_C1	Memi[$1+81]	# Fringe data starting column
define	FRINGE_C2	Memi[$1+82]	# Fringe data ending column
define	FRINGE_L1	Memi[$1+83]	# Fringe data starting line
define	FRINGE_L2	Memi[$1+84]	# Fringe data ending line

# Trim section
define	TRIM_C1		Memi[$1+90]	# Trim starting column
define	TRIM_C2		Memi[$1+91]	# Trim ending column
define	TRIM_L1		Memi[$1+92]	# Trim starting line
define	TRIM_L2		Memi[$1+93]	# Trim ending line

# Bias section
define	BIAS_C1		Memi[$1+100]	# Bias starting column
define	BIAS_C2		Memi[$1+101]	# Bias ending column
define	BIAS_L1		Memi[$1+102]	# Bias starting line
define	BIAS_L2		Memi[$1+103]	# Bias ending line

define	READAXIS	Memi[$1+110]	# Read out axis (1=cols, 2=lines)
define	CALCTYPE	Memi[$1+111]	# Calculation data type
define	OVERSCAN_TYPE	Memi[$1+112]	# Overscan type
define	OVERSCAN_VEC	Memi[$1+113]	# Pointer to overscan vector
define	DARKSCALE	Memr[P2R($1+114)]	# Dark count scale factor
define	FRINGESCALE	Memr[P2R($1+115)]	# Fringe scale factor
define	FLATSCALE	Memr[P2R($1+116)]	# Flat field scale factor
define	ILLUMSCALE	Memr[P2R($1+117)]	# Illumination scale factor
define	MINREPLACE	Memr[P2R($1+118)]	# Minimum replacement value
define	MEAN		Memr[P2R($1+119)]	# Mean of output image
define	COR		Memi[$1+120]	# Overall correction flag
define	CORS		Memi[$1+121+($2-1)]  # Individual correction flags

# The correction array contains the following elements with array indices
# given by the macro definitions.

define	NCORS		10		# Number of corrections

define	FIXPIX		1		# Fix bad pixels
define	TRIM		2		# Trim image
define	OVERSCAN	3		# Apply overscan correction
define	ZEROCOR		4		# Apply zero level correction
define	DARKCOR		5		# Apply dark count correction
define	FLATCOR		6		# Apply flat field correction
define	ILLUMCOR	7		# Apply illumination correction
define	FRINGECOR	8		# Apply fringe correction
define	FINDMEAN	9		# Find the mean of the output image
define	MINREP		10		# Check and replace minimum value

# The following definitions identify the correction values in the correction
# array.  They are defined in terms of bit fields so that it is possible to
# add corrections to form unique combination corrections.  Some of
# these combinations are implemented as compound operations for efficiency.

define	O	001B	# overscan
define	Z	002B	# zero level
define	D	004B	# dark count
define	F	010B	# flat field
define	I	020B	# Illumination
define	Q	040B	# Fringe

# The following correction combinations are recognized.

define	ZO	003B	# zero level + overscan
define	DO	005B	# dark count + overscan
define	DZ	006B	# dark count + zero level
define	DZO	007B	# dark count + zero level + overscan
define	FO	011B	# flat field + overscan
define	FZ	012B	# flat field + zero level
define	FZO	013B	# flat field + zero level + overscan
define	FD	014B	# flat field + dark count
define	FDO	015B	# flat field + dark count + overscan
define	FDZ	016B	# flat field + dark count + zero level
define	FDZO	017B	# flat field + dark count + zero level + overscan
define	QI	060B	# fringe + illumination

# The following overscan functions are recognized.
define  OVERSCAN_TYPES "|mean|median|minmax|chebyshev|legendre|spline3|spline1|"
define  OVERSCAN_MEAN   1               # Mean of overscan
define  OVERSCAN_MEDIAN 2               # Median of overscan
define  OVERSCAN_MINMAX 3               # Minmax of overscan
define  OVERSCAN_FIT    4               # Following codes are function fits
