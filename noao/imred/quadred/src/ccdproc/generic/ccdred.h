# CCDRED Data Structures and Definitions

# The CCD structure:  This structure is used to communicate processing
# parameters between the package procedures.  It contains pointers to
# data, calibration image IMIO pointers, scaling parameters, and the
# correction flags.  The corrections flags indicate which processing
# operations are to be performed.  The subsection parameters do not
# include a step size.  A step size is assumed.  If arbitrary subsampling
# is desired this would be the next generalization.

define	LEN_CCD		75		# Length of CCD structure

# CCD data coordinates
define	CCD_C1		Memi[$1]	# CCD starting column
define	CCD_C2		Memi[$1+1]	# CCD ending column
define	CCD_L1		Memi[$1+2]	# CCD starting line
define	CCD_L2		Memi[$1+3]	# CCD ending line

# Input data
define	IN_IM		Memi[$1+4]	# Input image pointer
define	IN_C1		Memi[$1+5]	# Input data starting column
define	IN_C2		Memi[$1+6]	# Input data ending column
define	IN_L1		Memi[$1+7]	# Input data starting line
define	IN_L2		Memi[$1+8]	# Input data ending line
define	IN_NSEC		Memi[$1+71]	# Number of input pieces
define	IN_SEC		Memi[$1+72]	# Pointer to sections (c1,c2,l1,l2)xn

# Output data
define	OUT_IM		Memi[$1+9]	# Output image pointer
define	OUT_C1		Memi[$1+10]	# Output data starting column
define	OUT_C2		Memi[$1+11]	# Output data ending column
define	OUT_L1		Memi[$1+12]	# Output data starting line
define	OUT_L2		Memi[$1+13]	# Output data ending line
define	OUT_SEC		Memi[$1+73]	# Pointer to sections (c1,c2,l1,l2)xn

# Zero level data
define	ZERO_IM		Memi[$1+14]	# Zero level image pointer
define	ZERO_C1		Memi[$1+15]	# Zero level data starting column
define	ZERO_C2		Memi[$1+16]	# Zero level data ending column
define	ZERO_L1		Memi[$1+17]	# Zero level data starting line
define	ZERO_L2		Memi[$1+18]	# Zero level data ending line

# Dark count data
define	DARK_IM		Memi[$1+19]	# Dark count image pointer
define	DARK_C1		Memi[$1+20]	# Dark count data starting column
define	DARK_C2		Memi[$1+21]	# Dark count data ending column
define	DARK_L1		Memi[$1+22]	# Dark count data starting line
define	DARK_L2		Memi[$1+23]	# Dark count data ending line

# Flat field data
define	FLAT_IM		Memi[$1+24]	# Flat field image pointer
define	FLAT_C1		Memi[$1+25]	# Flat field data starting column
define	FLAT_C2		Memi[$1+26]	# Flat field data ending column
define	FLAT_L1		Memi[$1+27]	# Flat field data starting line
define	FLAT_L2		Memi[$1+28]	# Flat field data ending line

# Illumination data
define	ILLUM_IM	Memi[$1+29]	# Illumination image pointer
define	ILLUM_C1	Memi[$1+30]	# Illumination data starting column
define	ILLUM_C2	Memi[$1+31]	# Illumination data ending column
define	ILLUM_L1	Memi[$1+32]	# Illumination data starting line
define	ILLUM_L2	Memi[$1+33]	# Illumination data ending line

# Fringe data
define	FRINGE_IM	Memi[$1+34]	# Fringe image pointer
define	FRINGE_C1	Memi[$1+35]	# Fringe data starting column
define	FRINGE_C2	Memi[$1+36]	# Fringe data ending column
define	FRINGE_L1	Memi[$1+37]	# Fringe data starting line
define	FRINGE_L2	Memi[$1+38]	# Fringe data ending line

# Trim section
define	TRIM_C1		Memi[$1+39]	# Trim starting column
define	TRIM_C2		Memi[$1+40]	# Trim ending column
define	TRIM_L1		Memi[$1+41]	# Trim starting line
define	TRIM_L2		Memi[$1+42]	# Trim ending line

# Bias section
define	BIAS_C1		Memi[$1+43]	# Bias starting column
define	BIAS_C2		Memi[$1+44]	# Bias ending column
define	BIAS_L1		Memi[$1+45]	# Bias starting line
define	BIAS_L2		Memi[$1+46]	# Bias ending line
define	BIAS_SEC	Memi[$1+74]	# Multiple bias sections

define	READAXIS	Memi[$1+47]	# Read out axis (1=cols, 2=lines)
define	CALCTYPE	Memi[$1+48]	# Calculation data type
define	NBADCOLS	Memi[$1+49]	# Number of column interpolation regions
define	BADCOLS		Memi[$1+50]	# Pointer to col interpolation regions
define	NBADLINES	Memi[$1+51]	# Number of line interpolation regions
define	BADLINES	Memi[$1+52]	# Pointer to line interpolation regions
define	OVERSCAN_VEC	Memi[$1+53]	# Pointer to overscan vector
define	DARKSCALE	Memr[P2R($1+54)]	# Dark count scale factor
define	FRINGESCALE	Memr[P2R($1+55)]	# Fringe scale factor
define	FLATSCALE	Memr[P2R($1+56)]	# Flat field scale factor
define	ILLUMSCALE	Memr[P2R($1+57)]	# Illumination scale factor
define	MINREPLACE	Memr[P2R($1+58)]	# Minimum replacement value
define	MEAN		Memr[P2R($1+59)]	# Mean of output image
define	COR		Memi[$1+60]	# Overall correction flag
define	CORS		Memi[$1+61+($2-1)]  # Individual correction flags

# Individual components of input, output, and bias section pieces.
define	IN_SC1		Memi[IN_SEC($1)+4*$2-4]
define	IN_SC2		Memi[IN_SEC($1)+4*$2-3]
define	IN_SL1		Memi[IN_SEC($1)+4*$2-2]
define	IN_SL2		Memi[IN_SEC($1)+4*$2-1]
define	OUT_SC1		Memi[OUT_SEC($1)+4*$2-4]
define	OUT_SC2		Memi[OUT_SEC($1)+4*$2-3]
define	OUT_SL1		Memi[OUT_SEC($1)+4*$2-2]
define	OUT_SL2		Memi[OUT_SEC($1)+4*$2-1]
define	BIAS_SC1	Memi[BIAS_SEC($1)+4*$2-4]
define	BIAS_SC2	Memi[BIAS_SEC($1)+4*$2-3]
define	BIAS_SL1	Memi[BIAS_SEC($1)+4*$2-2]
define	BIAS_SL2	Memi[BIAS_SEC($1)+4*$2-1]

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
