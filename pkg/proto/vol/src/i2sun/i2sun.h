# I2SUNRAS.H -- Include file for IRAF to Sun rasterfile program i2sunras.

define	COL		1
define	LINE		2
define	BAND		3
define	Z_LINEAR	1		# linear ztransform
define	Z_LOG		2		# log ztransform
define	Z_UNITARY	3		# no ztransform
define	Z_USER		4		# user-specified transform
define	U_MAXPTS	4096		# max user-specified lut pairs (DISPLAY)
define	U_Z1		0		# base user-specified transfer val
define	U_Z2		4095		# upper user-specified transfer val
define	MAXLOG		3		# if log, map to 1:10**MAXLOG b4 log10
define	DSP_MIN		0		# minimum display pixel value
define	DSP_MAX		255		# maximum display pixel value
define	RAS_HDR_INTS	8		# SunOS4.0 and earlier
define	RMT_NONE	0		# SunOS4.0 and earlier
define	RMT_EQUAL_RGB	1		# SunOS4.0 and earlier
define	RMT_STANDARD	1		# SunOS4.0 and earlier
define	RAS_MAGIC	1504078485	# SunOS4.0 and earlier
define	NGREY		256		# SunOS4.0 and earlier, 8bit fb
define	COLORSTART	1		# IMTOOL
define	COLOREND	200		# IMTOOL
define	COLORRANGE	200		# IMTOOL
define	WHITE		(NGREY-1)	# IMTOOL
define	BLACK		0		# IMTOOL
define	NBITS_FB	8
define	wrapup_		91

# Spatial and greyscale transformation structure.
define	LEN_TR		20
define	TR_ZTRANS	Memi[$1]	# Greyscale transformation.
define	TR_Z1		Memr[P2R($1+1)]	# Minimum data z-value
define	TR_Z2		Memr[P2R($1+2)]	# Maximum data z-value
define	TR_XSIZE	Memi[$1+3]	# Output rasterfile size in x
define	TR_YSIZE	Memi[$1+4]	# Output rasterfile size in y
define	TR_XMAG		Memr[P2R($1+5)]	# Magnification factor in x
define	TR_YMAG		Memr[P2R($1+6)]	# Magnification factor in y
define	TR_ORDER	Memi[$1+7]	# Interpolation order
define	TR_XS		Memi[$1+8]	# Starting output x pixel index
define	TR_XE		Memi[$1+9]	# Ending output x pixel index
define	TR_YS		Memi[$1+10]	# Starting output y pixel index
define	TR_YE		Memi[$1+11]	# Ending output y pixel index
define	TR_SLICEAXIS	Memi[$1+12]	# Slice or frame axis when ndim>2 
define	TR_SWAPBYTES	Memb[$1+13]	# Swap output bytes?
#					# Reserved space
