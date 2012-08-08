# Data structure for each spectrum

define	SP_SZNAME	99			# Length of image name
define	SP_SZTITLE	99			# Length of title
define	SP_SZPTYPE	9			# Length of plot type
define	SP_SZULABEL	99			# Length of user label
define	SP_SZLABEL	99			# Length of label
define	SP_LEN		225			# Length of SP structure

define	SP_INDEX	Memi[$1]		# Index
define	SP_SH		Memi[$1+1]		# Spectrum header
define	SP_NPTS		Memi[$1+2]		# Number of data points
define	SP_W0		Memr[P2R($1+3)]		# Starting wavelength
define	SP_WPC		Memr[P2R($1+4)]		# Wavelength per pix
define	SP_OMEAN	Memr[P2R($1+5)]		# Original mean intensity
define	SP_OMIN		Memr[P2R($1+6)]		# Original minimum intensity
define	SP_OMAX		Memr[P2R($1+7)]		# Original maximum intensity

define	SP_XSCALE	Memr[P2R($1+8)]		# Wavelength scale
define	SP_XOFFSET	Memr[P2R($1+9)]		# Wavelength offset
define	SP_SCALE	Memr[P2R($1+10)]	# Intensity scale
define	SP_OFFSET	Memr[P2R($1+11)]	# Intensity offset
define	SP_MEAN		Memr[P2R($1+12)]	# Mean intensity
define	SP_MIN		Memr[P2R($1+13)]	# Minimum intensity
define	SP_MAX		Memr[P2R($1+14)]	# Maximum intensity
define	SP_PX		Memi[$1+15]		# Pointer to wavelengths
define	SP_PY		Memi[$1+16]		# Pointer to intensities
define	SP_XLPOS	Memr[P2R($1+17)]	# X label position
define	SP_YLPOS	Memr[P2R($1+18)]	# Y label position
define	SP_COLOR	Memi[$1+19]		# Color
define	SP_IMNAME	Memc[P2C($1+20)]	# Image name
define	SP_IMTITLE	Memc[P2C($1+70)]	# Title
define	SP_PTYPE	Memc[P2C($1+120)]	# Plot type
define	SP_ULABEL	Memc[P2C($1+125)]	# Label
define	SP_LABEL	Memc[P2C($1+175)]	# Label

define	SP_X		Memr[SP_PX($1)]		# Wavelengths
define	SP_Y		Memr[SP_PY($1)]		# Intensities

define	LABELS		"|none|imname|imtitle|index|user|"
define	LABEL_NONE	1	# No labels
define	LABEL_IMNAME	2	# Image name
define	LABEL_IMTITLE	3	# Image title
define	LABEL_INDEX	4	# Index
define	LABEL_USER	5	# No labels

define	TRANSFORMS	"|none|log|"
define	TRANS_NONE	1	# No transform
define	TRANS_LOG	2	# Log transform
