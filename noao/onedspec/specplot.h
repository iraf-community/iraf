# Data structure for each spectrum

define	SP_SZNAME	99			# Length of image name
define	SP_SZTITLE	99			# Length of title
define	SP_SZPTYPE	9			# Length of plot type
define	SP_SZULABEL	99			# Length of user label
define	SP_SZLABEL	99			# Length of label
define	SP_LEN		224			# Length of SP structure

define	SP_INDEX	Memi[$1]		# Index
define	SP_PDATA	Memi[$1+1]		# Pointer to raw data
define	SP_NPTS		Memi[$1+2]		# Number of data points
define	SP_OW0		Memr[$1+3]		# Original starting wavelength
define	SP_OWPC		Memr[$1+4]		# Original wavelength per pix
define	SP_OMEAN	Memr[$1+5]		# Original mean intensity
define	SP_OMIN		Memr[$1+6]		# Original minimum intensity
define	SP_OMAX		Memr[$1+7]		# Original maximum intensity

define	SP_W0		Memr[$1+8]		# Starting wavelength
define	SP_WPC		Memr[$1+9]		# Wavelength per channel
define	SP_MEAN		Memr[$1+10]		# Mean intensity
define	SP_MIN		Memr[$1+11]		# Minimum intensity
define	SP_MAX		Memr[$1+12]		# Maximum intensity
define	SP_SCALE	Memr[$1+13]		# Intensity scale
define	SP_OFFSET	Memr[$1+14]		# Intensity offset
define	SP_PX		Memi[$1+15]		# Pointer to wavelengths
define	SP_PY		Memi[$1+16]		# Pointer to intensities
define	SP_XLPOS	Memr[$1+17]		# X label position
define	SP_YLPOS	Memr[$1+18]		# Y label position
define	SP_IMNAME	Memc[P2C($1+19)]	# Image name
define	SP_IMTITLE	Memc[P2C($1+69)]	# Title
define	SP_PTYPE	Memc[P2C($1+119)]	# Plot type
define	SP_ULABEL	Memc[P2C($1+124)]	# Label
define	SP_LABEL	Memc[P2C($1+174)]	# Label

define	SP_DATA		Memr[SP_PDATA($1)]	# Data
define	SP_X		Memr[SP_PX($1)]		# Wavelengths
define	SP_Y		Memr[SP_PY($1)]		# Intensities

define	LABELS		"|none|imname|imtitle|index|user|"
define	LABEL_NONE	1	# No labels
define	LABEL_IMNAME	2	# Image name
define	LABEL_IMTITLE	3	# Image title
define	LABEL_INDEX	4	# Index
define	LABEL_USER	5	# No labels
