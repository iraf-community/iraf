# GEOTRAN Structure

define	LEN_GEOSTRUCT	30

# output picture formatting parameters

define	GT_NCOLS		Memi[$1]	# number of output columns
define	GT_NLINES		Memi[$1+1]	# number of output lines
define	GT_XMIN			Memr[$1+2]	# x minimum
define	GT_XMAX			Memr[$1+3]	# x maximum
define	GT_YMIN			Memr[$1+4]	# y minimun	
define	GT_YMAX			Memr[$1+5]	# y maximum
define	GT_XSCALE		Memr[$1+6]	# x scale
define	GT_YSCALE		Memr[$1+7]	# y scale

# transformation parameters

define	GT_GEOMODE		Memi[$1+8]	# Type of transformation
define	GT_XIN			Memr[$1+9]	# x input pixel
define	GT_YIN			Memr[$1+10]	# y input pixel
define	GT_XOUT			Memr[$1+11]	# x output pixel
define	GT_YOUT			Memr[$1+12]	# y output pixel
define	GT_XSHIFT		Memr[$1+13]	# x shift
define	GT_YSHIFT		Memr[$1+14]	# y shift
define	GT_XMAG			Memr[$1+15]	# input image x scale
define	GT_YMAG			Memr[$1+16]	# input image y scale
define	GT_XROTATION		Memr[$1+17]	# rotation angle
define	GT_YROTATION		Memr[$1+18]	# scale angle

# interpolation parameters
define	GT_XSAMPLE		Memr[$1+19]	# x surface subsampling
define	GT_YSAMPLE		Memr[$1+20]	# y surface subsampling
define	GT_INTERPOLANT		Memi[$1+21]	# image interpolant
define	GT_BOUNDARY		Memi[$1+22]	# boundary extension
define	GT_CONSTANT		Memr[$1+23]	# constant boundary extension
define	GT_FLUXCONSERVE		Memi[$1+24]	# conserve total flux

# GEOTRAN MODES

define	GT_NONE		1		# parameters defined by user
define	GT_LINEAR	2		# use linear transformation
define	GT_DISTORT	3		# distortion transformation only
define	GT_GEOMETRIC	4               # use full transformation
