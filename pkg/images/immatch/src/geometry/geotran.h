# GEOTRAN Structure

define	LEN_GEOSTRUCT	(30 + SZ_FNAME)

# output picture formatting parameters

define	GT_NCOLS		Memi[$1]	 # number of output columns
define	GT_NLINES		Memi[$1+1]	 # number of output lines
define	GT_XMIN			Memr[P2R($1+2)]	 # x minimum
define	GT_XMAX			Memr[P2R($1+3)]	 # x maximum
define	GT_YMIN			Memr[P2R($1+4)]	 # y minimun	
define	GT_YMAX			Memr[P2R($1+5)]	 # y maximum
define	GT_XSCALE		Memr[P2R($1+6)]	 # x scale
define	GT_YSCALE		Memr[P2R($1+7)]	 # y scale

# transformation parameters

define	GT_GEOMODE		Memi[$1+8]	 # Type of transformation
define	GT_XIN			Memr[P2R($1+9)]	 # x input pixel
define	GT_YIN			Memr[P2R($1+10)] # y input pixel
define	GT_XOUT			Memr[P2R($1+11)] # x output pixel
define	GT_YOUT			Memr[P2R($1+12)] # y output pixel
define	GT_XSHIFT		Memr[P2R($1+13)] # x shift
define	GT_YSHIFT		Memr[P2R($1+14)] # y shift
define	GT_XMAG			Memr[P2R($1+15)] # input image x scale
define	GT_YMAG			Memr[P2R($1+16)] # input image y scale
define	GT_XROTATION		Memr[P2R($1+17)] # rotation angle
define	GT_YROTATION		Memr[P2R($1+18)] # scale angle

# interpolation parameters
define	GT_XSAMPLE		Memr[P2R($1+19)] # x surface subsampling
define	GT_YSAMPLE		Memr[P2R($1+20)] # y surface subsampling
define	GT_INTERPOLANT		Memi[$1+21]	 # image interpolant
define	GT_NSINC		Memi[$1+22]	 # sinc width half-width
define	GT_NXYMARGIN		Memi[$1+23]	 # the interpolation margin
define	GT_BOUNDARY		Memi[$1+24]	 # boundary extension
define	GT_CONSTANT		Memr[P2R($1+25)] # constant boundary extension
define	GT_FLUXCONSERVE		Memi[$1+26]	 # conserve total flux
define	GT_INTERPSTR		Memc[P2C($1+27)] # interpolation string

# GEOTRAN MODES

define	GT_NONE		1		# parameters defined by user
define	GT_LINEAR	2		# use linear transformation
define	GT_DISTORT	3		# distortion transformation only
define	GT_GEOMETRIC	4               # use full transformation

# GEOTRAN COORDINATE MODES

define	GT_ONE		1
define	GT_TWO		2
define	GT_FOUR		3
