# Graphics tools definitions.

define	GTXMIN		0	# WCS X minimum
define	GTXMAX		1	# WCS X maximum
define	GTYMIN		2	# WCS Y minimum
define	GTYMAX		3	# WCS Y maximum
define	GTXTRAN		4	# WCS X transform
define	GTYTRAN		5	# WCS Y transform
define	GTXSIZE		6	# X Mark size
define	GTYSIZE		7	# Y Mark size

define	GTSYSID		22	# Add SYSID?
define	GTPARAMS	8	# Graph parameters
define	GTTITLE		9	# Graph title
define	GTSUBTITLE	10	# Graph subtitle
define	GTCOMMENTS	11	# Comments
define	GTXLABEL	12	# X label
define	GTYLABEL	13	# Y label
define	GTXUNITS	14	# X units
define	GTYUNITS	15	# Y units

define	GTXBUF		16	# Autoscaling buffer factor
define	GTYBUF		17	# Autoscaling buffer factor
define	GTTYPE		18	# Graph type
define	GTMARK		19	# Mark type
define	GTLINE		20	# Line type
define	GTCOLOR		23	# Color
define	GTXFLIP		24	# Flip X axis
define	GTYFLIP		25	# Flip X axis
define	GTTRANSPOSE	21	# Transpose X and Y axes?

define	LEN_GT		26	# Length of graphics tools extension

define	GT_XMIN		Memr[$1+GTXMIN]
define	GT_XMAX		Memr[$1+GTXMAX]	
define	GT_YMIN		Memr[$1+GTYMIN]
define	GT_YMAX		Memr[$1+GTYMAX]
define	GT_XTRAN	Memi[$1+GTXTRAN]
define	GT_YTRAN	Memi[$1+GTYTRAN]
define	GT_XSIZE	Memr[$1+GTXSIZE]
define	GT_YSIZE	Memr[$1+GTYSIZE]
define	GT_SYSID	Memi[$1+GTSYSID]
define	GT_PARAMS	Memi[$1+GTPARAMS]
define	GT_TITLE	Memi[$1+GTTITLE]
define	GT_SUBTITLE	Memi[$1+GTSUBTITLE]
define	GT_COMMENTS	Memi[$1+GTCOMMENTS]
define	GT_XLABEL	Memi[$1+GTXLABEL]
define	GT_YLABEL	Memi[$1+GTYLABEL]
define	GT_XUNITS	Memi[$1+GTXUNITS]
define	GT_YUNITS	Memi[$1+GTYUNITS]
define	GT_XBUF		Memr[$1+GTXBUF]
define	GT_YBUF		Memr[$1+GTYBUF]
define	GT_TYPE		Memi[$1+GTTYPE]
define	GT_MARK		Memi[$1+GTMARK]
define	GT_LINE		Memi[$1+GTLINE]
define	GT_COLOR	Memi[$1+GTCOLOR]
define	GT_XFLIP	Memi[$1+GTXFLIP]
define	GT_YFLIP	Memi[$1+GTYFLIP]
define	GT_TRANSPOSE	Memi[$1+GTTRANSPOSE]

define	GTTYPES		"|mark|line|histogram"
define	GTMARKS	"|point|box|plus|cross|diamond|hline|vline|hebar|vebar|circle|"
