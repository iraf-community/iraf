# Window descriptor structure.

define	LEN_WDES	(210+(W_MAXWC+1)*LEN_WC)
define	LEN_WC		10			# 4=[XbXeYbYe]+2=tr_type[xy]
define	W_MAXWC		5			# max world coord systems
define	W_SZSTRING	99			# size of strings
define	W_SZIMSECT	W_SZSTRING		# image section string

define	W_DEVICE	Memi[$1]
define	W_FRAME		Memi[$1+1]		# device frame number
define	W_XRES		Memi[$1+2]		# device resolution, x
define	W_YRES		Memi[$1+3]		# device resolution, y
define	W_BPDISP	Memi[$1+4]		# bad pixel display option
define	W_BPCOLORS	Memi[$1+5]		# overlay colors
define	W_OCOLORS	Memi[$1+6]		# badpixel colors
define	W_IMSECT	Memc[P2C($1+10)]	# image section
define	W_OVRLY		Memc[P2C($1+60)]	# overlay mask
define	W_BPM		Memc[P2C($1+110)]	# bad pixel mask
define	W_ZPM		Memc[P2C($1+160)]	# Z scaling pixel mask
define	W_WC		($1+$2*LEN_WC+210)	# ptr to coord descriptor

# Fields of the WC coordinate descriptor, a substructure of the window
# descriptor.  "W_XB(W_WC(w,0))" is the XB field of wc 0 of window W.

define	W_XS		Memr[P2R($1)]		# starting X value
define	W_XE		Memr[P2R($1+1)]		# ending X value
define	W_XT		Memi[$1+2]		# X transformation type
define	W_YS		Memr[P2R($1+3)]		# starting Y value
define	W_YE		Memr[P2R($1+4)]		# ending Y value
define	W_YT		Memi[$1+5]		# Y transformation type
define	W_ZS		Memr[P2R($1+6)]		# starting Z value (greyscale)
define	W_ZE		Memr[P2R($1+7)]		# ending Z value
define	W_ZT		Memi[$1+8]		# Z transformation type
define	W_UPTR		Memi[$1+9] 		# LUT when ZT=USER

# WC types.

define	W_NWIN		0	# Display window in NDC coordinates
define	W_DWIN		1	# Display window in image pixel coordinates
define	W_WWIN		2	# Display window in image world coordinates
define	W_IPIX		3	# Image pixel coordinates (in pixels)
define	W_DPIX		4	# Display pixel coordinates (in pixels)

# Types of coordinate and greyscale transformations.

define	W_UNITARY	0			# values map without change
define	W_LINEAR	1			# linear mapping
define	W_LOG		2			# logarithmic mapping
define  W_USER		3			# user specifies transformation
