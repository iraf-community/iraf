# IMIO.H -- IMIO internal definitions.

define	DEF_PIXTYPE		TY_REAL		# pixel type on disk
define	DEF_HGMLEN		512		# length of histogram
define	DEF_COMPRESS		NO		# align lines on blk boundaries
define	DEF_ADVICE		SEQUENTIAL	# type of access to optimize for
define	DEF_FIOBUFSIZE		65536		# default FIO buffer size
define	MAX_HGMLEN		2048		# maximum size histogram
define	IM_MAXSTEP		64		# step size breakpoint
define	MIN_LENUSERAREA		64000		# user area size (chars)
define	SZ_UAPAD		5120		# padding at end of UA if copy
define	SZ_IMNAME		79		# IM_NAME field

define	LOOP_DONE		1		# used by IMLOOP
define	LOOP_AGAIN		0
define	IM_READ			0		# used by IMCSSZ
define	IM_WRITE		1

# The IMIO runtime Image Descriptor Structure.

define	LEN_IMDES		200

define	IM_FILESIZE	Meml[$1]		# size of pixfile
define	IM_NGET		Meml[$1+1]		# number getpix calls
define	IM_ACMODE	Memi[$1+2]		# access mode (ro, rw, etc.)
define	IM_VNBUFS	Memi[$1+3]		# number of in buffers
define	IM_VCOMPRESS	Memi[$1+4]		# if YES, len[i] == physlen[i]
define	IM_VADVICE	Memi[$1+5]		# expected type of access
define	IM_VBUFSIZE	Memi[$1+6]		# recommended FIO buffer size
define	IM_VCLOSEFD	Memi[$1+7]		# set F_CLOSEFD on pixfile
define	IM_VNBNDRYPIX	Memi[$1+8]		# npixels of boundary extension
define	IM_VTYBNDRY	Memi[$1+9]		# type of boundary extension
define	IM_VFLAGBADPIX	Memi[$1+10]		# flag bad pixels upon input
define	IM_FLUSH	Memi[$1+11]		# flush outbuf?
define	IM_UPDATE	Memi[$1+12]		# update header?
define	IM_FLUSHEPA	Memi[$1+13]		# epa of imfls? routine
define	IM_IBDES	Memi[$1+14]		# input bufdes
define	IM_OBDES	Memi[$1+15]		# output bufdes
define	IM_LASTBDES	Memi[$1+16]		# last buffer accessed
define	IM_OHDR		Memi[$1+17]		# if newcopy, ptr to old header
define	IM_NPHYSDIM	Memi[$1+18]		# number of physical dims
define	IM_SECTUSED	Memi[$1+19]		# image section in use
define	IM_FAST		Memi[$1+20]		# fast i/o permitted
define	IM_SWAP		Memi[$1+21]		# byte swapping required
define	IM_SVMTIME	Meml[$1+22]		# new time of last modify
define	IM_OOBPIX	Memr[$1+23]		# value for out of bounds pixels
define	IM_KERNEL	Memi[$1+24]		# IKI kernel assigned (runtime)
define	IM_KDES		Memi[$1+25]		# IKI kernel descriptor
define	IM_HFD		Memi[$1+26]		# header file descriptor
define	IM_PFD		Memi[$1+27]		# pixel file descriptor
define	IM_LENHDRMEM	Memi[$1+28]		# descr. length, IM_MAGIC to end
define	IM_UABLOCKED	Memi[$1+29]		# user area blocked, 80 chars
define	IM_CLINDEX	Memi[$1+30]		# index of image in cluster
define	IM_CLSIZE	Memi[$1+31]		# no. images in cluster
define	IM_PL		Memi[$1+32]		# PL descriptor if mask image
define	IM_PLREFIM	Memi[$1+33]		# PL reference image if any
define	IM_PLFLAGS	Memi[$1+34]		# PL mask i/o flags
			# (extra space)
define	IM_SVLEN	Meml[$1+$2+35-1]	# save true axis lengths
define	IM_VMAP		Memi[$1+$2+42-1]	# map section dimensions
define	IM_VOFF		Meml[$1+$2+49-1]	# section offsets
define	IM_VSTEP	Memi[$1+$2+56-1]	# section sample step size
define	IM_NAME		Memc[P2C($1+63)]	# imagefile name
			# (extra space)

# IM_PLFLAGS bit flags.
define	PL_ACMODE	mod($1,100B)		# extract access mode
define	PL_FLAGS	(($1)/100B*100B)	# extract flags bits

define	PL_RLIO		1			# range list i/o desired
define	PL_FAST		2			# no pixel conversions needed
define	PL_CLOSEPL	4			# close descriptor at imunmap
define	PL_BOOL		8			# boolean mask

# Buffer Descriptor Structure

define	LEN_BDES	19

define	BD_BUFPTR	Memi[$1]		# buffer pointer
define	BD_DTYPE	Memi[$1+1]		# datatype of pixels
define	BD_NPIX		Memi[$1+2]		# number of pixels in buf
define	BD_NDIM		Memi[$1+3]		# dimensionality of section
define	BD_BUFSIZE	Memi[$1+4]		# buffer size, chars
define	BD_VS		Meml[$1+$2+5-1]		# section start vector
define	BD_VE		Meml[$1+$2+12-1]	# section end vector
