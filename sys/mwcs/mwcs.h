# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MWCS.H -- Global (internal) definitions for the mini-WCS interface.

define	MWCS_MAGIC	4D57X		# identifies MWCS descriptors
define	DEF_SZSBUF	512		# initial string buffer size
define	INC_SZSBUF	512		# increment string buffer size
define	DEF_SZDBUF	64		# initial double buffer size
define	INC_SZDBUF	64		# increment double buffer size
define	MAX_DIM		7		# max dimension of a wcs
define	MAX_WCS		8		# max wcs per mwcs
define	MAX_WATTR	512		# max attributes per wcs
define	MAX_CTRAN	20		# max runtime ctran descriptors
define	MAX_CALL	7		# max CTRAN function calls
define	MAX_FUNC	7		# max WCS function descriptors
define	MAX_WCSFD	10		# max loaded WCS function drivers
define	MAX_FN		32		# max MWCS function drivers
define	SZ_FNNAME	20		# max size function name
define	SZ_ATNAME	20		# max size attribute name

# MWCS descriptor.  This consists of the base descriptor, global string
# buffer, global data buffer (TY_DOUBLE), and separately allocated buffers
# for each runtime CT (coordinate transformation) descriptor.  All character
# data is stored in SBUF.  All floating point data is stored as type double
# in DBUF (this does not mean that coordinate transformations are necessarily
# carried out in double precision).  All string and floating data is
# referenced in the base descriptor by its index in the appropriate data
# buffer, to make the descriptor invariant with respect to relocation of DBUF.
# To keep things simple, space is preallocated for a fixed number of WCS,
# and for each WCS, for a fixed number of attributes.

define	LEN_BASEMWCS	70
define	LEN_WCS		1626 # (depends upon MAX_WATTR)
define	LEN_MWCS	(LEN_BASEMWCS+LEN_WCS*MAX_WCS)
define	MI_LEN		(LEN_BASEMWCS+LEN_WCS*MI_NWCS($1))

define	MI_MAGIC	Memi[$1]		# magic marker
define	MI_NDIM		Memi[$1+1]		# wcs physical dimension
define	MI_WCS		Memi[$1+2]		# pointer to current wcs
define	MI_NWCS		Memi[$1+3]		# number of wcs defined
define	MI_REFIM	Memi[$1+4]		# reference image, if any
define	MI_SBUF		Memi[$1+5]		# string buffer pointer
define	MI_SBUFLEN	Memi[$1+6]		# string buffer alloclen
define	MI_SBUFUSED	Memi[$1+7]		# string buffer chars used
define	MI_DBUF		Memi[$1+8]		# double buffer pointer
define	MI_DBUFLEN	Memi[$1+9]		# double buffer alloclen
define	MI_DBUFUSED	Memi[$1+10]		# double buffer doubles used
define	MI_USEAXMAP	Memi[$1+11]		# enable axis mapping
define	MI_NLOGDIM	Memi[$1+12]		# dimension of logical system
	# (available)
define	MI_LTV		Memi[$1+18]		# dbuf index of LT vector
define	MI_LTM		Memi[$1+19]		# dbuf index of LT matrix
define	MI_AXNO		Memi[$1+20+($2)-1]	# axis map, log[phys]
define	MI_AXVAL	Memi[$1+30+($2)-1]	# axis value, if axno[i]=0
define	MI_PHYSAX	Memi[$1+40+($2)-1]	# inverted map, phys[log]
define	MI_CTRAN	Memi[$1+50+($2)-1]	# ctran descriptor pointers
define	MI_WCSP		($1+70+(($2)-1)*LEN_WCS)

# WCS descriptor.  This consists of a base structure, used to index string
# and double data which is stored in the global buffers SBUF and DBUF.
# An array of WCS descriptors is preallocated in the main MWCS descriptor.

define	WCS_NDIM	Memi[$1]		# dimension of world system
define	WCS_SYSTEM	Memi[$1+1]		# sbuf index of system name
define	WCS_AXCLASS	Memi[$1+2+($2)-1]	# axis type, 0 or FUNC index
define	WCS_R		Memi[$1+10]		# dbuf index of R array
define	WCS_W		Memi[$1+11]		# dbuf index of W array
define	WCS_CD		Memi[$1+12]		# dbuf index of CD matrix
define	WCS_NPTS	Memi[$1+20+($2)-1]	# number of points in wsampv
define	WCS_PV		Memi[$1+30+($2)-1]	# wsamp physical vector
define	WCS_WV		Memi[$1+40+($2)-1]	# wsamp world vector
define	WCS_NFUNC	Memi[$1+49]		# number of functions
define	WCS_FUNC	($1+50+(($2)-1)*5)	# function descriptors
define	WCS_NWATTR	Memi[$1+89]		# number of wcs attributes
define	WCS_WATTR	($1+90+(($2)-1)*3)	# pointer to wattr substruct

# WCS function descriptor.
define	LEN_WF		5
define	WF_FN		Memi[$1]		# function code
define	WF_NAXES	Memi[$1+1]		# number of axes 
define	WF_AXIS		Memi[$1+2+($2)-1]	# axes function applies to

# Function type flags.
define	FORWARD		0			# forward transform (P->W)
define	INVERSE		1			# inverse transform (W->P)

# WCS attribute descriptor.
define	LEN_AT		3
define	AT_AXIS		Memi[$1]		# wcs axis which owns attribute
define	AT_NAME		Memi[$1+1]		# sbuf index of name string
define	AT_VALUE	Memi[$1+2]		# sbuf index of value string

# CTRAN descriptor.  Prepared when a coordinate transformation is set up
# with mw_sctran.  The transformation is optimized and reduced to a series
# of matrix multiply, translate, wcs function call etc. instructions as
# described by this descriptor.  Both single and double precision versions
# of the transform are prepared, with the application deciding at runtime
# which precision routine to call.

define	LEN_CTBASE	(20+MAX_CALL*LEN_FC*2)

define	CT_D		($1)			# pointer to type double CT
define	CT_R		Memi[$1]		# pointer to type real CT
define	CT_MW		Memi[$1+1]		# pointer back to MWCS
define	CT_WCSI		Memi[$1+2]		# pointer back to system 1
define	CT_WCSO		Memi[$1+3]		# pointer back to system 2
define	CT_TYPE		Memi[$1+4]		# ctran type (optimized)
define	CT_NDIM		Memi[$1+5]		# ctran physical dimension
define	CT_LTM		Memi[$1+6]		# pointer to rot matrix
define	CT_LTV		Memi[$1+7]		# pointer to translation vector
define	CT_NCALLI	Memi[$1+8]		# number of function calls
define	CT_NCALLO	Memi[$1+9]		# number of function calls
define	CT_AXIS		Memi[$1+10+($2)-1]	# maps ctran axis to physax
define	CT_FCI		($1+20+(($2)-1)*LEN_FC)	# pointer to CALL descriptor
define	CT_FCO		($1+188+(($2)-1)*LEN_FC)

# CT types, for optimized transforms.
define	LNR		0			# linear, not rotated
define	LRO		1			# linear, rotated
define	GEN		2			# general catch all case

# Base FC (WCS function call) descriptor.  This consists of a base descriptor
# common to all WCS functions, followed by a private area reserved for use
# by the WCS function.

define	LEN_FC		64
define	FC_CT		Memi[$1]		# CTRAN descriptor
define	FC_WCS		Memi[$1+1]		# WCS descriptor
define	FC_WF		Memi[$1+2]		# WF descriptor
define	FC_FCN		Memi[$1+3]		# epa of WCS function
define	FC_NAXES	Memi[$1+4]		# number of axes in call
define	FC_AXIS		Memi[$1+5+($2)-1]	# CTRAN axes used by FC (max 3)
define	FCU		8			# offset to first user field

# WCS function driver (stored in common).
define	LEN_FN		5			# length of function driver
define	FN_FLAGS	fn_table[1,$1]		# function type flags
define	FN_INIT		fn_table[2,$1]		# initialize call descriptor
define	FN_DESTROY	fn_table[3,$1]		# free call descriptor
define	FN_FWD		fn_table[4,$1]		# forward transformation
define	FN_INV		fn_table[5,$1]		# inverse transformation
define	FN_NAME		fn_names[1,$1]		# function name

# WCS function codes.
define	F_LINEAR	0			# linear (not a function)

# WCS function type bit flags.
define	F_RADEC		01B			# function requires RA/DEC

# Handy macros.
define	S		Memc[MI_SBUF($1)+$2-1]	# string = S(mw,i)
define	D		Memd[MI_DBUF($1)+$2-1]	# double = D(mw,i)
