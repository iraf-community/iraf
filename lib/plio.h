# PLIO.H -- Package internal global defines for the Pixel List package.

# Size limiting definitons
define	PL_MAXDIM	7		# maximum mask dimensionality
define	PL_MAGICVAL	2014		# identifies mask and version no.
define	PL_LLBUFLEN	512		# initial llbuf size
define	PL_STARTINC	512		# starting llbuf increment on overflow
define	PL_MAXINC	4096		# maximum increment per resize
define	PL_MAXFREE	40		# max percent free (wasted) space
define	PL_MAXDEPTH	27		# max mask depth, bits
define	PL_DEFMAXLINE	1024		# default max elements per [lr]list

# Main PL descriptor.
define	LEN_PLDES	28
define	MLO		3		# must update PLSET.H too if changed

define	PL_MAGIC	Memi[$1]	# magic / version no.
define	PL_PRIVATE1	Memi[$1+1]	# private data for client
define	PL_PRIVATE2	Memi[$1+2]	# more private data for client
define	PL_MAXLINE	Memi[$1+MLO]	# max elements per [lr]list [PLSET.H]
define	PL_MAXVAL	Memi[$1+4]	# max pixel value (2**N - 1)
define	PL_NAXES	Memi[$1+5]	# dimensionality
define	PL_AXLEN	Memi[$1+6+$2-1]	# length of each axis
define	PL_PLANE	Memi[$1+13+$2-1] # active plane (pl_setplane)
define	PL_LLBP		Memi[$1+20]	# line list bufptr
define	PL_LLOP		Memi[$1+21]	# next location in llbuf
define	PL_LLLEN	Memi[$1+22]	# current llbuf length
define	PL_LLFREE	Memi[$1+23]	# amount of free space in list
define	PL_LLNUPDATES	Memi[$1+24]	# number of list modifications
define	PL_LLINC	Memi[$1+25]	# current llbuf increment on overflow
define	PL_NLP		Memi[$1+26]	# number of line pointers
define	PL_LPP		Memi[$1+27]	# ptr to array of LL offsets

define	PM_REFIM	PL_PRIVATE1($1)	# reference image
define	PM_MAPXY	PL_PRIVATE2($1)	# need to transform coords

# Handy macros.
define	Ref		(PL_LLBP($1)+$2)# llbuf offset -> pointer
define	LL		Mems[Ref($1,$2)]# pointer -> array reference
define	PL_LP		Memi[PL_LPP($1)+$2-1]
define	PL_EMPTYLINE	0		# llbuf offset of the empty line
define	PL_NEEDCOMPRESS	((PL_LLFREE($1)*100/PL_LLOP($1))>PL_MAXFREE)
define	LL_MAXLEN	PL_MAXLINE($1)
define	RL_MAXLEN	(PL_MAXLINE($1)*3)
define	MV		((2**min($1,27))-1)

# Handy rasterop decoding macros.
define	R_OPCODE		and($1,17B)
define	R_DATA			($1/100B)
define	R_NEED_DST		(and(xor(($1)/2,($1)),5)!=0)
define	R_NEED_SRC		(and(xor(($1)/4,($1)),3)!=0)
define	R_NOTDST		(and($1,6)==4)
define	R_NOTSRC		(and($1,6)==2)

# Internal rasterop definitions (PIX_SRC=14B, PIX_DST=12B, CLR=0B, SET=17B).
define	PIX_NOTSRC		03B
define	PIX_NOTDST		05B
define	PIX_SRC_AND_DST		10B
define	PIX_SRC_OR_DST		16B
define	PIX_SRC_XOR_DST		06B
define	PIX_SRC_AND_NOTDST	04B
define	PIX_SRC_OR_NOTDST	15B
define	PIX_NOTSRC_AND_DST	02B
define	PIX_NOTSRC_OR_DST	13B
define	PIX_NOT_SRC_AND_DST	07B
define	PIX_NOT_SRC_OR_DST	01B
define	PIX_NOT_SRC_XOR_DST	11B


# Macro defines for the line list data format.

# ----- Old/original line list header definitions.  This version uses a
# ----- three element header, but the maximum values are limited to 32K by
# ----- the use of type short.

# Line list definitions (accessed as a short integer array).
define	OLL_NREF	$1[1]		# number of references
define	OLL_BLEN	$1[2]		# length of buffer containing LL
define	OLL_LEN		$1[3]		# length of encoded line list
define	OLL_FIRST	4		# first data range entry in list

# Line list definitions (accessed via a short integer pointer).
define	OLP_NREF	Mems[$1]	# number of references
define	OLP_BLEN	Mems[$1+1]	# length of buffer containing LL
define	OLP_LEN		Mems[$1+2]	# length of encoded line list
define	OLP_FIRST	3		# first data range entry in list


# ----- New format line list header definitions.  This version uses a 
# ----- variable length header and a version number to allow new encodings
# ----- while retaining backwards compatibility.

define  LL_CURVERSION	(-100)		# LL version code (must be negative)
define	LL_OLDFORMAT	(LL_VERSION($1) > 0)
define	LL_CURHDRLEN	7

# Line list definitions (accessed as a short integer array).
define	LL_NREFS	$1[1]		# number of references
define	LL_HDRLEN	$1[2]		# length of encoded line list
define	LL_VERSION	$1[3]		# version number (negative)
define	LL_LENLO	$1[4]		# length of encoded line list
define	LL_LENHI	$1[5]		# length of encoded line list
define	LL_BLENLO	$1[6]		# length of LL buffer
define	LL_BLENHI	$1[7]		# length of LL buffer

# Handy line list macros.
define	LL_NREF		(int(LL_NREFS($1)))
define	LL_BLEN		((int(LL_BLENHI($1)))*32768+(int(LL_BLENLO($1))))
define	LL_SETBLEN	LL_BLENLO($1)=mod($2,32768); LL_BLENHI($1)=($2)/32768
define	LL_LEN		((int(LL_LENHI($1)))*32768+(int(LL_LENLO($1))))
define	LL_SETLEN	LL_LENLO($1)=mod($2,32768); LL_LENHI($1)=($2)/32768
define	LL_FIRST	(LL_HDRLEN($1)+1)

# Line list definitions (accessed as a short integer pointer).
define	LP_NREFS	Mems[$1]	# number of references
define	LP_HDRLEN	Mems[$1+1]	# length of encoded line list
define	LP_VERSION	Mems[$1+2]	# version number (negative)
define	LP_LENLO	Mems[$1+3]	# length of encoded line list
define	LP_LENHI	Mems[$1+4]	# length of encoded line list
define	LP_BLENLO	Mems[$1+5]	# length of LL buffer
define	LP_BLENHI	Mems[$1+6]	# length of LL buffer

# Handy line list pointer macros.
define	LP_NREF		(int(LP_NREFS($1)))
define	LP_BLEN		(int(LP_BLENHI($1))*32768+int(LP_BLENLO($1)))
define	LP_SETBLEN	LP_BLENLO($1)=mod($2,32768); LP_BLENHI($1)=($2)/32768
define	LP_LEN		(int(LP_LENHI($1))*32768+int(LP_LENLO($1)))
define	LP_SETLEN	LP_LENLO($1)=mod($2,32768); LP_LENHI($1)=($2)/32768
define	LP_FIRST	(($1)+LP_HDRLEN($1))


# Packed instruction decoding.
define	I_SHIFT		10000B		# shift to encode/decode data bits
define	I_DATA		and(int($1),7777B)	# extract data
define	I_OPCODE	(($1)/10000B)	# extract opcode value
define	I_OPCODEMASK	and(int($1),70000B)	# extract opcode mask
define	I_DATAMAX	7777B		# max data field value
define	I_PVMAX		777777777B	# max mask pixel value (27 bits)

# LL instruction opcodes.
define	I_ZN		0		# N zeros
define	I_HN		4		# N high values
define	I_PN		5		# N-1 zeros and 1 high value
define	I_SH		1		# set high value (2 words)
define	I_IH		2		# increment high value
define	I_DH		3		# decrement high value
define	I_IS		6		# increment and output 1 high value
define	I_DS		7		# decrement and output 1 high value

# The LL instruction opcodes again, but as bitmasks this time.
define	M_ZN		00000B
define	M_HN		40000B
define	M_PN		50000B
define	M_SH		10000B
define	M_IH		20000B
define	M_DH		30000B
define	M_IS		60000B
define	M_DS		70000B

# The following bit is set if the instruction changes the current position.
define	M_MOVE		40000B

# PL external format descriptor (with some extra space).
define	LEN_PLEXTERN	20
define	PLE_MAGIC	Memi[$1]	# usual magic value
define	PLE_NAXES	Memi[$1+1]	# number of axes 
define	PLE_AXLEN	Memi[$1+2+$2-1]	# length of each axis
define	PLE_LLOP	Memi[$1+9]	# next location in llbuf
define	PLE_LLLEN	Memi[$1+10]	# length of llbuf
define	PLE_NLP		Memi[$1+11]	# number of line pointers
define	PLE_NLPX	Memi[$1+12]	# length of compressed LP array
define	PLE_EXLEN	Memi[$1+13]	# length of full PLEXTERN structure
define	PLE_FLAGS	Memi[$1+14]	# flags for type of encoding
define	PLE_MAXLINE	Memi[$1+15]	# max elements per [lr]list
define	PLE_MAXVAL	Memi[$1+16]	# max pixel value

# PLIO mask savefile descriptor.
define	PLIO_SVMAGIC	123126B		# "SV"
define	LEN_SVDES	3
define	SV_MAGIC	Memi[$1]	# identifies file type
define	SV_TITLELEN	Memi[$1+1]	# title string length, including EOS
define	SV_MASKLEN	Memi[$1+2]	# encoded mask length, shorts

# Internal symbols.
define	LOOP_DONE	1
define	LOOP_AGAIN	0
