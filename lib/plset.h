# PLSET.H -- User global defines for the Pixel List (image mask) package.

define	PL_MAXDIM	7		# maximum mask dimensionality
define	PL_MAXDEPTH	27		# max mask depth, pixels
define	PL_NOTEQUAL	0		# two masks are not equivalent
define	PL_EQUAL	1		# two masks are equivalent
define	PL_UPDATE	1		# enable overwrite of existing savefile

# PLSETI/PLSTATI parameter codes.
define	P_PRIVATE1	1		# used by client, e.g., PMIO
define	P_PRIVATE2	2		# used by client, e.g., PMIO
define	P_MAXLINE	3		# used to size dynamic line buffers
define	P_DEPTH		4		# mask pixel depth, bits (1=boolean)

# Range list definitions.  For applications which access mask lines as range
# lists (ranges of constant nonzero value) rather than as pixel arrays.
# Here, $1 = rl (the range-list array).

define	RL_FIRST	2		# first data range entry in list
define	RL_LENELEM	3		# size of each element of list
define	RL_MAXLEN	(Memi[$1+3]*3)	# maximum range list length (arg=pl)

define	RL_LEN		$1[1,1]		# physical length of range list
define	RL_AXLEN	$1[2,1]		# length of mask image line
define	RLS_LEN		Mems[$1]	# RL_LEN for rl = ptr to int
define	RLS_AXLEN	Mems[$1+1]	# RL_AXLEN  "     "     "
define	RLI_LEN		Memi[$1]	# RL_LEN for rl = ptr to int
define	RLI_AXLEN	Memi[$1+1]	# RL_AXLEN  "     "     "
define	RLL_LEN		Meml[$1]	# RL_LEN for rl = ptr to int
define	RLL_AXLEN	Meml[$1+1]	# RL_AXLEN  "     "     "

define	RL_X		$1[1,$2]	# fields of a range list entry ($2=rn)
define	RL_N		$1[2,$2]	# direct array references
define	RL_V		$1[3,$2]

define	RL_XOFF		0		# offsets
define	RL_NOFF		1
define	RL_VOFF		2

# Public rasterop definitions.  Rasterops are constructed using these macros
# and the bitwise intrinsic functions 'and', 'or', and 'xor' (but not 'not';
# use the PIX_NOT macro instead).

define	PIX_NOT		(and(17B,not($1)))
define	PIX_VALUE	(($1)*100B)

define	PIX_CLR		00B		# clear destination subregion
define	PIX_SET		17B		# set destination to a constant value
define	PIX_SRC		14B		# denotes source in rasterops
define	PIX_DST		12B		# denotes destination in rasterops

# Options for PL_DEBUG output.
define	PD_SUMMARY	1		# print mask summary information
define	PD_INDEX	2		# print index
define	PD_LLOUT	4		# print line lists as line lists
define	PD_RLOUT	8		# print line lists as range lists
define	PD_LHDR		16		# print line headers
