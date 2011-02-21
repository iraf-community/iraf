# QPIO.H -- Definitions for the QPOE event i/o subpackage.

# Default parameter and domain names.
define	DEF_BLOCK	"defblock"	# header param - default block factor
define	DEF_XBLOCK	"defxblock"	# header param - default X block factor
define	DEF_YBLOCK	"defyblock"	# header param - default Y block factor
define	DEF_MASK	"defmask"	# header param - default region mask
define	DEF_FILTER	"deffilt"	# header param - default event filter
define	DEF_EVENTTYPE	"event"		# default name of user event datatype
define	DEF_EVENTPARAM	"events"	# default event-list parameter

# QPIO keywords recognized in expressions (abbreviations permitted).
define KEYWORDS "|block|xblock|yblock|debug|filter|key|noindex|param|mask|rect|"

define	KW_BLOCK	1		# blocking factor for image matrix
define	KW_XBLOCK	2		# X blocking factor for image matrix
define	KW_YBLOCK	3		# Y blocking factor for image matrix
define	KW_DEBUG	4		# debug level (integer, 0=nodebug)
define	KW_FILTER	5		# event attribute filter
define	KW_KEY		6		# event key (Y,X) fields (e.g.(s10,s8))
define	KW_NOINDEX	7		# don't use index even if present
define	KW_PARAM	8		# name of event list header parameter
define	KW_MASK		9		# region mask
define	KW_RECT		10		# rectangle (bounding box) for i/o

# Size limiting definitions.
define	SZ_EVLIST	1024		# event list buffer size (arbitrary)
define	NDIM		2		# all QPOE images are 2-dim

# The main QPIO descriptor.
define	LEN_IODES	82

# general
define	IO_QP		Memi[$1]	# backpointer to QPOE descriptor
define	IO_MODE		Memi[$1+1]	# read_only or new_file
define	IO_DEBUG	Memi[$1+2]	# debug level
define	IO_NLINES	Memi[$1+3]	# number of image lines (physical)
define	IO_NCOLS	Memi[$1+4]	# number of image columns (physical)
define	IO_XBLOCK	Memr[P2R($1+5)]	# blocking factor for qpio_readpix
define	IO_YBLOCK	Memr[P2R($1+6)]	# blocking factor for qpio_readpix
define	IO_OPTBUFSIZE	Memi[$1+7]	# optbufsize for FIO (qpio_readpix)
define	IO_NOINDEX	Memi[$1+8]	# don't use indexed extraction
define	IO_NODEFFILT	Memi[$1+9]	# disable use of default filter
define	IO_NODEFMASK	Memi[$1+10]	# disable use of default mask
define	IO_PARAM	Memi[$1+11]	# pointer to buffer with param name
define	IO_PSYM		Memi[$1+12]	# symbol table entry for parameter
define	IO_MASK		Memi[$1+13]	# pointer to buffer with mask name
define	IO_MDEPTH	Memi[$1+14]	# mask depth, bits
define	IO_EXCLOSE	Memi[$1+15]	# qpex was opened by qpio
define	IO_PLCLOSE	Memi[$1+16]	# mask was opened by qpio
define	IO_PL		Memi[$1+17]	# PLIO (mask) pointer
define	IO_EX		Memi[$1+18]	# QPEX (event attribute filter) pointer
define	IO_FD		Memi[$1+19]	# file descriptor of open lfile
define	IO_LF		Memi[$1+20]	# lfile where event list is stored
define	IO_CHAN		Memi[$1+21]	# i/o channel of open lfile
# events
define	IO_DD		Memi[$1+22]	# pointer to domain descriptor
define	IO_EVXOFF	Memi[$1+23]	# offset of X field used for extraction
define	IO_EVXTYPE	Memi[$1+24]	# datatype of X field
define	IO_EVYOFF	Memi[$1+25]	# offset of Y field used for extraction
define	IO_EVYTYPE	Memi[$1+26]	# datatype of Y field
define	IO_EVENTLEN	Memi[$1+27]	# length of event struct, shorts
define	IO_MINEVL	Memi[$1+28]	# pointer to min event for full list
define	IO_MAXEVL	Memi[$1+29]	# pointer to max event for full list
# buckets
define	IO_SZBBUCKET	Memi[$1+30]	# event file bucket size, bytes
define	IO_BUCKETLEN	Memi[$1+31]	# nevents per bucket (excl. min/max)
define	IO_NEVENTS	Memi[$1+32]	# total data events in event list
define	IO_FBOFF	Memi[$1+33]	# lfile offset of first bucket
define	IO_EVMINOFF	Memi[$1+34]	# offset to the MIN event in a bucket
define	IO_EVMAXOFF	Memi[$1+35]	# offset to the MAX event in a bucket
# index
define	IO_INDEXLEN	Memi[$1+38]	# length of index (same as nlines)
define	IO_IXXOFF	Memi[$1+39]	# offset of X field used in index
define	IO_IXXTYPE	Memi[$1+40]	# datatype of X field used in index
define	IO_IXYOFF	Memi[$1+41]	# offset of Y field used in index
define	IO_IXYTYPE	Memi[$1+42]	# datatype of Y field used in index
define	IO_YOFFVP	Memi[$1+43]	# pointer to Y-index array (len=nlines)
define	IO_YLENVP	Memi[$1+44]	# pointer to Y-line length array
define	IO_YOFFVOFF	Memi[$1+45]	# lfile offset of stored YOFFV
define	IO_YOFFVLEN	Memi[$1+46]	# length, words, of compressed YOFFV
define	IO_YLENVOFF	Memi[$1+47]	# lfile offset of stored YLENV
define	IO_YLENVLEN	Memi[$1+48]	# length, words, of compressed YLENV
# i/o
define	IO_ACTIVE	Memi[$1+50]	# set once i/o begins
define	IO_IOTYPE	Memi[$1+51]	# type of i/o selected for BB
define	IO_LINEIO	Memi[$1+52]	# flag - BB width is full line width
define	IO_RMUSED	Memi[$1+53]	# flag - region mask used in this BB
define	IO_BBUSED	Memi[$1+54]	# flag - bounding box in use
define	IO_BBMASK	Memi[$1+55]	# BB region mask subras, nonindexed i/o
define	IO_RL		Memi[$1+56]	# range list pointer
define	IO_RLI		Memi[$1+57]	# range list index
define	IO_EVI		Memi[$1+58]	# event index into event list (for i/o)
define	IO_EV1		Memi[$1+59]	# event index of first event on line
define	IO_EV2		Memi[$1+60]	# event index of last event on line
define	IO_BP		Memi[$1+61]	# pointer to bucket buffer
define	IO_BKNO		Memi[$1+62]	# bucket number
define	IO_BKFIRSTEV	Memi[$1+63]	# event index of first event in bucket
define	IO_BKLASTEV	Memi[$1+64]	# event index of last event in bucket
# (avail)
define	IO_V		Meml[$1+70+$2-1]# current vector
define	IO_VS		Meml[$1+72+$2-1]# start vector
define	IO_VE		Meml[$1+74+$2-1]# end vector
define	IO_VN		Meml[$1+76+$2-1]# size of section
define	IO_VSDEF	Meml[$1+78+$2-1]# default start vector
define	IO_VEDEF	Meml[$1+80+$2-1]# default end vector

# Handy macros.
define	IO_MINEVB	(IO_BP($1)+IO_EVMINOFF($1))
define	IO_MAXEVB	(IO_BP($1)+IO_EVMAXOFF($1))
define	EVI_TO_BUCKET	((($2)-1)/IO_BUCKETLEN($1)+1)
define	BUCKET_TO_EVI	((($2)-1)*IO_BUCKETLEN($1)+1)

# I/O types (specially optimized code for each case).
define	NoINDEX_NoRMorBB	0	# nonindexed, no RM no BB
define	NoINDEX_RMorBB		1	# nonindexed, maybe RM or BB
define	INDEX_NoRMorBB		2	# indexed, no RM or BB
define	INDEX_RMorBB		3	# indexed, maybe RM or BB
define	NoDATA_NoAREA		4	# no events can be returned

# Stored Event List header (one per stored event list).
define	LEN_EHDES	18
define	EH_NEVENTS	Memi[$1]	# total data events in event list
define	EH_EVENTLEN	Memi[$1+1]	# event length, shorts
define	EH_SZBBUCKET	Memi[$1+2]	# event file bucket size, bytes
define	EH_BUCKETLEN	Memi[$1+3]	# nevents per bucket (excl. min/max)
define	EH_FBOFF	Memi[$1+4]	# lfile offset of first bucket
define	EH_EVMINOFF	Memi[$1+5]	# offset to the MIN event in a bucket
define	EH_EVMAXOFF	Memi[$1+6]	# offset to the MAX event in a bucket
define	EH_MINEVLOFF	Memi[$1+7]	# offset of stored MINEVL
define	EH_MAXEVLOFF	Memi[$1+8]	# offset of stored MAXEVL
define	EH_INDEXLEN	Memi[$1+9]	# length of index (same as nlines)
define	EH_YOFFVOFF	Memi[$1+10]	# lfile offset of stored YOFFV
define	EH_YOFFVLEN	Memi[$1+11]	# length, words, of compressed YOFFV
define	EH_YLENVOFF	Memi[$1+12]	# lfile offset of stored YLENV
define	EH_YLENVLEN	Memi[$1+13]	# length, words, of compressed YLENV
define	EH_IXXOFF	Memi[$1+14]	# event offset of indexed X field
define	EH_IXYOFF	Memi[$1+15]	# event offset of indexed Y field
define	EH_IXXTYPE	Memi[$1+16]	# datatype of indexed X field
define	EH_IXYTYPE	Memi[$1+17]	# datatype of indexed Y field
