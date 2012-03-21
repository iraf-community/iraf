# KI.H -- IRAF Kernel Interface definitions.

define	MAX_NODES	512		# max nodes known to KI
define	MAX_CHANNELS	LAST_FD		# requires <config.h>
define	MAX_INDIRECT	20		# max indirection in a route
define	MAX_ALIAS	6		# maximum number of aliases per node
define	SZ_ALIAS	16		# size of a node name alias
define	SZ_SERVER	128		# size of a server name
define	HNT_SUBDIR	"dev"		# parts of host name table filename
define	HNT_FILENAME	"hosts"		# default host name table
define	HNT_ENVNAME	"irafhnt"	# user host name table

define	READ_IN_PROGRESS (-11)		# used in binary file i/o
define	OSHIFT		128		# optype= (opcode * OSHIFT) + subcode
define	FIRST_CHAN	4		# first available kchan.
define	LOCAL		0		# node is a local node
define	REMOTE		1		# node is a remote node

# Node status flags.
define	F_IOERR		01B		# fatal error on kschan
define	F_REUSE		02B		# node descriptor may be reused

# ZFIOTX buffer descriptor.

define	SZ_TXBUF	16386		# size of text file buffer (should be
					# at least (2*LEN_SEQBUF*SZ_LINE))
define	LEN_TXBDES	(5+SZ_TXBUF/SZ_STRUCT)

define	B_CI		Memi[$1]	# character index into current record
define	B_RP		Memi[$1+1]	# pointer to current record
define	B_ITOP		Memi[$1+2]	# end of input buffer
define	B_OTOP		Memi[$1+3]	# end of output buffer
define	B_BUFTOP	Memi[$1+4]	# end of buffer
define	B_BUFPTR	P2C(($1)+5)	# first char of buffer

# ZGFDIR buffer descriptor.

define	SZ_DIRDATA	2048		# amount of directory data to read
define	LEN_DIRBDES	(5+SZ_DIRDATA/SZ_STRUCT)

define	D_IP		Memi[$1]	# input pointer into dirbuf
define	D_ITOP		Memi[$1+1]	# top of dirbuf
define	D_EOFSEEN	Memi[$1+2]	# dirbuf contains last of data
define	D_DATA		P2C(($1)+5)	# pointer to data area

# Record descriptor structure (format of a line of text record in the input
# buffer when reading from a remote text file).

define	R_RECLEN	Memc[$1]	# encoded record length (2 chars)
define	R_SEKOFF	Memc[$1+2]	# encoded seek offset (5 chars)
define	R_DATA		(($1)+7)	# pointer to data text

define	NCHARS_INT	2		# nchars to encode an int
define	NCHARS_LONG	5		# nchars to encode a long
define	R_GETNCHARS	(($1)-7)	# reclen to nchars
define	R_GETRECLEN	(($1)+7)	# nchars to reclen

# KII instruction format.

define	LEN_INTFIELDS	16		# number of integer fields
define	FIRSTINTFIELD	p_opcode	# first integer field in common
define	MAX_ARGS	13		# max procedure arguments
define	SZ_SBUF		255		# size of string buffer
define	SZB_PACKET	320		# packet size, bytes

# KII opcodes.

define	KI_ENVINIT	1
define	KI_SETROOT	2
define	KI_ZOSCMD	3
define	KI_FMAPFN	4

define	KI_ZFACSS	10
define	KI_ZFALOC	11
define	KI_ZFCHDR	12
define	KI_ZFDELE	13
define	KI_ZFINFO	14
define	KI_ZFGCWD	15
define	KI_ZFMKCP	16
define	KI_ZFMKDR	17
define	KI_ZFPATH	18
define	KI_ZFPROT	19
define	KI_ZFRNAM	20
define	KI_ZFRMDR	21
define	KI_ZFSUBD	22
define	KI_ZDVALL	23
define	KI_ZDVOWN	24
define	KI_ZFUTIM	25

define	KI_ZOPDIR	30
define	KI_ZCLDIR	31
define	KI_ZGFDIR	32

define	KI_ZOPDPR	35
define	KI_ZCLDPR	36
define	KI_ZOPCPR	37
define	KI_ZCLCPR	38
define	KI_ZINTPR	39

# Device driver opcodes.  BF must be the lowest numbered binary file driver
# and TX must be the lowest number text file driver.

define	KI_ZFIOBF	40
define	KI_ZFIOLP	41
define	KI_ZFIOPL	42
define	KI_ZFIOPR	43
define	KI_ZFIOSF	44
define	KI_ZFIOGD	45

define	KI_ZFIOTX	50
define	KI_ZFIOTY	51

define	KI_ZFIOMT	55

# KII subcodes.

define	BF_OPN		1		# binary files (BF, SF, PR, PL, etc.)
define	BF_CLS		2
define	BF_ARD		3
define	BF_AWR		4
define	BF_AWT		5
define	BF_STT		6

define	TX_OPN		1		# text files (TX, TY)
define	TX_CLS		2
define	TX_GET		3
define	TX_PUT		4
define	TX_FLS		5
define	TX_SEK		6
define	TX_NOT		7
define	TX_STT		8

define	MT_OP		1		# magtape zz-routines
define	MT_CL		2
define	MT_RD		3
define	MT_WR		4
define	MT_WT		5
define	MT_ST		6
define	MT_RW		7
