# Definitions for the Model 75 UNIX/IIS device driver.

define	IIS_READ	1		# read function code
define	IIS_WRITE	0		# write function code
define	IIS_INACTIVE	2		# no i/o in progress

# Function control block structure containing only our own internal variables.

define	LEN_FCB		20
define	FCB_CHAN	Memi[($1)]		# os channel
define	FCB_STATUS	Mems[P2S(($1)+1)]	# channel status (r, w, err)
define	FCB_NBYTES	Mems[P2S(($1)+2)]	# nbytes last transfer
define	FCB_STATE	Mems[P2S(($1)+3)]	# instruction processing state
define	FCB_IISHDR	Mems[P2S(($1)+4)]	# m70 header of current instr.
			# (extra space)

# Instruction processing states

define	READY		0		# ready for new instruction
define	DATA_READ	1		# read data to complete instruction
define	DATA_WRITE	2		# write data to complete instruction

# IIS device status words.

define	IIS_FILSIZE		(512 * 512 * SZB_CHAR)
define	IIS_BLKSIZE		1024
define	IIS_OPTBUFSIZE		(512 * SZB_CHAR)
define	IIS_MAXBUFSIZE		16384
