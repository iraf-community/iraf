# Definitions for the VMS/IIS device driver.

define	IIS_READ	1		# read function code
define	IIS_WRITE	0		# write function code
define	IIS_INACTIVE	2		# no i/o in progress

define	EFN		EFN2		# EFN to use for i/o
define	EFN1W		0		# efn #1, wait for completion
define	EFN2		1		# efn #2, no wait for completion
define	EFN3		2		# efn #3, no wait for completion

# Function control block structure for IIS.  The first part of the structure
# is filled in by VMS at open time; all we need to know is the offset of the
# device name.  We use the latter part of the buffer for our own internal
# variables.

define	LEN_FCB		28
define	FCB_U_NAME	Mems[($1)+16+($2)-1]
define	FCB_IOSB	Mems[($1)+20+($2)-1]
define	FCB_KCHAN	Mems[($1)+24]	# NULL if on local node, else remote
define	FCB_STATUS	Mems[($1)+25]	# channel status (r, w, err)
define	FCB_NBYTES	Mems[($1)+26]	# nbytes last transfer
define	FCB_EFN		Mems[($1)+27]	# event flag used for transfer

# IIS device status words.

define	IIS_FILSIZE		(512 * 512 * SZB_CHAR)
define	IIS_BLKSIZE		1024
define	IIS_OPTBUFSIZE		(512 * SZB_CHAR)
define	IIS_MAXBUFSIZE		32768
