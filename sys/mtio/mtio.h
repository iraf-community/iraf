# MTIO.H --  Magtape i/o interface definitions.  Note that the system config
# file contains additional definitions (i.e., MT_MAXTAPES).

define	TAPECAP		"dev$tapecap"	# default tapecap file
define	LOCKLDIR	"tmp$"		# where the lock file goes
define	LOCKFILE	"mt"		# root lockfile name
define	LOCKEXTN	".lok"		# lockfile extension
define	MT_MAGIC	(-5417)		# was zopnmt called by mtopen?
define	SZ_DEVICE	79		# max length of drive name
define	SZ_IODEV	79		# max length host device name
define	SZ_LKNAME	79		# max length lock file mame
define	SZ_DEVCAP	512		# max command line tapecap chars

# MTIO device descriptor structure.  The device descriptor is implemented
# as the two dimensional integer array MTDEV, defined in the mtio common.
# The DEVPOS substructure must agree with the driver, os$zfiomt.c.

define	MT_DEVICE	mtnam[1,$1+1]	# drive name
define	MT_IODEV	mtosn[1,$1+1]	# i/o device
define	MT_LKNAME	mtlkn[1,$1+1]	# lock file name

define	LEN_MTIODES	11
define	MT_DEVPOS	MT_FILNO	# devpos struct (passed to driver)
define	LEN_DEVPOS	5

define	MT_OSCHAN	mtdev[1,$1+1]	# OS channel or 0
define	MT_ACMODE	mtdev[2,$1+1]	# new access mode
define	MT_DEVCAP	mtdev[3,$1+1]	# pointer to tapecap entry for device
define	MT_FILE		mtdev[4,$1+1]	# new file number
define	MT_RECORD	mtdev[5,$1+1]	# new record number
define	MT_ATEOF	mtdev[6,$1+1]	# reached end of file on a read
define	MT_FILNO	mtdev[7,$1+1]	# old file number at open
define	MT_RECNO	mtdev[8,$1+1]	# old record number at open
define	MT_NFILES	mtdev[9,$1+1]	# nfiles on tape
define	MT_TAPEUSED	mtdev[10,$1+1]	# total tape used, bytes
define	MT_PFLAGS	mtdev[11,$1+1]	# i/o flags returned by driver

# PFLAGS bitflags.
define	MF_ERR		001B		# i/o error in last operation
define	MF_EOF		002B		# tape mark seen in last operation
define	MF_EOT		004B		# end of tape seen in last op
define	MF_EOR		010B		# last op was a record advance
