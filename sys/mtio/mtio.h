# Magtape Interface Definitions.  Note that the system config file
# contains additional definitions (i.e., MT_MAXTAPES, MT_SZBDEFIBUF,
# MT_SZBDEFOBUF).

define	LOCKLDIR	"uparm$"	# where the lock file goes
define	LOCKFILE	"mt"		# root lockfile name
define	LOCKEXTN	".lok"		# root lockfile name
define	MT_MAGIC	(-5417)		# was zopnmt called by mtopen?
define	SZ_DRIVE	31		# max length of drive name
define	SZ_OSDEV	31		# max length host device name
define	EOT		(-4)		# end-of-tape

# MTIO device descriptor structure.  The device descriptor is implemented
# as the two dimensional integer array MTDEV, defined in the mtio common.

define	MT_DRIVE	mtnam[1,$1+1]	# drive number
define	MT_OSDEV	mtosn[1,$1+1]	# drive number

define	MT_STRUCT	mtdev[1,$1+1]	# reference entire structure
define	MT_OSCHAN	mtdev[1,$1+1]	# OS channel or 0
define	MT_DENSITY	mtdev[2,$1+1]	# new density
define	MT_MAXBUFSIZE	mtdev[3,$1+1]	# max device transfer (record) size
define	MT_ACMODE	mtdev[4,$1+1]	# new access mode
define	MT_FILE		mtdev[5,$1+1]	# new file number
define	MT_RECORD	mtdev[6,$1+1]	# next record number
define	MT_NRECORDS	mtdev[7,$1+1]	# num. records read/written
define	MT_OLDFILE	mtdev[8,$1+1]	# file posn at open time
define	MT_OLDRECORD	mtdev[9,$1+1]	# record posn at open time
define	MT_ATEOF	mtdev[10,$1+1]	# set when hit EOF reading
define	MT_ATEOT	mtdev[11,$1+1]	# set when hit EOT on open
define	LEN_MTIODES	11		# length of above structure
