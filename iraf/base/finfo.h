# FINFO.H -- FINFO definitions (info on directory entries).

define	LEN_FINFO	(6+16)		# long finfo[LEN_FINFO]
define	FI_NINTFIELDS	6		# number of integer fields in struct
define	FI_SZOWNER	15		# char ownerid[FI_SZOWNER]

define	FI_TYPE		$1[1]		# file type (see below)
define	FI_SIZE		$1[2]		# file size, chars
define	FI_ATIME	$1[3]		# time of last access
define	FI_MTIME	$1[4]		# time of last modify
define	FI_CTIME	$1[5]		# time of file creation
define	FI_PERM		$1[6]		# permissions (owner,group,world)
define	FI_OWNER	$1[7]		# login name of file owner

# File types.
define	FI_REGULAR	1		# regular text file
define	FI_DIRECTORY	2		# directory file
define	FI_EXEC		3		# executable image
define	FI_SPECIAL	4		# terminals etc.

# FI_PERM bit assignments.
define	FI_ROWNER	1		# read perm for owner
define	FI_WOWNER	2		# write perm for owner
define	FI_RGROUP	3		# read perm for group
define	FI_WGROUP	4		# write perm for group
define	FI_RWORLD	5		# read perm for world
define	FI_WWORLD	6		# write perm for world
define	FI_RDLOCK	13		# temporary read lock in place
define	FI_WRLOCK	14		# temporary write lock in place

# FI_PERM bit masks.
define	FF_ROWNER	000001B		# same as above
define	FF_WOWNER	000002B
define	FF_RGROUP	000004B
define	FF_WGROUP	000010B
define	FF_RWORLD	000020B
define	FF_WWORLD	000040B
define	FF_RDLOCK	010000B
define	FF_WRLOCK	020000B
