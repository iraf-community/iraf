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

# FI_PERM bits.

define	FI_ROWNER	1
define	FI_WOWNER	2
define	FI_RGROUP	3
define	FI_WGROUP	4
define	FI_RWORLD	5
define	FI_WWORLD	6
