# Defintion for dbtext structure.

define	DT_LEN		5

define	DT		Memi[$1]	# FIO channel
define	DT_NRECS	Memi[$1+1]	# Number of records
define	DT_OFFSETS	Memi[$1+2]	# Pointer to record offsets
define	DT_NAMES	Memi[$1+3]	# Pointer to name indices
define	DT_MAP		Memi[$1+4]	# Pointer to record names

define	DT_OFFSET	Meml[DT_OFFSETS($1)+$2-1]
define	DT_NAMEI	Memi[DT_NAMES($1)+$2-1]
define	DT_NAME		Memc[DT_MAP($1)+DT_NAMEI($1,$2)]

define	DT_ALLOC	20		# Allocation block size
