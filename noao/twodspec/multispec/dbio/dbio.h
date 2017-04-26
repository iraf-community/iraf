
# Definitions for subset DBIO

define	SZ_DB_KEY	79		# Size of database reference keys
define	MAX_DB_DES	10		# Maximum number of DBIO descriptors
define	DB_ERRCODE	1000		# Start of DBIO error codes

# DBIO descriptor

define	LEN_DB_DES	3

define	DB_FD		Memi[$1]	# The database FIO descriptor
define	DB_DIC		Memi[$1+1]	# Pointer to the dictionary memory
define	DB_UPDATE	Memi[$1+2]	# Has dictionary been change [y/n]

# DBIO dictionary entry.  Each entry is referenced with the pointer to the
# dictionary memory ($1) and the entry number ($2).

define	LEN_DB_ENTRY	43

define	DB_KEY		Memi[$1+($2-1)*LEN_DB_ENTRY]	# Key
define	DB_OFFSET	Meml[$1+($2-1)*LEN_DB_ENTRY+40]	# File Offset
define	DB_SZ_ELEM	Memi[$1+($2-1)*LEN_DB_ENTRY+41]	# Element size
define	DB_DIM		Memi[$1+($2-1)*LEN_DB_ENTRY+42]	# Number of elements
