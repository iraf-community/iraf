# KEYSELECT.H -- Global constants used by keyselect

#* HISTORY *
#* B.Simon	12-Mar-92	Original

define	SZ_STRCOL	19
define	SZ_BIGCOL	63

define	ERR_SYNTAX	1
define	ERR_NOFIND	2

define	ASSIGN_CHAR	'='
define	CONCAT_CHAR	':'
define	SEP_CHAR	','

define	IS_SEP		($1 <= ' ' || $1 == ',')

