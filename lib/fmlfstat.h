# FMLFSTAT.H -- Lfile status structure definitions.

# Lfstat structure.
define	LEN_LFSTAT	2		# struct size
define	LFU_SIZE	$1[1]		# lfile size, bytes
define	LFU_FLAGS	$1[2]		# lfile flag bits

# Flag bits.
define	LFB_DELETED	1B		# delete bit
define	LFB_TEXTFILE	2B		# file contains packed text
