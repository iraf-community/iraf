# GRC.H -- Global definitions and data structures for the RCURSOR (cursor read)
# procedures.

define	KEYSFILE	"lib$scr/cursor.key"
define	KEYSTROKES	"ABCDEFHJKLMPRTUVWXYZ<>0123456789:="
define	MAX_KEYS	128
define	LEN_RCSTRUCT	(10+(128/SZ_STRUCT))

define	RC_CASE		Memi[$1]		# case sensitive
define	RC_MARKCUR	Memi[$1+1]		# mark cursor
define	RC_PHYSOPEN	Memi[$1+2]		# physical open by rcursor
define	RC_AXES		Memi[$1+3]		# draw axes if screen redrawn
			# (open)
define	RC_KEYS		Memc[P2C($1+10)+$2]	# keystroke mappings

define	LEN_CT		2,4
define	CT_TRAN		1
define	CT_SCALE	2
define	CT_WORIGIN	3
define	CT_MORIGIN	4
