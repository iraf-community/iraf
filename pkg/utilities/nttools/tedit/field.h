# FIELD.H -- Tedit field descriptor

define	TED_FLDLEN	11		# field descriptor length

define	TED_FSCREEN	Memi[$1]	# screen associated with field
define	TED_RDOFLD	Memi[$1+1]	# is this a read only field?
define	TED_PGSIZE	Memi[$1+2]	# number of rows on screen
define	TED_LSTROW	Memi[$1+3]	# last row in table
define	TED_LSTCOL	Memi[$1+4]	# last column in table
define	TED_NXTROW	Memi[$1+5]	# next row to edit
define	TED_NXTCOL	Memi[$1+6]	# next column to edit
define	TED_DIRECT	Memi[$1+7]	# direction of motion
define	TED_FINDEX	Memi[$1+8]	# column index
define	TED_MRKFLD	Memi[$1+9]	# has this field been changed?
define	TED_COMMAND	Memi[$1+10]	# has command key been pressed?

define	SZ_FIELD	512		# Maximum length of a single field

# The following are the legal alignments (used by align_field and move_field)

define	LEFT		1
define	CENTER		2
define	RIGHT		3
