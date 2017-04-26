# SCREEN.H -- Tedit screen descriptor

define	MAXSCR		10		# maximum number of screens

define	TED_SCRLEN	10		# screen descriptor length

define	TED_WINDOW	Memi[$1]	# window descriptor
define	TED_TABLE	Memi[$1+1]	# table descriptor (or NULL)
define	TED_PASTE	Memi[$1+2]	# paste table descriptor (or NULL)
define	TED_LOROW	Memi[$1+3]	# lowest row on screen
define	TED_HIROW	Memi[$1+4]	# highest row on screen
define	TED_LOCOL	Memi[$1+5]	# lowest column on screen
define	TED_HICOL	Memi[$1+6]	# highest column on the screen
define	TED_CURROW	Memi[$1+7]	# current row on the screen
define	TED_CURCOL	Memi[$1+8]	# current column on the screen
define	TED_SCRIDX	Memi[$1+9]	# current character within the field


