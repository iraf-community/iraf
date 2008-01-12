# SCREEN.H -- Tedit screen descriptor

define	MAXSCR		10		# maximum number of screens

define	TED_SCRLEN	10		# screen descriptor length

define	TED_WINDOW	Memi[P2I($1)]	# window descriptor
define	TED_TABLE	Memi[P2I($1+1)]	# table descriptor (or NULL)
define	TED_PASTE	Memi[P2I($1+2)]	# paste table descriptor (or NULL)
define	TED_LOROW	Memi[P2I($1+3)]	# lowest row on screen
define	TED_HIROW	Memi[P2I($1+4)]	# highest row on screen
define	TED_LOCOL	Memi[P2I($1+5)]	# lowest column on screen
define	TED_HICOL	Memi[P2I($1+6)]	# highest column on the screen
define	TED_CURROW	Memi[P2I($1+7)]	# current row on the screen
define	TED_CURCOL	Memi[P2I($1+8)]	# current column on the screen
define	TED_SCRIDX	Memi[P2I($1+9)]	# current character within the field


