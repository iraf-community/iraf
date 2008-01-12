# CURSES.H -- Macros used by the curses subroutines

# Window which covers terminal screen

define	STDSCR		1

# The following string defines the set of commands read from the edcap file

define	CMDSTR "|MOVE_UP|MOVE_DOWN|MOVE_RIGHT|MOVE_LEFT|NEXT_WORD|PREV_WORD\
|NEXT_PAGE|PREV_PAGE|MOVE_START|MOVE_END|MOVE_BOL|MOVE_EOL|DEL_CHAR|DEL_LEFT\
|DEL_WORD|DEL_LINE|UNDEL_CHAR|UNDEL_WORD|UNDEL_LINE|GET_HELP|REPAINT\
|EXIT_UPDATE|"

# The following values are returned when 
# the corresponding escape sequence is entered

define	K_BASE		256		# Smallest value

define	K_UP		256		# Move up one row
define	K_DOWN		257		# Move down one row
define	K_RIGHT		258		# Move right one column
define	K_LEFT		259		# Move left one column
define	K_NEXTW		260		# Move forwards one word
define	K_PREVW		261		# Move backwards one word
define	K_NEXTP		262		# Move forwards one window
define	K_PREVP		263		# Move backwards one window
define	K_HOME		264		# Move to first row
define	K_END		265		# Move to last row
define	K_BOL		266		# Move to first column in row
define	K_EOL		267		# Move to last column in row
define	K_DEL		268		# Delete character underneath cursor
define	K_BS		269		# Delete character to left of cursor
define	K_DWORD		270		# Delete previous word
define	K_DLINE		271		# Delete entire line
define	K_UNDCHR	272		# Undelete character
define	K_UNDWRD	273		# Undelete word
define	K_UNDLIN	274		# Undelete line
define	K_HELP		275		# Display help window
define	K_PAINT		276		# Force window redraw
define	K_EXIT		277		# Exit procedure

# Codes used by winstat to retrieve window fields

define	W_TOP		1		# Window's top row                  
define	W_LEFT		2		# Window's leftmost column          
define	W_BOT		3		# Window's bottom row               
define	W_RIGHT		4		# Window's rightmost column         
define	W_CURROW	5		# Cursor row relative to window     
define	W_CURCOL	6		# Cursor column relative to window  
define	W_CLEAR		7		# Redraw window when refreshed      
define	W_LEAVE		8		# Leave cursor after redraw         
define	W_SCROLL	9		# Window will scroll                
define	W_HIDDEN	10		# Window is hidden                  
define	W_BOXED		11		# Window is boxed                   
define	W_ATRIB		12		# Character attribute of window

# Direction to move rectangle used by ps_slide and wslide

define	DIR_UP		0
define	DIR_DOWN	1
define	DIR_LEFT	2
define	DIR_RIGHT	3

# Character attributes 

define	A_NORM		0
define	A_STANDOUT	128

# Definition of rectangle

define	RSIZE		4
define	RTOP		$1[1]
define	RLEFT		$1[2]
define	RBOT		$1[3]
define	RRIGHT		$1[4]

# Macros used to manipulate rectangles

define	RWIDTH		($1[4] - $1[2] + 1)
define	RHEIGHT		($1[3] - $1[1] + 1)
define	RASG		{$1[1] = $2; $1[2] = $3; $1[3] = $4; $1[4] = $5}
define	RCPY		{$1[1]=$2[1]; $1[2]=$2[2]; $1[3]=$2[3]; $1[4]=$2[4]}

# Constant used to create a rectangle much greater than screen size

define	GIANT		9999
