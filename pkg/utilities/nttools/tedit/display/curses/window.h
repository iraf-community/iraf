# WINDOW.H -- Window structure definitions and macros

define	MAXWIN		50		# Maximum number of windows
define	LEN_WINSTRUCT	15		# Length of window structure

# definition of window structure

define	WIN_TOP		Memi[$1]	# Window's top row                  
define	WIN_LEFT	Memi[$1+1]	# Window's leftmost column          
define	WIN_BOT		Memi[$1+2]	# Window's bottom row               
define	WIN_RIGHT	Memi[$1+3]	# Window's rightmost column         
define	WIN_CURROW	Memi[$1+4]	# Cursor row relative to window     
define	WIN_CURCOL	Memi[$1+5]	# Cursor column relative to window  
define	WIN_CLEAR	Memi[$1+6]	# Redraw window when refreshed      
define	WIN_LEAVE	Memi[$1+7]	# Leave cursor after redraw         
define	WIN_SCROLL	Memi[$1+8]	# Window will scroll                
define	WIN_HIDDEN	Memi[$1+9]	# Window is hidden                  
define	WIN_BOXED	Memi[$1+10]	# Window is boxed                   
define	WIN_ATRIB	Memi[$1+11]	# Character attribute of window     
define	WIN_BUFFER	Memi[$1+12]	# Holds characters under the window
define	WIN_FUNC	Memi[$1+13]	# Function bound to window
define	WIN_DATA	Memi[$1+14]	# Data structure bound to window

# Macros used to manipulate rectangle

define	WIN_RECT	Memi[$1]
define	WIN_WIDTH	(WIN_RIGHT($1) - WIN_LEFT($1) + 1)
define	WIN_HEIGHT	(WIN_BOT($1) - WIN_TOP($1) + 1)
