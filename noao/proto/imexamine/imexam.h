# IMEXAM.H -- IMEXAMINE global definitions.

define	MAX_FRAMES	4			# max display frames

# IMEXAMINE data structure.
 
define	IE_LEN		230			# length of IE structure
define	IE_SZFNAME	99			# length of file name
define	IE_SZTITLE	512			# length of multiline title
 
define	IE_IM		Memi[$1]		# IMIO pointer
define	IE_DS		Memi[$1+1]		# display frame pointer
define	IE_GP		Memi[$1+2]		# GIO pointer
define	IE_PP		Memi[$1+3]		# pset pointer
define	IE_LIST		Memi[$1+4]		# image list
define	IE_LISTLEN	Memi[$1+5]		# number of images in list
define	IE_USEDISPLAY	Memi[$1+6]		# use image display?
define	IE_INDEX	Memi[$1+7]		# image index
define	IE_DFRAME	Memi[$1+8]		# frame used to display images
define	IE_MAPFRAME	Memi[$1+9]		# mapped display frame
define	IE_NEWFRAME	Memi[$1+10]		# new (current) display frame
define	IE_NFRAMES	Memi[$1+11]		# number of image frames
define	IE_ALLFRAMES	Memi[$1+12]		# use all frames for display?
define	IE_LOGFD	Memi[$1+13]		# log file descriptor
define	IE_MAGZERO	Memr[$1+14]		# magnitude zero point
define	IE_XORIGIN	Memr[$1+15]		# X origin
define	IE_YORIGIN	Memr[$1+16]		# Y origin
define	IE_GTYPE	Memi[$1+17]		# current graph type
define	IE_X1		Memr[$1+18]		# current graph x1
define	IE_X2		Memr[$1+19]		# current graph x2
define	IE_Y1		Memr[$1+20]		# current graph y1
define	IE_Y2		Memr[$1+21]		# current graph y2
define	IE_IX1		Memi[$1+22]		# image section coordinate
define	IE_IX2		Memi[$1+23]		# image section coordinate
define	IE_IY1		Memi[$1+24]		# image section coordinate
define	IE_IY2		Memi[$1+25]		# image section coordinate
define	IE_LASTKEY	Memi[$1+26]		# last type of keyed output
	# (available)
define	IE_IMAGE	Memc[P2C($1+30)]	# image name
define	IE_LOGFILE	Memc[P2C($1+130)]	# logfile name
