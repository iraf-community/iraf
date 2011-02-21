# IMEXAM.H -- IMEXAMINE global definitions.

define	MAX_FRAMES	16			# max display frames

# IMEXAMINE data structure.
 
define	IE_LEN		370			# length of IE structure
define	IE_SZFNAME	99			# length of file name
define	IE_SZFORMAT	9			# length of format strings
define	IE_SZTITLE	512			# length of multiline title
 
define	IE_IM		Memi[$1]		# IMIO pointer
define	IE_MW		Memi[$1+1]		# MWCS pointer
define	IE_CTLW		Memi[$1+2]		# CT-MWCS pointer (L -> W)
define	IE_CTWL		Memi[$1+3]		# CT-MWCS pointer (W -> L)
define	IE_DS		Memi[$1+4]		# display frame pointer
define	IE_GP		Memi[$1+5]		# GIO pointer
define	IE_PP		Memi[$1+6]		# pset pointer
define	IE_LIST		Memi[$1+7]		# image list
define	IE_LISTLEN	Memi[$1+8]		# number of images in list
define	IE_USEDISPLAY	Memi[$1+9]		# use image display?
define	IE_INDEX	Memi[$1+10]		# image index
define	IE_DFRAME	Memi[$1+11]		# frame used to display images
define	IE_MAPFRAME	Memi[$1+12]		# mapped display frame
define	IE_NEWFRAME	Memi[$1+13]		# new (current) display frame
define	IE_NFRAMES	Memi[$1+14]		# number of image frames
define	IE_ALLFRAMES	Memi[$1+15]		# use all frames for display?
define	IE_LOGFD	Memi[$1+16]		# log file descriptor
define	IE_MAGZERO	Memr[P2R($1+17)]	# magnitude zero point
define	IE_XORIGIN	Memr[P2R($1+18)]	# X origin
define	IE_YORIGIN	Memr[P2R($1+19)]	# Y origin
define	IE_GTYPE	Memi[$1+20]		# current graph type
define	IE_X1		Memr[P2R($1+21)]	# current graph x1
define	IE_X2		Memr[P2R($1+22)]	# current graph x2
define	IE_Y1		Memr[P2R($1+23)]	# current graph y1
define	IE_Y2		Memr[P2R($1+24)]	# current graph y2
define	IE_IX1		Memi[$1+25]		# image section coordinate
define	IE_IX2		Memi[$1+26]		# image section coordinate
define	IE_IY1		Memi[$1+27]		# image section coordinate
define	IE_IY2		Memi[$1+28]		# image section coordinate
define	IE_P1		Memi[$1+29]		# Physical axis for logical x
define	IE_P2		Memi[$1+30]		# Physical axis for logical y
define	IE_IN		Memr[P2R($1+31)+$2-1]	# Input coordinate vector
define	IE_OUT		Memr[P2R($1+38)+$2-1]	# Output coordinate vector
define	IE_WCSDIM	Memi[$1+45]		# WCS dimension
define	IE_LASTKEY	Memi[$1+46]		# last type of keyed output
	# (available)
define	IE_IMAGE	Memc[P2C($1+50)]	# full image name
define	IE_IMNAME	Memc[P2C($1+100)]	# short image name for labels
define	IE_LOGFILE	Memc[P2C($1+150)]	# logfile name
define	IE_WCSNAME	Memc[P2C($1+200)]	# WCS name
define	IE_XLABEL	Memc[P2C($1+250)]	# WCS label
define	IE_YLABEL	Memc[P2C($1+300)]	# WCS label
define	IE_XFORMAT	Memc[P2C($1+350)]	# WCS format
define	IE_YFORMAT	Memc[P2C($1+360)]	# WCS format
