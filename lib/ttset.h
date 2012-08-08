# TTSET.H -- Set/stat terminal driver options (VOS logical terminal driver).

define	TT_INITIALIZE	0		# initialize TT driver status
define	TT_KINCHAN	101		# kernel tty input channel
define	TT_KOUTCHAN	102		# kernel tty output channel
define	TT_LOGINCHAN	103		# login spoolfile channel
define	TT_LOGOUTCHAN	104		# logout spoolfile channel
define	TT_PBINCHAN	105		# playback spoolfile channel
define	TT_UCASEIN	106		# map input to lower case
define	TT_UCASEOUT	107		# map output to upper case
define	TT_SHIFTLOCK	108		# software shiftlock
define	TT_RAWMODE	109		# raw mode in effect
define	TT_LOGIO	110		# log terminal i/o
define	TT_LOGIN	111		# log terminal input
define	TT_LOGOUT	112		# log terminal output
define	TT_PLAYBACK	113		# take input from a spool file
define	TT_PBVERIFY	114		# pause at \n during playback
define	TT_PBDELAY	115		# msec delay/per record during playback
define	TT_PASSTHRU	116		# passthru mode (direct i/o to device)
define	TT_IOFILE	117		# logio file name
define	TT_INFILE	118		# login file name
define	TT_OUTFILE	119		# logout file name
define	TT_PBFILE	120		# playback file name
define	TT_TDEVICE	121		# terminal device at record time
define	TT_GDEVICE	122		# stdgraph device at record time
define	TT_FILTER	123		# input filter (e.g. for escapes)
define	TT_FILTERKEY	124		# input filter key character
