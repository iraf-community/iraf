# FSET.H -- FSET/FSTATUS parameters (r = read_only, * = internal to FIO).
# Some of these parameters provide access to the guts of the i/o system and
# should not be used by packages outside FIO, to avoid a dependence on the
# inner workings of FIO.  Parameters affecting the file buffer number, types,
# or sizes are read-only after the first i/o to the file.

define	F_ADVICE	1	#  advice on type of access (rand,seq,def)
define	F_ASYNC		2	#  enable asynchronous i/o [y/n]
define	F_BLKSIZE	3	#r device block size, chars
define	F_BUFPTR	4	#* install externally created file buffer
define	F_BUFSIZE	5	#  file buffer size, chars
define	F_BUFTOP	6	#* set pointer to top of buffer
define	F_BUFTYPE	7	#  file buffer type (F_LOCAL or F_GLOBAL)
define	F_CANCEL	8	#  cancel buffered data
define	F_CHANNEL	9	#r channel number
define	F_CLOBBER	10	#  is file clobber enabled [y/n]
define	F_CLOSEFD	11	#  close host channel when inactive
define	F_DEVCODE	12	#* device driver code (index in device table)
define	F_DEVICE	13	#* entry point address device read/get routine
define	F_EOF		14      #r is file positioned at EOF [y/n]
define	F_FFIOMODE	15	#r is i/o in progress on channel
define	F_FILENAME	16	#r get file name (fstats)
define	F_FILESIZE	17	#r get file size (fstatl)
define	F_FILEWAIT	18	#  is file wait on open enabled [y/n]
define	F_FIODES	19	#* struct pointer to file descriptor structure
define	F_FIRSTBUFOFF	20	#  file offset of first FIO buffer (default=1)
define	F_FLUSHNL	21	#  is flush on newline enabled [y/n]
define	F_IOMODE	22	#  raw (vs "cooked") mode for terminal i/o
define	F_KEEP		23	#  keep file after task completion?
define	F_LASTREFFILE	24	#r get FD of last referenced (active) file
define	F_MAXBUFSIZE	25	#r maximum file buffer size
define	F_MODE		26	#r file access mode (ro,wo,rw)
define	F_NBUFS		27	#  number of file buffers
define	F_NCHARS	28	#r nchars last transfer
define	F_ONEVERSION	29	#  keep only one version of a file
define	F_OPEN		30	#r is file open
define	F_OPTBUFSIZE	31	#r optimal buffer size for device (chars)
define	F_PBBSIZE	32	#  push back buffer size, chars
define	F_RAW		33	#r set/stat raw mode (see F_IOMODE)
define	F_READ		34	#r does file have read access [y/n]
define	F_REDIR		35	#r i/o is redirected
define	F_SETREDRAW	36	#w set/enable screen redraw code (suspend proc)
define	F_SZBBLK	37	#r size in bytes of last dev block read|written
define	F_TYPE		38	#r file type (text, binary)
define	F_UNREAD	39	#r number of unread chars in FIO buffer
define	F_VALIDATE	40	#  validate FIO buffer contents (fseti)
define	F_WRITE		41	#r does file have write access [y/n]

define	F_LOCAL		1	# allocate local file buffers
define	F_GLOBAL	2	# take file buffers from global pool
define	F_GETPROT	2	# is file protected?
define	F_FFIOINACT	0	# no i/o in progress
define	F_FFIOREAD	1	# read in progress
define	F_FFIOWRITE	2	# write in progress

# Terminal mode stuff.  I/O mode flags maybe combined, e.g., IO_RAW+IO_NDELAY.
define	IO_NORMAL	0		# "normal" terminal i/o
define	IO_RAW		001B		# enables raw mode i/o
define	IO_NDELAY	100B		# enables nonblocking i/o
