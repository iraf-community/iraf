# FIO.H -- FIO definitions.

# Logical seek: adjust i/o pointer to the seek offset.  If the buffer was
# being written into when the seek occurs, adjust ITOP and OTOP to mark
# the end of the good data in the buffer, then set IOP to new offset.

define	(UPDATE_IOP,	if (bufptr[$1] != NULL && otop[$1] == buftop[$1]) {
			    itop[$1] = min(buftop[$1], max(itop[$1], iop[$1]))
			    otop[$1] = itop[$1]
			})
define	(LSEEK,		{UPDATE_IOP($1);iop[$1]=($2-boffset[$1]+bufptr[$1])})
define	LNOTE		(boffset[$1]+iop[$1]-bufptr[$1])
define	BUF_MODIFIED	(otop[$1] > bufptr[$1])

define	INACTIVE		0
define	READ_IN_PROGRESS	1
define	WRITE_IN_PROGRESS	2


# File descriptor structure (dynamically allocated part)

define	SZ_FFNAME	255
define	LEN_FIODES	(20+LEN_CHANDES+256)

define	FCHAN		Memi[$1]		# os channnel
define	FMODE		Memi[$1+1]		# mode of access
define	FTYPE		Memi[$1+2]		# binary or text
define	FDEV		Memi[$1+3]		# device index
define	FBUFSIZE	Memi[$1+4]		# buffer size
define	FIRSTBUFOFF	Memi[$1+5]		# offset of first file buffer
define	FNBUFS		Memi[$1+6]		# number of buffers
define	FLOCBUF		Memi[$1+7]		# zlocva of aread buffer
define	FPBBUFSIZE	Memi[$1+8]		# size pushback buffer
define	FPBBUF		Memi[$1+9]		# ptr to pushback buffer
define	FPBTOP		Memi[$1+10]		# ptr to top of pbbuf
define	FPBIOP		Memi[$1+11]		# iop into pbbuf
define	FPBSP		Memi[$1+12]		# pbbuf stack pointer 
define	FILSTAT		Memi[$1+13]		# channel status
define	FNCHARS		Memi[$1+14]		# nchars last i/o
define	FNBYTES		Memi[$1+15]		# nbytes last rec read
define	FBUFMODE	Memi[$1+16]		# i/o mode for buffer
define	FFIOMODE	Memi[$1+17]		# i/o mode for file
define	FCD		Memi[$1+18]		# ptr to chan descr.
			# (open)
define	FLCD		($1+20)				# local storage for cd
define	FNAME		Memc[P2C($1+20+LEN_CHANDES)]	# filename

# Channel descriptor (stored in fd if file not multiply open).  The DEVPAR
# (device parameter) fields are reserved for use with special devices and
# are not used by FIO.

define	LEN_CHANDES	(10+256)
define	FREFCNT		Memi[FCD($1)]		# chan reference count
define	FCIOMODE	Memi[FCD($1)+1]		# chan i/o mode
define	FCLOSEFD	Memi[FCD($1)+2]		# close chan when inactive
define	FAFD		Memi[FCD($1)+3]		# active fd
define	FBLKSIZE	Memi[FCD($1)+4]		# device block size
define	FOPTBUFSIZE	Memi[FCD($1)+5]		# "optimum" buffer size
define	FMAXBUFSIZE	Memi[FCD($1)+6]		# maximum buffer size
define	FDEVOPEN	Memi[FCD($1)+7]		# device zopen proc
define	FILSZ_PTR	FCD($1)+8
define	FILSIZE		Meml[FILSZ_PTR($1)]	# file size, chars
define	FPKOSFN		Memc[P2C(FCD($1)+10)]	# packed osfn of file


# Flags

define	FF_FLUSHNL	1B		# flush each line to output device
define	FF_READ		2B		# read perm on file
define	FF_WRITE	4B		# write perm on file
define	FF_EOF		10B		# at EOF
define	FF_ERR		20B		# i/o error
define	FF_KEEP		40B		# keep file open after task quits?
define	FF_FLUSH	100B		# write each line to z buffer
define	FF_RAW		200B		# raw i/o when reading a text device
define	FF_NDELAY	400B		# nonblocking i/o
define	FF_PUSHBACK	1000B		# data is pushed back into input


# Device table entry points

define	LEN_DTE		7		# length of device table entry
define	TX_DRIVER	1		# index into devtbl of textfile driver
define	BF_DRIVER	8		# index of binary file driver
define	TY_DRIVER	15		# index of terminal driver
define	PR_DRIVER	22		# index of IPC driver
define	SF_DRIVER	29		# index of static file driver
define	STD_DRIVER	($1 <= 29)	# is device code that of a std driver?

define	ZGETTX		zdev[FDEV($1)]		# text files
define	ZPUTTX		zdev[FDEV($1)+1]
define	ZFLSTX		zdev[FDEV($1)+2]
define	ZSTTTX		zdev[FDEV($1)+3]
define	ZCLSTX		zdev[FDEV($1)+4]
define	ZSEKTX		zdev[FDEV($1)+5]
define	ZNOTTX		zdev[FDEV($1)+6]

define	ZARDBF		zdev[FDEV($1)]		# binary files
define	ZAWRBF		zdev[FDEV($1)+1]
define	ZAWTBF		zdev[FDEV($1)+2]
define	ZSTTBF		zdev[FDEV($1)+3]
define	ZCLSBF		zdev[FDEV($1)+4]


# File status codes (ZFSTTX, ZFSTTB).  FIO makes a distinction between the
# device block size, which establishes the alignment restrictions for
# asynchronous reads and writes, and the "optimal" buffer size, the default
# buffer size supplied by the device z-routines, which defines the minimum
# buffer size for efficient sequential access to the device.

define	FSTT_BLKSIZE	1		# block size, bytes
define	FSTT_FILSIZE	2		# file size, bytes
define	FSTT_OPTBUFSIZE	3		# optimum buffer size, bytes
define	FSTT_MAXBUFSIZE	4		# optimum buffer size, bytes

define	REMOVE_PROTECTION	0	# for ZFPROT
define	SET_PROTECTION		1
define	QUERY_PROTECTION	2

define	STRING_FILE	(-1)		# open a string as a file
define	SZ_SPOOLBUF	4096		# def. initial size of a spool buffer


# Filename Mapping definitions.

define	SZ_VFNFN	127		# max size ROOT or EXTN in VFN
define	SZ_OSDIR	255		# max chars in V_OSDIR field

define	VFN_READ	1		# VFN access modes for VFNOPEN
define	VFN_WRITE	2
define	VFN_UNMAP	3

define	VFN_NOUPDATE	0		# update flag for VFNCLOSE
define	VFN_UPDATE	1


# Terminal driver escape sequences.

define	LEN_RAWCMD	5		# +1 for iomode character (N|B)
define	RAWOFF		"\033-rAw"	# raw mode off
define	RAWON		"\033+rAw"	# raw mode on

define	LEN_SETREDRAW	6		# 5 char escape sequence + code
define	SETREDRAW	"\033=rDw"	# set/enable screen redraw code


# Magtape driver global definitions.

define	LEN_MTDEVPOS	5
