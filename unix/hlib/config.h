# System configuration parameters.  Sizes are in SPP chars unless otherwise
# specified.

define	FIRST_FD	10		# first open file descriptor
define	LAST_FD		4096		# number of file descriptors
define	PSIOCTRL	9		# the last pseudofile (see etc$prpsio.x)
define	FBUF_ALLOC	vmalloc		# call to allocate file buffer
define	LEN_DEVTBL	150		# FIO device table (7 cells/device)
define	LEN_RANDBUF	8		# bufsize = LEN_RANDBUF * blksize
define	LEN_SEQBUF	8		# bufsize = LEN_SEQBUF * optbufsize
define	SZ_STDIOBUF	1024		# size of STDIN/STDOUT buffers
define	SZ_PBBUF	1024		# default size of FIO pushback buf

define	SZ_MEM		1		# size of Mem common
define	SZ_MEMALIGN	SZ_DOUBLE	# alignment criteria for malloc
define	SZ_PHYSMEM	750000		# max phys memory available to a task
define	SZ_STACK	8192		# size of a stack segment (salloc)
define	SZ_STKHDR	(4*SZ_POINTER)	# size of stack segment header
define	SZ_VMEMALIGN	SZ_VMPAGE	# alignment criterium for vmalloc
define	VMEM_BASE	0		# fwa to align with, vmalloc
define	SZ_WORKSET	100000		# tasks normal working set size
define	LEN_JUMPBUF	1024		# buffer for ZSVJMP
define	JUMPCOM		zjucom		# IRAF Main ZDOJMP common

define	MAX_ONEXIT	10		# max onexit procedures
define	MAX_ONERROR	10		# max onerror procedures
define	MAX_CLGFILPAR	10		# max open params for CLGFIL
define	MAX_CHILDPROCS	10		# max connected subprocesses
define	MAX_BKGJOBS	10		# max detached processes

define	IM_FALLOC	YES		# "falloc" pixel storage file (IMIO)?
define	IM_PACKDENSITY	0.6		# minimum storage efficiency for images

define	MT_MAXTAPES	2		# maximum open tape drives
define	MT_SZBDEFIBUF	65535		# def. input buffer size (bytes)
define	MT_SZBDEFOBUF	8192		# def. output buffer size (bytes)

# File Locking.

define	OS_FILELOCKING	false		# OS provides file locking
define	FILELOCK_PERIOD	120		# minimum lifetime of a file lock, secs
define	MIN_TIMELEFT	60		# rollback if less time left on lock

# Characteristics of host OS filenames.

define	CASE_INSENSITIVE	false	# is case ignored in OS filenames
define	HOST_CASE		'L'	# case used [UL] if case insensitive
define	UNDERSCORE_PERMITTED	true	# is _ permitted in filenames
define	PERIOD_PERMITTED	true	# is . permitted in root
define	MAX_ROOTLEN		128	# max chars in OS root filename
define	MAX_EXTNLEN		32	# max chars in OS filename extension
define	EXTN_DELIMITER		'.'	# character preceding extension
define	LEADING_ALPHA_ONLY	false	# first char must be a letter
define	ONECASE_OUT		false	# output filenames in host case

# IRAF vs OS filename extensions.

define	EXTN_MAP		""
define	RESERVED_EXTNS		"|zsf|zvf|zl1|zl2|zmd|zlk|"

# Escape sequence encoding metacharacters.

define	VFN_ESCAPE_CHAR		'\1'	# escape character for encoding
define	SHIFT_NEXTCHAR		'0'	# shift next char to upper case
define	SHIFT_TO_LOWER		'1'	# shift to lower case
define	SHIFT_TO_UPPER		'2'	# shift to upper case
define	UNDERSCORE_CODE		'3'	# code for encoding _
define	PERIOD_CODE		'4'	# code for encoding .

# Reserved filenames and filename extensions.

define	SETENV_FILE		"zzsetenv.def"
define	FNMAPPING_FILE		"zzfnmap.zvf"
define	SUBFILE_EXTN		".zsf"
define	FNMAPFILE_EXTN		".zvf"
define	TIMELOCK1_EXTN		".zl1"
define	TIMELOCK2_EXTN		".zl2"
define	DEGENFLAG_EXTN		".zmd"
define	LOCKFILE_EXTN		".zlk"
