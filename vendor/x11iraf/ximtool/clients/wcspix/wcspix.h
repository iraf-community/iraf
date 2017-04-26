# WCSPIX.H -- Include file for the WCS/Pixel value ISM task

define	WCSPIX_NAME	"wcspix"
define	WCSPIX_MODE	"text"
define	WCSPIX_CONNECT	"unix:/tmp/.ISM%d"

define	WCSPIX_DBG	FALSE

# Main task data structures.
define  MAX_WCSLINES	4			# max WCS output lines
define	LEN_PIXTAB	81			# size of pixel table
define	LEN_WCSNAME	32			# size of a WCS name

define  SZ_WCSPIX       7
define  WP_CPTR         Memi[$1  ]              # object cache pointer
define  WP_PTABSZ       Memi[$1+1]              # pixel table size
define  WP_BPM       	Memi[$1+2]              # get BPM data
define  WP_SYSTEMS      Memi[$1+3]              # WCS readout systems
define  WP_WCS      	Memi[$1+4]              # WCS system string
define  WP_FORMATS      Memi[$1+5]              # WCS readout formats
define  WP_DBGLEVEL     Memi[$1+6]              # debug level

define  OBJCACHE   Memi[WP_CPTR($1)+$2]		# object cache
define  SYSTEMS    Memi[WP_SYSTEMS($1)+$2-1]
define  FORMATS    Memi[WP_FORMATS($1)+$2-1]
define  WCSNAME    Memc[WP_WCS($1)+(LEN_WCSNAME*($2-1))]


# Element of an object cache.
define  SZ_CACHE        256                     # size of object cache
define  SZ_CNODE        135                     # size of a cache node
define  SZ_OBJREF       128                     # size of a object reference

define  C_OBJID         Memi[$1]                # object id
define  C_REGID         Memi[$1+1]              # region id
define  C_CLASS         Memi[$1+2]              # object class
define  C_DATA          Memi[$1+3]              # object data ptr
define  C_NREF          Memi[$1+4]		# no. times object referenced
define  C_REF           Memc[P2C($1+6)]         # object reference file


# WCSPIX ISM task methods.
define	WCSPIX_CMDS	"|set|get|quit|initialize|cache|uncache\
			 |wcstran|wcslist|objinfo|debug"

define	SET		 1
define	GET		 2
define	QUIT		 3
define	INITIALIZE	 4
define	CACHE		 5
define	UNCACHE		 6
define	WCSTRAN		 7
define	WCSLIST		 8
define	OBJINFO	 	 9
define	DEBUG		10

# Parameters definable from the GUI
define	SZ_PARAM	32			# size of a parameter string

define WCSPIX_SYSTEMS  "|none|display|logical|physical|world|sky\
			|amplifier|ccd|detector|other|"
define  SYS_NONE	1			# no coords requested
define  SYS_DISPLAY	2			# image display coords
define  SYS_LOGICAL	3			# logical coords
define  SYS_PHYSICAL	4			# physical coords
define  SYS_WORLD	5			# world coords
define  SYS_SKY		6			# sky coords
define  SYS_AMP		7			# amplifier coords
define  SYS_CCD		8			# CCD coords
define  SYS_DETECTOR	9			# detector coords
define  SYS_OTHER	10			# ??? coords

define  SKYPROJ "FK5 FK4 ICRS GAPPT FK4-NO-E Ecliptic Galactic Supergalactic"


define  WCSPIX_PARAMS	"|psize|bpm|wcs|format|"
define  PAR_PSIZE	1			# pixel table size
define  PAR_BPM		2			# get BPM data
define  PAR_WCS		3			# WCS system
define  PAR_FMT		4			# WCS format

define	WCSPIX_FMT	"|default|hms|degrees|radians|"
define  FMT_DEFAULT	1			# no formatting
define  FMT_HMS		2			# covert to sexigesimal
define  FMT_DEG		3			# output degrees
define  FMT_RAD		4			# output radians

define  DEF_PTABSZ	0			# default pixtable size
define  DEF_FMT		FMT_DEFAULT		# default output format
define  DEF_SYSTEM	SYS_LOGICAL		# default coord system
define  DEF_BPM_FLAG	NO			# default get-BPM-data flag


# Object class definitions.
define	UNKNOWN_CLASS	1			# unknown class
define	IMAGE_CLASS	2			# generic image class
define	MEF_CLASS	3			# Mosaic MEF image class
define	MULTISPEC_CLASS	4			# multispec data class

# Class methods.
define	LEN_CLASS	6			# length of class table
define	MAX_CLASSES	16			# max supported classes
define	SZ_CLNAME	32			# size of a class name

define	CL_INIT		cl_table[1,$1]		# class initializer
define	CL_CACHE	cl_table[2,$1]		# cache the object
define	CL_UNCACHE	cl_table[3,$1]		# uncache the object
define	CL_WCSTRAN	cl_table[4,$1]		# WCS tranformations
define	CL_WCSLIST	cl_table[5,$1]		# list available WCS
define	CL_OBJINFO	cl_table[6,$1]		# get object header
define	CL_NAME		cl_names[1,$1]		# class name

