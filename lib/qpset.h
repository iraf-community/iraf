# QPSET.H -- User accessible definitions for the QPOE package.

define	SZ_COMMENT		79	# max size comment string
define	SZ_DATATYPE		79	# max size datatype name string

# QPOE Read-Write Parameters.
define	QPOE_BLOCKFACTOR	1	# blocking factor for pixel arrays
define	QPOE_BUCKETLEN		2	# event list bucket length, nevents
define	QPOE_CACHESIZE		3	# lfile (buffer) cache size, nlfiles
define	QPOE_DATABUFLEN		4	# QPEX data buffer length, chars
define	QPOE_DEBUGLEVEL		5	# debug level (0 = no messages)
define	QPOE_DEFLUTLEN		6	# default lookup table length (bins)
define	QPOE_INDEXLEN		7	# symbol table hash index length
define	QPOE_LUTMINRANGES	8	# min ranges before using LUT
define	QPOE_LUTSCALE		9	# scale nranges to LUT bins
define	QPOE_MAXFRLUTLEN	10	# max full-res LUT length
define	QPOE_MAXLFILES		11	# max lfiles in datafile
define	QPOE_MAXPTPAGES		   25	# max lfiles in datafile
define	QPOE_MAXPUSHBACK	12	# max amount of pushed back macro data
define	QPOE_MAXRRLUTLEN	13	# max reduced-res LUT length
define	QPOE_OPTBUFSIZE		14	# optimum buffer size for IMIO/QPF/FIO
define	QPOE_PAGESIZE		15	# page size of datafile, bytes
define	QPOE_PROGBUFLEN		16	# QPEX program buffer length, ints
define	QPOE_SBUFSIZE		17	# symtab string buf size, chars (init)
define	QPOE_STABLEN		18	# symtab data area size, su (init)
define	QPOE_NODEFFILT		19	# disable use of default filter
define	QPOE_NODEFMASK		20	# disable use of default mask

# QPOE Read-Only Parameters.
define	QPOE_FM			21	# FMIO descriptor
define	QPOE_MODE		22	# poefile access mode
define	QPOE_ST			23	# SYMTAB symbol table descriptor
define	QPOE_VERSION		24	# QPOE version number

# Parameter flags (for qp_addf).
define	QPF_NONE		(-1)	# no flags (0 gives default flags)
define	QPF_INHERIT		0002B	# copy parameter in a NEW_COPY open
