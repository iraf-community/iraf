# QPSET.H -- User accessible definitions for the QPOE package.

define	SZ_COMMENT		256	# max size comment string
define	SZ_DATATYPE		256	# max size datatype name string

# QPOE Read-Write Parameters.
define	QPOE_BLOCKFACTOR	1	# blocking factor for pixel arrays
define	QPOE_XBLOCKFACTOR	2	# X blocking factor for pixel arrays
define	QPOE_YBLOCKFACTOR	3	# Y blocking factor for pixel arrays
define	QPOE_BUCKETLEN		4	# event list bucket length, nevents
define	QPOE_CACHESIZE		5	# lfile (buffer) cache size, nlfiles
define	QPOE_DATABUFLEN		6	# QPEX data buffer length, chars
define	QPOE_DEBUGLEVEL		7	# debug level (0 = no messages)
define	QPOE_DEFLUTLEN		8	# default lookup table length (bins)
define	QPOE_INDEXLEN		9	# symbol table hash index length
define	QPOE_LUTMINRANGES	10	# min ranges before using LUT
define	QPOE_LUTSCALE		11	# scale nranges to LUT bins
define	QPOE_MAXFRLUTLEN	12	# max full-res LUT length
define	QPOE_MAXLFILES		13	# max lfiles in datafile
define	QPOE_MAXPTPAGES		14	# max lfiles in datafile
define	QPOE_MAXPUSHBACK	15	# max amount of pushed back macro data
define	QPOE_MAXRRLUTLEN	16	# max reduced-res LUT length
define	QPOE_OPTBUFSIZE		17	# optimum buffer size for IMIO/QPF/FIO
define	QPOE_PAGESIZE		18	# page size of datafile, bytes
define	QPOE_PROGBUFLEN		19	# QPEX program buffer length, ints
define	QPOE_SBUFSIZE		20	# symtab string buf size, chars (init)
define	QPOE_STABLEN		21	# symtab data area size, su (init)
define	QPOE_NODEFFILT		22	# disable use of default filter
define	QPOE_NODEFMASK		23	# disable use of default mask

# QPOE Read-Only Parameters.
define	QPOE_FM			24	# FMIO descriptor
define	QPOE_MODE		25	# poefile access mode
define	QPOE_ST			26	# SYMTAB symbol table descriptor
define	QPOE_VERSION		27	# QPOE version number

# Parameter flags (for qp_addf).
define	QPF_NONE		(-1)	# no flags (0 gives default flags)
define	QPF_INHERIT		0002B	# copy parameter in a NEW_COPY open
