# Private definitions file for the catalog query interface.


# Miscellaneous definitions mostly concerning buffer sizes.

#define CQ_SZ_LINE    SZ_LINE           # The text storage size in chars
define CQ_SZFNAME    (1+SZ_FNAME) / 2   # The file name storage size in structs
define CQ_SZLINE     (1+SZ_LINE) / 2    # The text storage size in structs
define CQ_ALLOC       20		# The record allocation block size

define USE_URLGET	TRUE


# The catalog record map descriptor (borrowed from dttext interface). 

define	CQ_LEN	     (8 + 2 * CQ_SZFNAME)

define	CQ_FD	      Memi[$1]		        # The database FIO channel
define	CQ_MODE	      Memi[$1+1]	        # The database access mode
define	CQ_NRECS      Memi[$1+2]		# The number of records
define	CQ_MAP	      Memi[$1+3]		# The pointer to record names
define	CQ_NAMES      Memi[$1+4]		# The pointer to name indices
define	CQ_OFFSETS    Memi[$1+5]		# The pointer to record offsets
define	CQ_CATNO      Memi[$1+6]		# The current catalog number
define	CQ_CAT	      Memi[$1+7]  		# The current catalog descriptor
define	CQ_CATDB      Memc[P2C($1+8)]           # The database file name
define	CQ_CATNAME    Memc[P2C($1+8+CQ_SZFNAME)]# The current catalog name

define	CQ_NAMEI      Memi[CQ_NAMES($1)+$2-1]
define	CQ_NAME	      Memc[CQ_MAP($1)+CQ_NAMEI($1,$2)]
define	CQ_OFFSET     Meml[CQ_OFFSETS($1)+$2-1]


# The current catalog desciptor. 

define	CQ_LEN_CC    (15 + 2 * CQ_SZLINE) 
define	QOFFSET	      P2C($1+15+$2*CQ_SZLINE)

define	CQ_NQPARS     Memi[$1]	                # The no of query params
define	CQ_PQPNAMES   Memi[$1+1]	        # The query param names ptr
define	CQ_PQPDVALUES Memi[$1+2]	        # The query param defaults ptr
define	CQ_PQPVALUES  Memi[$1+3]	        # The query param values ptr
define	CQ_PQPUNITS   Memi[$1+4]	        # The query param units ptr
define	CQ_PQPFMTS    Memi[$1+5]                # The query param format ptr
define	CQ_HFMT	      Memi[$1+6]		# The header format 
define	CQ_ADDRESS    Memc[QOFFSET($1,0)]       # The catalog address
define	CQ_QUERY      Memc[QOFFSET($1,1)]       # The network query

# The catalog results descriptor.

define	CQ_LEN_RES	(30+2*CQ_SZFNAME+2*CQ_SZLINE)
define	ROFFSET	        P2C($1+30+$2*CQ_SZFNAME+$3*CQ_SZLINE)

define	CQ_RNQPARS	Memi[$1]        # The number of query params
define	CQ_RQPNAMES     Memi[$1+1]      # The query param names ptr
define	CQ_RQPVALUES    Memi[$1+2]      # The query param values ptr
define	CQ_RQPUNITS     Memi[$1+3]      # The query param units ptr

define	CQ_RTYPE	Memi[$1+4]	# The results data format
define	CQ_RECSIZE	Memi[$1+5]      # The results record size 
define	CQ_RHSKIP	Memi[$1+6]      # The number of header records to skip
define	CQ_RTRIML	Memi[$1+7]      # The beginning of record trim 
define	CQ_RTRIMR	Memi[$1+8]      # The end of record trim 
define	CQ_RTSKIP	Memi[$1+9]      # The number of trailer records to skip

define	CQ_NHEADER	Memi[$1+10]	# The number of header keywords
define	CQ_HKNAMES	Memi[$1+11]     # The results keyword names
define	CQ_HKVALUES	Memi[$1+12]     # The result keyword values

define	CQ_NFIELDS	Memi[$1+13]	# The number of record fields
define	CQ_FNAMES	Memi[$1+14]     # The record field names
define	CQ_FTYPES	Memi[$1+15]     # The record field data types ptr
define	CQ_FOFFSETS	Memi[$1+16]     # The record field offsets ptr
define	CQ_FSIZES	Memi[$1+17]     # The record field sizes ptr
define	CQ_FUNITS	Memi[$1+18]     # The record field units
define	CQ_FFMTS	Memi[$1+19]     # The record field formats

define	CQ_RFD		Memi[$1+20]     # The results file descriptor
define	CQ_RBUF		Memi[$1+21]     # The results data descriptor
define	CQ_RNRECS	Memi[$1+22]     # The number of results records
define	CQ_RINDEX	Memi[$1+23]	# The results record index pointer

define	CQ_RECPTR	Memi[$1+24] 	# The current record
define	CQ_FNFIELDS	Memi[$1+25]     # The number of fields in current record
define	CQ_FINDICES	Memi[$1+26]     # The current record indices pointer

define	CQ_RCATDB	Memc[ROFFSET($1,0,0)] # The catalog database name
define	CQ_RCATNAME	Memc[ROFFSET($1,1,0)] # The catalog name

define	CQ_RADDRESS	Memc[ROFFSET($1,2,0)] # Query address
define	CQ_RQUERY	Memc[ROFFSET($1,2,1)] # Query string

# The image survey descriptor. May need to extend this structure as more
# experience with different image formats is obtained. May not need wcs and
# keyword default value strings ...

define	CQ_LEN_IM	(30+3*CQ_SZFNAME+2*CQ_SZLINE)
define	IOFFSET	        P2C($1+30+$2*CQ_SZFNAME+$3*CQ_SZLINE)

define	CQ_INQPARS	Memi[$1]        # The number of query params
define	CQ_IQPNAMES     Memi[$1+1]      # The query param names ptr
define	CQ_IQPVALUES    Memi[$1+2]      # The query param values ptr
define	CQ_IQPUNITS     Memi[$1+3]      # The query param units ptr
define	CQ_IMTYPE	Memi[$1+4]	# The image data format

define	CQ_WCS		Memi[$1+10]      # The image wcs type
define	CQ_NWCS		Memi[$1+11]	# The number of wcs keywords
define	CQ_WPNAMES	Memi[$1+12]     # The wcs parameter names
define	CQ_WKNAMES	Memi[$1+13]	# The wcs keyword names	
define	CQ_WKDVALUES	Memi[$1+14]	# The wcs keyword default values
define	CQ_WKVALUES	Memi[$1+15]	# The wcs keyword values
define	CQ_WKTYPES	Memi[$1+16]	# The wcs keyword data types
define	CQ_WKUNITS	Memi[$1+17]	# The wcs keyword value units

define	CQ_NIMPARS	Memi[$1+19]	# The number of header keywords
define	CQ_IPNAMES	Memi[$1+20]     # The results keyword names
define	CQ_IKNAMES	Memi[$1+21]     # The result keyword values
define	CQ_IKDVALUES	Memi[$1+22]     # The result keyword values
define	CQ_IKVALUES	Memi[$1+23]     # The result keyword values
define	CQ_IKTYPES	Memi[$1+24]     # The result keyword values
define	CQ_IKUNITS	Memi[$1+25]     # The result keyword values

define	CQ_IMCATDB	Memc[IOFFSET($1,0,0)] # The survey database name
define	CQ_IMCATNAME	Memc[IOFFSET($1,1,0)] # The survey name
define	CQ_IMNAME	Memc[IOFFSET($1,2,0)] # The image name

define	CQ_IMADDRESS	Memc[IOFFSET($1,3,0)] # Query address
define	CQ_IMQUERY	Memc[IOFFSET($1,3,1)] # Query string


define	CQ_HFMTSTR	"|none|http|"
define	CQ_HNONE	1
define	CQ_HHTTP	2
