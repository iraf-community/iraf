# Public definitions file for the catalog query interface.

# The catalog access interface parameter definitions

define	CQNRECS		1	# the number of records in the catalog database
define	CQSZRECLIST	2	# the length of the record name list in chars
define	CQRECLIST	3	# the record name @list
define	CQCATDB		4	# the catalog database file name
define	CQCATNO		5	# the current catalog record number
define	CQCATNAME	6	# the current catalog name

# The max size of a query parameter name, value, units, and formats string.

define	CQ_SZ_QPNAME	19
define	CQ_SZ_QPVALUE	79
define	CQ_SZ_QPUNITS	19
define	CQ_SZ_QPFMTS	11

# The maximum number of fields or columns in the result.

define	CQ_MAX_NFIELDS	100

# The catalog access results parameter definitions

define	CQRCATDB	1   # the catalog database file
define	CQRCATNAME	2   # the catalog name 
define	CQRADDRESS	3   # the address
define	CQRQUERY	4   # the query
define	CQRNQPARS	5   # the number of query parameters 
define	CQRQPNAMES	6   # the query parameter names
define	CQRQPVALUES	7   # the query parameter values
define	CQRQPUNITS	8   # the query parameter units

define	CQRTYPE		9   # the results format (currently stext or btext)
define	CQRECSIZE	10  # the record length in characters (default = 0)
define	CQRHSKIP	11  # the number of header lines to skip (default = 0)
define	CQRTSKIP	12  # the number of trailing lines to skip (default = 0)
define	CQRTRIML	13  # the number of leading characters to trim
define	CQRTRIMR	14  # the number of trailing characters to trim

define	CQRNRECS	15  # The number of records in the results
define	CQNHEADER	16  # The number of header keywords in the results
define	CQNFIELDS	17  # The number of record fields in the results
define	CQRECPTR	18  # the current record pointer

# The surveys access results parameter definitions

define  CQIMCATDB	1	
define  CQIMCATNAME	2	
define  CQIMADDRESS	3	
define  CQIMQUERY	4	
define	CQINQPARS	5
define	CQIQPNAMES	6
define	CQIQPVALUES	7
define	CQIQPUNITS	8
define  CQIMNAME	9	
define	CQIMTYPE	10
define	CQWCS		11
define	CQNWCS		12
define	CQNIMPARS	13


# The max size of a field name, value, units, and formats string.

define	CQ_SZ_FNAME	19
define	CQ_SZ_FVALUE	79
define	CQ_SZ_FUNITS	19
define	CQ_SZ_FFMTS	11


# Define the default input catalog file types

define  CQ_RTYPESTR     "|stext|btext|"

define  CQ_STEXT        1       # Simple text (free format fields)
                                #     Newline delimited records
                                #     Whitespace delimited fields
                                #     No embedded whitespace unless in ""
                                #     Skip nlines header
                                #     Skip nchars at beginning / end of record
                                #     Skip nlines trailer


define  CQ_BTEXT        2       # Blocked text (fixed format fields)
                                #     Fixed size newline delimited records
                                #     Offset and size delimited fields
                                #     Embedded whitespace permitted
                                #     Skip nlines header
                                #     Skip nchars at beginning / end of record
                                #     Skip nlines trailer


define	CQ_ITYPESTR	"|fits|"
define	CQ_FITS		1


define	CQ_WTYPESTR	"|fits|dss|none|"
define	CQ_WFITS	1
define	CQ_WDSS		2
define	CQ_WNONE	3
