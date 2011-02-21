# tblerr.h -- error codes for table I/O routines
#
# Phil Hodge, 30-Sep-87  Change numbers and reorganize.
# Phil Hodge,  2-Jun-89  Remove 4867 from error numbers.

define	ER_TBNAMTOOLONG		01	# file name (incl extension) is too long
define	ER_TBBADMODE		02	# I/O mode is not supported for a table
define	ER_TBREADONLY		03	# attempt to modify a readonly table

define	ER_TBTOOLATE		31	# too late, table is already open
define	ER_TBNOTOPEN		32	# table must be open for this option
define	ER_TBBADOPTION		33	# invalid option for tbpset
define	ER_TBUNKPARAM		34	# unknown parameter for tbpsta

define	ER_TBCOLEXISTS		41	# column already exists
define	ER_TBBADTYPE		42	# invalid data type for a table column

define	ER_TBBEYONDEOF		51	# requested row is beyond EOF

define	ER_TBPARNOTFND		61	# header parameter not found
define	ER_TBMUSTADD		62	# new parameter must be added, not put
define	ER_TBDTYPECONFLICT	63	# can't put numeric parameter as comment

define	ER_TBCORRUPTED		81	# table or memory is corrupted
define	ER_TBCOLBADTYP		82	# bad data type (memory corrupted?)
define	ER_TBFILEMPTY		83	# table data file is empty
define	ER_TBCINFMISSING	84	# EOF while reading column info
