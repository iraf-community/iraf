include <mach.h>	# defines SZB_CHAR
include <error.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# size of packed par record; used by tbcrcd2
define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)

# These routines are for converting tables between various machine formats.
# The high-level routine is tbtopns, which calls either the normal tbtopn
# or tbtopn2.  The latter swaps bytes in the integer portions of the
# size-information record and column descriptors.
#
# Phil Hodge,  7-Oct-1989  Subroutines created.
# Phil Hodge,  6-Feb-1992  Add text for text table type.
# Phil Hodge,  8-Apr-1993  Assign a value for TB_VERSION.
# Phil Hodge, 15-Dec-1994  Table name is now SZ_LINE instead of SZ_FNAME.
# Phil Hodge, 27-Nov-1995  Assign values for TB_MODIFIED, etc., in tbtopn2;
#				rename tbwopn2 to tbuopn2.
# Phil Hodge,  2-Feb-1996  Assign initial values to TB_EXTVER, etc.
# Phil Hodge, 29-Apr-1996  Init TB_COLPTR=NULL; close table if error in tbuopn.
# Phil Hodge,  2-Mar-1998  Initialize TB_ROW_SELECT(tp) = NO, etc.
# Phil Hodge, 14-Apr-1998  Change tbcrcd2 to agree with modified tbcrcd.
# Phil Hodge, 22-Mar-1999  Convert file name to OS file name TB_OS_FILENAME;
#		use calloc instead of malloc for tp;
#		size of strings is SZ_FNAME instead of SZ_LINE.
# Phil Hodge,  1-Jun-1999  Initialize both TB_FILE and TB_FILE2 to 0.
# Phil Hodge,  7-Jun-1999  Replace TB_F_TYPE by TB_TYPE;
#	replace TB_HDUTYPE by TB_SUBTYPE;
#	when allocating TB_COLPTR, the type is TY_POINTER, not TY_INT.
# Phil Hodge, 23-Jun-2000  In tbcrcd2, assign default values to
#	COL_TDTYPE, COL_TSCAL, COL_TZERO.
# Phil Hodge, 12-Sep-2000  Initialize TB_INDEF_IS_CURRENT.

# tbtopns -- open table & optionally swap bytes
# This routine opens an existing table read-only.  If byte_swap is NO,
# we just call tbtopn; if byte_swap = YES we open the table file and
# swap bytes in the integer values, the size-info record and parts of
# the column descriptors.

procedure tbtopns (tablename, byte_swap, tp, fd)

char	tablename[ARB]		# i: the name of the table
int	byte_swap		# i: YES if we need to swap bytes
pointer tp			# o: pointer to table descriptor
int	fd			# o: fd number for table file
#--
pointer tbtopn(), tbtopn2()
errchk	tbtopn, tbtopn2

begin
	if (byte_swap == YES)
	    tp = tbtopn2 (tablename)
	else
	    tp = tbtopn (tablename, READ_ONLY, NULL)

	fd = TB_FILE(tp)
end


# tbtopn2 -- open a table
# Open an existing table read-only, and byte-swap the integer values in
# the size-information record and the column descriptors.

pointer procedure tbtopn2 (tablename)

char	tablename[ARB]		# i: the name of the table
#--
pointer tp			# pointer to table descriptor
pointer sp
pointer name			# scratch for table name including extension
pointer message			# scratch for error message
int	access()
errchk	tbtext, malloc, tbuopn2

begin
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call tbtext (tablename, Memc[name], SZ_FNAME)

	if (access (Memc[name], 0, TEXT_FILE) == YES) {
	    call smark (sp)
	    call salloc (message, SZ_FNAME, TY_CHAR)
	    call sprintf (Memc[message], SZ_FNAME, "`%s' is not a binary table")
		call pargstr (Memc[name])
	    call error (1, Memc[message])
	}

	# Allocate space for the table descriptor and the table name.
	call calloc (tp, LEN_TBLSTRUCT, TY_STRUCT)
	call malloc (TB_NAME_PTR(tp), SZ_FNAME, TY_CHAR)
	call malloc (TB_OS_FILENAME_PTR(tp), SZ_FNAME, TY_CHAR)
	TB_OS_FILENAME(tp) = EOS	# not used (only used for CFITSIO)
	TB_EXTNAME_PTR(tp) = NULL

	# Fill in some initial values.
	call strcpy (Memc[name], TB_NAME(tp), SZ_FNAME)
	call sfree (sp)
	TB_IOMODE(tp) = READ_ONLY
	TB_READONLY(tp) = true

	TB_TYPE(tp)    = TBL_TYPE_S_ROW			# column is OK, too
	TB_SUBTYPE(tp) = TBL_SUBTYPE_UNKNOWN

	# Default values; some may be changed below.
	TB_TYPE(tp)    = TBL_TYPE_S_ROW
	TB_NPAR(tp)    = 0
	TB_MAXPAR(tp)  = DEFMAXPAR
	TB_NROWS(tp)   = 0
	TB_ALLROWS(tp) = 0
	TB_NCOLS(tp)   = 0
	TB_COLUSED(tp) = 0
	TB_ROWLEN(tp)  = 0

	TB_ROW_SELECT(tp) = NO
	TB_NSEL_ROWS(tp) = 0
	TB_ROWSET(tp) = NULL

	TB_COLUMN_SELECT(tp) = NO
	TB_NSEL_COLS(tp) = 0
	TB_SELCOL_PTR(tp) = NULL

	TB_IS_OPEN(tp) = false
	TB_MODIFIED(tp) = false
	TB_INDEF_IS_CURRENT(tp) = false
	TB_FILE(tp)    = 0
	TB_FILE2(tp)   = 0
	TB_INDEF(tp)   = NULL
	TB_COLPTR(tp)  = NULL
	TB_HDU(tp)     = -1
	TB_EXTVER(tp)  = -1
	TB_OVERWRITE(tp) = -1
	TB_CD(tp)      = NULL
	TB_COMMENT(tp) = NULL
	TB_VERSION(tp) = TBL_CURRENT_VERSION

	# Open the table.  This allocates space for the TB_COLPTR array.
	iferr {
	    call tbuopn2 (tp)
	} then {
	    call tbtclo (tp)
	    call erract (EA_ERROR)
	}
	TB_IS_OPEN(tp) = true

	return (tp)
end


# tbuopn2 -- open old table
# This is like tbuopn except that it swaps bytes and the indef record
# is not created.

procedure tbuopn2 (tp)

pointer tp		# i: pointer to table descriptor
#--
pointer colptr		# pointer to column descriptor
int	colnum		# column number (a loop index)
int	open()
errchk	open, malloc, tbtrsi2, tbcrcd2

begin
	# Open the file
	TB_FILE(tp) = open (TB_NAME(tp), TB_IOMODE(tp), BINARY_FILE)

	call tbtrsi2 (tp)		# read size info & swap bytes

	# Allocate space for the array of pointers to column descriptors.
	call malloc (TB_COLPTR(tp), TB_MAXCOLS(tp), TY_POINTER)

	# Create column descriptors & read contents from table.
	do colnum = 1, TB_NCOLS(tp) {
	    call malloc (colptr, LEN_COLSTRUCT, TY_STRUCT)
	    TB_COLINFO(tp,colnum) = colptr
	    # read column descriptor & swap bytes
	    call tbcrcd2 (tp, colptr, colnum)
	}
end


# tbtrsi2 -- read size info
# This is like tbtrsi except that it swaps bytes.

procedure tbtrsi2 (tp)

pointer tp			# Pointer to table descriptor
#--
int	sizinfo[LEN_SIZINFO]	# Size information record
long	tbtbod()
int	read()
errchk	seek, read

begin
	call seek (TB_FILE(tp), BOF)
	if (read (TB_FILE(tp), sizinfo, SZ_SIZINFO) == EOF)
	    call error (ER_TBFILEMPTY, "table data file is empty")
        if (SZ_INT != SZ_INT32)
            call iupk32 (sizinfo, sizinfo, SZ_SIZINFO)

	# Swap bytes in the size information record.
	call bswap4 (sizinfo, 1, sizinfo, 1, SZ_SIZINFO*SZB_CHAR)

	TB_TYPE(tp) = S_TYPE(sizinfo)
	if ((TB_TYPE(tp) != TBL_TYPE_S_ROW) &&
	    (TB_TYPE(tp) != TBL_TYPE_S_COL))
		call error (ER_TBCORRUPTED, "unknown table type")

	TB_NPAR(tp)    = S_NPAR(sizinfo)
	TB_MAXPAR(tp)  = S_MAXPAR(sizinfo)
	TB_NROWS(tp)   = S_NROWS(sizinfo)
	TB_ALLROWS(tp) = S_ALLROWS(sizinfo)
	TB_NCOLS(tp)   = S_NCOLS(sizinfo)
	TB_MAXCOLS(tp) = S_MAXCOLS(sizinfo)
	TB_COLUSED(tp) = S_COLUSED(sizinfo)
	TB_ROWLEN(tp)  = S_ROWLEN(sizinfo)
	TB_VERSION(tp) = S_VERSION(sizinfo)

	TB_BOD(tp) = tbtbod (TB_MAXPAR(tp), TB_MAXCOLS(tp))
end


# tbcrcd2 -- read column descriptor
# This is like tbcrcd except that it swaps bytes in the integer portion
# of the column descriptor.

procedure tbcrcd2 (tp, cp, colnum)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	colnum			# i: column number
#--
pointer sp
pointer coldef			# column descriptor read from table
pointer pformat			# scratch for print format
long	offset			# location of column descriptor in table file
int	stat			# status from read operation
int	read()

errchk	seek, read

begin
	if (TB_TYPE(tp) == TBL_TYPE_TEXT || TB_TYPE(tp) == TBL_TYPE_FITS)
	    call error (1, "tbcrcd:  internal error")

	call smark (sp)
	call salloc (coldef, LEN_COLDEF, TY_STRUCT)
	call salloc (pformat, SZ_COLFMT, TY_CHAR)

	offset = SZ_SIZINFO +
		TB_MAXPAR(tp) * SZ_PACKED_REC +
		(colnum-1) * SZ_COLDEF + 1
	call seek (TB_FILE(tp), offset)
	stat = read (TB_FILE(tp), Memi[coldef], SZ_COLDEF)
	if (stat == EOF)
	    call error (ER_TBCINFMISSING,
			"tbcrcd:  EOF while reading column info for table")
	if (SZ_INT != SZ_INT32)
	    call iupk32 (Memi[coldef], Memi[coldef], SZ_COLDEF)

	# Swap bytes in the first four longwords.
	call bswap4 (Memi[coldef], 1, Memi[coldef], 1, 4*SZ_INT32*SZB_CHAR)

	# Check for and correct data type TY_CHAR.
	if (COL_DTYPE(cp) == TY_CHAR)
	    COL_DTYPE(cp) = -COL_LEN(cp) * SZB_CHAR

	# Copy the column definition that we just read from the file into
	# the column descriptor in memory.
	COL_NUMBER(cp) = CD_COL_NUMBER(coldef)
	COL_OFFSET(cp) = CD_COL_OFFSET(coldef)
	COL_LEN(cp) = CD_COL_LEN(coldef)
	COL_DTYPE(cp) = CD_COL_DTYPE(coldef)

	# COL_TDTYPE, COL_TSCAL, COL_TZERO are only needed for FITS tables.
	COL_TDTYPE(cp) = COL_DTYPE(cp)
	COL_TSCAL(cp) = 1.d0
	COL_TZERO(cp) = 0.d0

	call tbbncp1 (CD_COL_NAME(coldef), COL_NAME(cp),
		SZ_CD_COLNAME / SZB_CHAR)
	call strupk (COL_NAME(cp), COL_NAME(cp), SZ_COLNAME)

	call tbbncp1 (CD_COL_UNITS(coldef), COL_UNITS(cp),
		SZ_CD_COLUNITS / SZB_CHAR)
	call strupk (COL_UNITS(cp), COL_UNITS(cp), SZ_COLUNITS)

	# include a leading '%' in the print format
	# (tbbncp1 is in tbcrcd.x)
	Memc[pformat] = '%'
	call tbbncp1 (CD_COL_FMT(coldef), Memc[pformat+1],
		SZ_CD_COLFMT / SZB_CHAR)
	call strupk (Memc[pformat+1], Memc[pformat+1], SZ_COLFMT-1)
	call strcpy (Memc[pformat], COL_FMT(cp), SZ_COLFMT)

	call sfree (sp)
end
