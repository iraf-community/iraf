# This file contains tbtopn and tbtvfn.

include	<error.h>
include <syserr.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbtopn -- Open a table
# For a new table this procedure only creates the table descriptor,
# fills in default values, and creates the array of pointers to column
# descriptors.  The table is created later by calling tbtcre.
# For an existing table this procedure creates the descriptors and
# also opens the table.
#
# Phil Hodge,  7-Aug-1987  Include call to tbtext.
# Phil Hodge,  8-Sep-1987  Delete declarations of streq() and strlen().
# Phil Hodge,  8-Oct-1987  Set TB_MAXCOLS if NEW_COPY.
# Phil Hodge,  7-Mar-1989  Eliminate TB_OFFSET, TB_CURROW.
# Phil Hodge, 29-Jun-1992  Modify for text tables.
# Phil Hodge, 16-Nov-1992  Close TB_FILE here instead of in tbwopn if error.
# Phil Hodge,  8-Apr-1993  Assign a default for TB_VERSION.
# Phil Hodge, 23-Nov-1993  Check for null or blank input table name.
# Phil Hodge, 15-Dec-1994  Allocate SZ_LINE for table name.
# Phil Hodge, 22-Dec-1994  Initialize for CDF or FITS file.
# Phil Hodge,  3-Apr-1995  Assign an initial value for TB_MODIFIED.
# Phil Hodge,  2-Feb-1996  Call tbnparse instead of tbparse.
# Phil Hodge, 29-Apr-1996  Init TB_COLPTR=NULL; close table if error in tbuopn.
# Phil Hodge,  2-Mar-1998  Change calling sequence of tbnparse;
#			call tbsopn to select rows and columns;
#			reduce size of table name to SZ_FNAME.
# Phil Hodge, 22-Mar-1999  Convert file name to OS file name TB_OS_FILENAME;
#			use calloc instead of malloc for tp.
# Phil Hodge, 20-Apr-1999  Call tbttyp to get table type (or file type);
#			remove type from calling sequence of tbnparse.
# Phil Hodge,  1-Jun-1999  Initialize both TB_FILE and TB_FILE2 to 0.
# Phil Hodge,  7-Jun-1999  Initialize TB_SUBTYPE, TB_KEYLIST_PTR;
#			replace TB_F_TYPE by TB_TYPE; delete TB_HDUTYPE;
#			initialize TB_MAXCOLS.
# Phil Hodge, 12-Sep-2000  Initialize TB_INDEF_IS_CURRENT.
# Phil Hodge, 13-Nov-2001  Add file name to error message when the file
#			cannot be opened; increase buffer size of errmess.

pointer procedure tbtopn (tablename, iomode, template)

char	tablename[ARB]	# i: the name of the table
int	iomode		# i: I/O mode
pointer template	# i: pointer to template table, or zero
#--
pointer sp
pointer brackets
pointer errmess		# for possible error message
pointer rowselect, colselect	# for selector strings
pointer tp		# pointer to table descriptor
int	exists		# true if table file exists
bool	crash

int	lstart
char	url[SZ_PATHNAME], tblname[SZ_PATHNAME]
char	osfn[SZ_PATHNAME], cnvname[SZ_PATHNAME], cosfn[SZ_PATHNAME]

long	tbtbod()
int	tbnparse(), tbttyp(), strncmp(), access(), vot_to_fits()
bool	streq(), is_votable()

errchk	malloc, tbuopn, tbsopn, tbctpe, tbnparse, tbttyp, vfn_expand_ldir

begin
	call smark (sp)
	call salloc (brackets, SZ_FNAME, TY_CHAR)
	call salloc (errmess, SZ_LINE, TY_CHAR)
	call salloc (rowselect, SZ_LINE, TY_CHAR)
	call salloc (colselect, SZ_LINE, TY_CHAR)

	crash = false			# initial value

        # If we're given a URL to a file, cache it.
	call aclrc (cnvname, SZ_PATHNAME)
        if (strncmp ("http:", tablename, 5) == 0) {
	    call strcpy (tablename, url, SZ_PATHNAME)
            if (iomode == NEW_FILE)
                call syserr (SYS_FNOWRITEPERM)

	    call fcname ("cache$", url, "f", tblname, SZ_PATHNAME)
	    call strcpy (tblname, cnvname, SZ_PATHNAME)
	    call strcat (".fits", cnvname, SZ_PATHNAME)

	    if (access (cnvname, 0, 0) == NO) {
                #call fcadd ("cache$", url, "fits", tblname, SZ_PATHNAME)
                call fcadd ("cache$", url, "", tblname, SZ_PATHNAME)
		if (access (cnvname,0,0) == YES && is_votable (cnvname)) {
	            if (vot_to_fits (tblname, tblname) != OK) {
	                call error (ER_TBCONVERT, 
			    "tbtopn: cannot convert table format")
		    }
		}
	    } else
                call strcpy (cnvname, tblname, SZ_PATHNAME)

        } else if (strncmp ("file://", tablename, 7) == 0) {
	    lstart = 8
            if (strncmp ("file://localhost", tablename, 16) == 0) 
		lstart = 17
            else if (strncmp ("file:///localhost", tablename, 17) == 0) 
		lstart = 18
	
	    # Strip file:// prefix from the URI.
            call strcpy (tablename[lstart], tblname, SZ_PATHNAME)

	    call fcname ("cache$", tablename[lstart], "f", tblname, SZ_PATHNAME)
	    call strcpy (tblname, cnvname, SZ_PATHNAME)
	    call strcat (".fits", cnvname, SZ_PATHNAME)

	    if (access (cnvname, 0, 0) == NO) {
                call fcadd ("cache$", tablename[lstart], "fits", tblname,
		    SZ_PATHNAME)
		if (access (cnvname,0,0) == YES && is_votable (cnvname)) {
	            if (vot_to_fits (tblname, tblname) != OK) {
	                call error (ER_TBCONVERT, 
			    "tbtopn: cannot convert table format")
		    }
		}
	    } else
                call strcpy (cnvname, tblname, SZ_PATHNAME)

	} else if (is_votable (tablename)) {
	    call fcname ("cache$", tablename, "f", tblname, SZ_PATHNAME)
	    call strcpy (tblname, cnvname, SZ_PATHNAME)
	    call strcat (".fits", cnvname, SZ_PATHNAME)

	    if (access (cnvname, 0, 0) == NO) {
                call fcadd ("cache$", tablename, "fits", tblname, SZ_PATHNAME)
		if (access (cnvname,0,0) == YES && is_votable (cnvname)) {
                    if (vot_to_fits (tblname, cnvname) != OK) {
                        call error (ER_TBCONVERT, 
		            "tbtopn: cannot convert table format")
	            }
	        }
	    } else
                call strcpy (cnvname, tblname, SZ_PATHNAME)

        } else {
	    # Nothing to do, just use it and hope it's a format we know about.
            call strcpy (tablename, tblname, SZ_PATHNAME)
        }


	# Allocate space for the table descriptor and for the table name.
	# The TB_EXTNAME is the name of the table within a CDF file,
	# or it can be the EXTNAME in a FITS file.
	call calloc (tp, LEN_TBLSTRUCT, TY_STRUCT)
	call malloc (TB_NAME_PTR(tp), SZ_FNAME, TY_CHAR)
	call malloc (TB_OS_FILENAME_PTR(tp), SZ_FNAME, TY_CHAR)
	call malloc (TB_EXTNAME_PTR(tp), SZ_FNAME, TY_CHAR)
	if (cnvname[1] != EOS) {
	    call malloc (TB_SRC_PTR(tp), SZ_FNAME, TY_CHAR)
	    call strcpy (tablename, TB_SRC(tp), SZ_FNAME)
	}

	# Parse the table name, copying the file name to TB_NAME and
	# extracting information from the bracketed expression (if any)
	# to get TB_EXTNAME, etc.
	if (tbnparse (tblname, TB_NAME(tp), TB_EXTNAME(tp), Memc[brackets],
		SZ_FNAME, TB_EXTVER(tp), TB_HDU(tp), TB_OVERWRITE(tp),
		Memc[rowselect], Memc[colselect], SZ_LINE) < 1) {
	    crash = true
	    call strcpy ("tbtopn:  Table name is null or blank.",
			Memc[errmess], SZ_LINE)
	}
	# Convert from iraf virtual file name to actual file name.
	call vfn_expand_ldir (TB_NAME(tp), TB_OS_FILENAME(tp), SZ_FNAME)

	# Get the table type.  If the file doesn't exist, this is a guess
	# based on the filename extension.  For a FITS table in an existing
	# file, this is the file type; if an extension was specified, we're
	# not checking that yet.
	TB_TYPE(tp) = tbttyp (TB_NAME(tp), exists)

	TB_IOMODE(tp) = iomode
	TB_READONLY(tp) = (iomode == READ_ONLY)

	if (iomode != READ_ONLY && iomode != READ_WRITE &&
		(Memc[rowselect] != EOS || Memc[colselect] != EOS)) {
	    crash = true
	    call strcpy (
"tbtopn:  Row and column selectors may only be used with existing tables.",
			Memc[errmess], SZ_LINE)
	}

	if (crash) {
	    call mfree (TB_EXTNAME_PTR(tp), TY_CHAR)
	    call mfree (TB_NAME_PTR(tp), TY_CHAR)
	    call mfree (TB_OS_FILENAME_PTR(tp), TY_CHAR)
	    call mfree (tp, TY_STRUCT)
	    call error (1, Memc[errmess])
	}

	# Default values; these may be changed below.
	TB_NPAR(tp)     = 0
	TB_MAXPAR(tp)   = DEFMAXPAR
	TB_NROWS(tp)    = 0
	TB_ALLROWS(tp)  = 100
	TB_NCOLS(tp)    = 0
	TB_MAXCOLS(tp)  = DEFMAXCOLS
	TB_COLUSED(tp)  = 0
	TB_ROWLEN(tp)   = 0

	TB_ROW_SELECT(tp) = NO
	TB_NSEL_ROWS(tp) = 0
	TB_ROWSET(tp) = NULL

	TB_COLUMN_SELECT(tp) = NO
	TB_NSEL_COLS(tp) = 0
	TB_SELCOL_PTR(tp) = NULL

	TB_IS_OPEN(tp)  = false
	TB_MODIFIED(tp) = false
	TB_INDEF_IS_CURRENT(tp) = false
	TB_FILE(tp)     = 0
	TB_FILE2(tp)    = 0
	TB_INDEF(tp)    = NULL
	TB_COLPTR(tp)   = NULL
	TB_CD(tp)       = NULL
	TB_SUBTYPE(tp)  = TBL_SUBTYPE_UNKNOWN
	TB_COMMENT(tp)  = NULL
	TB_KEYLIST_PTR(tp) = NULL
	TB_VERSION(tp)  = TBL_CURRENT_VERSION

	if ((iomode == READ_ONLY) || (iomode == READ_WRITE)) {

	    if (exists != YES) {			# set by tbttyp
		call sprintf (Memc[errmess], SZ_LINE,
			"Table `%s' does not exist or cannot be opened.")
		    call pargstr (TB_OS_FILENAME(tp))
		call mfree (TB_EXTNAME_PTR(tp), TY_CHAR)
		call mfree (TB_NAME_PTR(tp), TY_CHAR)
		call mfree (TB_OS_FILENAME_PTR(tp), TY_CHAR)
		call mfree (tp, TY_STRUCT)
		call error (1, Memc[errmess])
	    }

	    # Open the table.  This allocates space for the TB_COLPTR array.
	    iferr {
		call tbuopn (tp)
	    } then {
		call tbtclo (tp)
		call erract (EA_ERROR)
	    }

	    # Select rows and columns (if specified).
	    call tbsopn (tp, Memc[rowselect], Memc[colselect])

	} else if ((iomode == NEW_FILE) || (iomode == NEW_COPY) ||
			(iomode == TEMP_FILE)) {
	    # Allocate space for the array of pointers to column descriptors.
	    if (iomode == NEW_COPY)
		TB_MAXCOLS(tp) = TB_MAXCOLS(template)
	    call malloc (TB_COLPTR(tp), TB_MAXCOLS(tp), TY_POINTER)
	    TB_BOD(tp) = tbtbod (TB_MAXPAR(tp), TB_MAXCOLS(tp))
	    # Copy column descriptors from template table.
	    if (iomode == NEW_COPY) {
		TB_IOMODE(tp) = NEW_FILE
		call tbctpe (tp, template)		# copy from template
	    }
	    # Write to standard output if the name is STDOUT.  Note that
	    # this overrides the type from the template, if NEW_COPY.
	    if (streq (TB_NAME(tp), "STDOUT") || streq (TB_NAME(tp), "STDERR"))
		TB_TYPE(tp) = TBL_TYPE_TEXT
	} else {
	    call mfree (TB_EXTNAME_PTR(tp), TY_CHAR)
	    call mfree (TB_NAME_PTR(tp), TY_CHAR)
	    call mfree (TB_OS_FILENAME_PTR(tp), TY_CHAR)
	    call mfree (tp, TY_STRUCT)
	    call error (ER_TBBADMODE,
		"tbtopn:  the specified I/O mode is not supported for a table")
	}

	call sfree (sp)
	return (tp)
end
