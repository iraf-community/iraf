include <error.h>		# for EA_ERROR
include <time.h>		# this defines SZ_TIME
include <fset.h>		# defines F_REDIR
include <ctype.h>		# defines IS_WHITE
include <tbset.h>

define	SZ_LONG_LINE	(8192+SZ_LINE)	# allows input line up to 8192 char
define  SZ_FMT		17	# size of string containing print format
define	SZ_DTYPE	29	# size of string for data type
define	CPSPACE		21	# size of increment in space for col descr ptrs
define	T_MAXDIM	7	# maximum dimension for an array in a table

# These three macros are for dim, the pointer to an array of pointers to
# dimension info.
# for column i:
#   dimension is TCR_NDIM (dim, i)
#   length of axis j is TCR_AXLEN (dim, i, j)
define	LEN_DIM_INFO	T_MAXDIM + 1	# unit = SZ_INT32
define	TCR_NDIM 	Memi[Memi[$1+$2-1]]	# dimension of array
define	TCR_AXLEN 	Memi[Memi[$1+$2-1]+$3]	# length of an axis

# tcreate -- Program to create a table from data in an ASCII file.
#
# Phil Hodge, 22-Jul-1987  Task created
# Phil Hodge, 11-Aug-1987  Modify mk_new_cols for datatype=-n for char string.
# Phil Hodge,  8-Sep-1987  Change name from tcreat.
# Phil Hodge, 15-Oct-1987  Use tbcigi instead of COL_DTYPE.
# Phil Hodge, 20-Sep-1988  Print warning if file does not exist.
# Phil Hodge,  9-Mar-1989  Change data type in call to tbhanp from char to int.
# Phil Hodge, 17-May-1989  Add history record to table giving creation date.
# Phil Hodge, 22-May-1992  Allow input lines up to 1024 char; print warning
#			if entire line of data file is not read;
#			print prompt if input is STDIN and not redirected.
# Phil Hodge, 11-Jan-1993  In mk_new_cols, move "ncols = ncols + 1" to just
#			before the call to tbcdef.
# Phil Hodge, 10-May-1993  In row_copy, include TY_SHORT.
# Phil Hodge, 11-Aug-1993  Add tcr_ctoi, which calls ctoi after skipping over
#			leading whitespace and/or a "+" sign; call in row_copy.
# Phil Hodge, 18-Nov-1994  Add option of creating columns of arrays.
# Phil Hodge, 19-Jul-1995  Add tp to calling sequence of tbcisa.
# Phil Hodge, 20-Jul-1998  In cp_upar_tbl, call tbfres for a FITS table.
# Phil Hodge, 18-Jun-1999  Add option to create a text table with explicit
#			column definitions.
# Phil Hodge, 29-Jul-1999  In tcr_ctoi, check that the value in the data file
#		has no fractional part.  linenum was added to the calling
#		sequence of tcr_ctoi for a possible error message.
# Phil Hodge, 12-Nov-2001  Allow input lines up to 8192 characters in length.
# Phil Hodge, 24-Dec-2003  Move the call to tbcisa from mk_new_cols to a
#		point after the call to tbtcre.  This had to be done because
#		tbcisa sets the value of a header keyword, and the table file
#		doesn't exist until after tbtcre has been called.  Add the
#		three routines dim_alloc, dim_set, and dim_free.  Also change
#		the data type of cptr from TY_INT to TY_POINTER.
# Phil Hodge, 20-Dec-2004  Check cdname, dname, pname for " ".

procedure tcreate()

pointer sp
pointer tname			# scratch for name of table to be created
pointer cdname			# scratch for name of file of column definitions
pointer dname			# scratch for name of file for table data
pointer pname			# scratch for name of file of header parameters
pointer ttype			# scratch for table type (e.g. "row")
pointer tp			# pointer to descriptor for output table
pointer cptr			# pointer to array of column pointers
pointer dim			# pointer to array of column dimension info
int	uparfd			# fd for input file of header parameters
int	nskip			# number of lines to skip at beg of data file
int	nlines			# number of lines in file per row in table
int	npar			# number of header parameters
int	nrows, ncols		# number of rows and columns in table
int	extracol		# number of extra columns to allocate
int	extrapar		# extra space to allocate for header parameters
int	maxcols			# size of arrays for column info
bool	histflag		# add a history record with creation date?
pointer tbtopn()
int	clgeti()
bool	clgetb()
bool	isblank()

begin
	call smark (sp)
	call salloc (tname, SZ_FNAME, TY_CHAR)
	call salloc (cdname, SZ_FNAME, TY_CHAR)
	call salloc (dname, SZ_FNAME, TY_CHAR)
	call salloc (pname, SZ_FNAME, TY_CHAR)
	call salloc (ttype, SZ_FNAME, TY_CHAR)

	call clgstr ("table", Memc[tname], SZ_FNAME)
	call clgstr ("cdfile", Memc[cdname], SZ_FNAME)
	call clgstr ("datafile", Memc[dname], SZ_FNAME)
	call clgstr ("uparfile", Memc[pname], SZ_FNAME)
	nskip  = clgeti ("nskip")
	nlines = clgeti ("nlines")
	nrows = clgeti ("nrows")
	histflag = clgetb ("hist")
	extrapar = clgeti ("extrapar")
	call clgstr ("tbltype", Memc[ttype], SZ_FNAME)

	# The user might have given the name as " " instead of EOS (""); check
	# for this, and in this case make sure the value is EOS to simplify
	# checking elsewhere in this file.
	if (isblank (Memc[cdname]))
	    Memc[cdname] = EOS
	if (isblank (Memc[dname]))
	    Memc[dname] = EOS
	if (isblank (Memc[pname]))
	    Memc[pname] = EOS

	tp = tbtopn (Memc[tname], NEW_FILE, 0)

	if (Memc[ttype] == 'r') {		# row-ordered stsdas format
	    call tbpset (tp, TBL_WHTYPE, TBL_TYPE_S_ROW)
	    extracol = clgeti ("extracol")
	} else if (Memc[ttype] == 'c') {	# column-ordered stsdas format
	    if (nrows <= 0)
		call error (1, "must specify nrows>0 for column-ordered table")
	    call tbpset (tp, TBL_WHTYPE, TBL_TYPE_S_COL)
	    call tbpset (tp, TBL_ALLROWS, nrows)
	    extracol = 0
	} else if (Memc[ttype] == 't') {	# text table
	    # not a simple text table, one with explicit column definitions
	    call tbpset (tp, TBL_WHTYPE, TBL_TYPE_TEXT)
	    call tbpset (tp, TBL_SUBTYPE, TBL_SUBTYPE_EXPLICIT)
	} else {				# default type
	    extracol = clgeti ("extracol")
	}

	# Read column descriptions, and create columns; ncols = 0 is OK.
	call mk_new_cols (Memc[cdname], tp, cptr, dim, ncols, maxcols)

	# Increase allocation of space for columns.
	if (extracol > 0)
	    call tbpset (tp, TBL_INCR_ROWLEN, extracol)

	# Open the (optional) file containing header parameters, and count how
	# many there are.  If npar = 0 the input file will not be left open.
	if (Memc[pname] != EOS) {
	    call c_user_par (Memc[pname], uparfd, npar)
	} else {
	    npar = 0				# there is no upar file
	    uparfd = NULL
	}
	if (histflag)
	    npar = npar + 1

	# Specify how much space to allocate for header parameters.
	call tbpset (tp, TBL_MAXPAR, npar+extrapar)

	# Open (create) the table.
	call tbtcre (tp)

	# Assign column dimension info, if appropriate.
	call dim_set (tp, Memi[cptr], dim, ncols)

	# Copy header parameters to table, and close the uparfile.
	call cp_upar_tbl (tp, uparfd, histflag)

	# Read from data file and write to table.
	if (ncols > 0)
	    call cp_dat_tbl (Memc[dname], tp, Memi[cptr], nskip, nlines, nrows)

	call tbtclo (tp)
	if (cptr != NULL)
	    call mfree (cptr, TY_POINTER)
	call dim_free (dim, maxcols)

	call sfree (sp)
end


# mk_new_cols -- make new columns
# This routine reads column descriptions from an input ASCII file
# and defines those columns in the table.

procedure mk_new_cols (cdname, tp, cptr, dim, ncols, maxcols)

char	cdname[ARB]		# i: name of column-definitions file
pointer tp			# i: pointer to table descriptor
pointer cptr			# o: pointer to array of column descriptors
pointer dim			# o: pointer to array of column dimension info
int	ncols			# o: number of columns created (may be zero)
int	maxcols			# o: size of arrays for column info
#--
pointer sp
pointer lbuf			# buffer for reading lines from col descr file
char	colname[SZ_COLNAME]	# column name
char	colunits[SZ_COLUNITS]	# column units
char	colfmt[SZ_COLFMT]	# print format for column
char	chdtype[SZ_DTYPE]	# column data type expressed as a char string
int	fd			# for input ASCII file
int	linenum			# line number counter (ignored)
int	datatype		# column data type expressed as an int
int	nelem			# array length
int	ip			# index in line of text from input file
int	access(), open(), g_next_l(), ctowrd(), fstati()
bool	streq()

begin
	ncols = 0			# initial values
	cptr = NULL
	dim = NULL

	if (cdname[1] == EOS) {
	    call eprintf ("No cdfile; an empty table will be created.\n")
	    return
	} else if (access (cdname, 0, 0) == NO) {
	    call eprintf ("WARNING:  can't read file %s;\n")
		call pargstr (cdname)
	    call eprintf (" ... an empty table will be created.\n")
	    return
	} else if (streq (cdname, "STDIN")) {
	    # Print a prompt if the input is not redirected.
	    if (fstati (STDIN, F_REDIR) == NO) {
		call printf (
	"Give column definitions (name, datatype, print format, units)\n")
		call printf (" ... then newline & EOF to finish.\n")
		call flush (STDOUT)
	    }
	}

	fd = open (cdname, READ_ONLY, TEXT_FILE)

	call smark (sp)
	call salloc (lbuf, SZ_LONG_LINE, TY_CHAR)

	maxcols = CPSPACE
	call calloc (cptr, maxcols, TY_POINTER)
	call dim_alloc (dim, ncols, maxcols)

	# While get next non-comment line ...
	linenum = 0
	while (g_next_l (fd, Memc[lbuf], SZ_LONG_LINE, linenum) != EOF) {
	    ip = 1
	    if (ctowrd (Memc[lbuf], ip, colname, SZ_COLNAME) < 1)
		call error (1, "could not read column name")
	    if (ncols+1 > maxcols) {
		maxcols = maxcols + CPSPACE
		call realloc (cptr, maxcols, TY_POINTER)
		call dim_alloc (dim, ncols, maxcols)
	    }
	    if (ctowrd (Memc[lbuf], ip, chdtype, SZ_DTYPE) < 1) {
		call strcpy ("r", chdtype, SZ_DTYPE)	# default is real
		colfmt[1] = EOS
		colunits[1] = EOS
	    } else if (ctowrd (Memc[lbuf], ip, colfmt, SZ_COLFMT) < 1) {
		colfmt[1] = EOS
		colunits[1] = EOS
	    } else if (ctowrd (Memc[lbuf], ip, colunits, SZ_COLUNITS) < 1) {
		colunits[1] = EOS
	    }

	    # Convert the format from Fortran style to SPP style.
	    call tbbftp (colfmt, colfmt)

	    iferr {
		# Convert data type to an integer.  Use ncols+1 because
		# ncols hasn't been incremented yet.
		call tcr_nelem (chdtype,
			TCR_NDIM (dim, ncols+1), TCR_AXLEN (dim, ncols+1, 1),
			T_MAXDIM, nelem, datatype)
	    } then {
		call erract (EA_WARN)
		call eprintf ("column `%s' ignored\n")
		    call pargstr (colname)
	    } else {
		# Create the column.
		ncols = ncols + 1			# bug fix 1/11/93
		call tbcdef (tp, Memi[cptr+ncols-1],
			colname, colunits, colfmt, datatype, nelem, 1)
	    }
	}
	call close (fd)		# done with column descriptions file
	call sfree (sp)
end

# Allocate or reallocate memory for the array of column dimensions.

procedure dim_alloc (dim, ncols, maxcols)

pointer dim		# io: allocate (or reallocate) this buffer
int	ncols		# i: current number of columns
int	maxcols		# i: new number of elements for dim
#--
int	i, k		# loop indices

begin
	if (dim == NULL)
	    call malloc (dim, maxcols, TY_POINTER)
	else
	    call realloc (dim, maxcols, TY_POINTER)

	# Assign initial values.  These may be updated later.
	do i = ncols+1, maxcols {		# zero indexed
	    call malloc (Memi[dim+i-1], LEN_DIM_INFO, TY_INT)
	    TCR_NDIM (dim, i) = 1
	    do k = 1, T_MAXDIM
		TCR_AXLEN (dim, i, k) = 1
	}
end

# For each column of multi-dimensional arrays, call the routine to assign
# the keyword giving the length of each axis.

procedure dim_set (tp, cp, dim, ncols)

pointer tp		# i: pointer to table descriptor
pointer cp[ARB]		# i: array of column descriptors
pointer dim		# i: pointer to array of column dimension info
int	ncols		# i: current number of columns
#--
int	i		# loop index

begin
	do i = 1, ncols {
	    if (TCR_NDIM (dim, i) > 1)
		call tbcisa (tp, cp[i], TCR_NDIM(dim,i), TCR_AXLEN(dim,i,1))
	}
end

# Free memory for the array of column dimensions.

procedure dim_free (dim, maxcols)

pointer dim		# io: pointer to array of column dimension info
int	maxcols		# i: new number of elements for dim
#--
int	i		# loop index

begin
	if (dim == NULL)
	    return

	do i = 1, maxcols {		# zero indexed
	    if (Memi[dim+i-1] != NULL)
		call mfree (Memi[dim+i-1], TY_INT)
	}
	call mfree (dim, TY_POINTER)
end

# c_user_par -- count header parameters
# This routine opens an input ASCII file containing header parameters
# and counts the number of such parameters.  If the input file exists
# and does contain parameters, the file will be left open; otherwise,
# the input file will be closed, and both npar and uparfd will be set
# to zero.
# Blank and comment lines are ignored.

procedure c_user_par (pname, uparfd, npar)

char	pname[ARB]		# i: name of file of header parameters
int	uparfd			# o: fd for input file of header parameters
int	npar			# o: number of header parameters in file
#--
pointer sp
pointer lbuf			# scratch for input line buffer
int	linenum			# line number counter (ignored)
int	access(), open(), g_next_l()

begin
	uparfd = NULL			# initial values
	npar = 0

	if (pname[1] == EOS) {
	    return
	} else if (access (pname, 0, 0) == NO) {
	    call eprintf ("WARNING:  can't read file %s.\n")
		call pargstr (pname)
	    return
	}
	uparfd = open (pname, READ_ONLY, TEXT_FILE)

	call smark (sp)
	call salloc (lbuf, SZ_LONG_LINE, TY_CHAR)

	linenum = 0
	while (g_next_l (uparfd, Memc[lbuf], SZ_LONG_LINE, linenum) != EOF)
	    npar = npar + 1

	call sfree (sp)
	if (npar <= 0) {
	    call close (uparfd)
	    uparfd = NULL
	}
end


# cp_upar_tbl -- copy header parameters to table
# This routine reads header parameters (keyword, type, value) from an
# ASCII file and writes them to the table.  The input file is then closed.
# If uparfd is zero then it is assumed that the file does not exist.

procedure cp_upar_tbl (tp, uparfd, histflag)

pointer tp			# i: pointer to table descriptor
int	uparfd			# io: fd for file of header parameters
bool	histflag		# i: add a history record with current date?
#--
pointer sp
pointer lbuf			# scratch for input line buffer
pointer datetime		# scratch for date and time
pointer history			# scratch for history record
char	keyword[SZ_KEYWORD]	# keyword for parameter
char	chdtype[SZ_DTYPE]	# column data type expressed as a char string
long	old_time, new_time	# zero; current clock time
int	datatype		# data type:  TY_CHAR, etc
int	linenum			# line number counter (ignored)
int	ip			# counter for indexing in line buffer
int	parnum			# parameter number (ignored)
int	tbltype			# table type, to check for fits type
int	g_next_l(), ctowrd()
int	tbpsta(), tbfres()
long	clktime()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LONG_LINE, TY_CHAR)

	tbltype = tbpsta (tp, TBL_WHTYPE)

	# If there is a file of header parameters, read them and add to the
	# table header.
	if (uparfd != NULL) {

	    call seek (uparfd, BOF)		# rewind to beginning of file
	    linenum = 0
	    while (g_next_l (uparfd, Memc[lbuf], SZ_LONG_LINE,
			linenum) != EOF) {
		ip = 1
		# Read:  keyword   datatype   value   comment
		if (ctowrd (Memc[lbuf], ip, keyword, SZ_KEYWORD) <= 0) {
		    call eprintf ("line is `%s'\n")
			call pargstr (Memc[lbuf])
		    call error (1, "bad line in 'uparfile'")
		}
		if (tbltype == TBL_TYPE_FITS && tbfres (keyword) == YES)
		    next		# skip reserved keywords if FITS table
		if (ctowrd (Memc[lbuf], ip, chdtype, SZ_DTYPE) <= 0) {
		    call eprintf ("line is `%s'\n")
			call pargstr (Memc[lbuf])
		    call error (1, "bad line in 'uparfile'")
		}
		call strlwr (chdtype)
		datatype = chdtype[1]
		switch (datatype) {
		case 'r':
		    datatype = TY_REAL
		case 'i':
		    datatype = TY_INT
		case 'd':
		    datatype = TY_DOUBLE
		case 'b':
		    datatype = TY_BOOL
		default:
		    datatype = TY_CHAR
		}
		while (IS_WHITE(Memc[lbuf+ip-1]))
		    ip = ip + 1
		call tbhanp (tp, keyword, datatype, Memc[lbuf+ip-1], parnum)
	    }
	    # Close the input ASCII file containing header parameters.
	    call close (uparfd)
	}

	if (histflag) {

	    call salloc (datetime, SZ_TIME, TY_CHAR)
	    call salloc (history, SZ_LINE, TY_CHAR)

	    old_time = 0
	    new_time = clktime (old_time)
	    call cnvtime (new_time, Memc[datetime], SZ_TIME)
	    call strcpy ("Created ", Memc[history], SZ_LINE)
	    call strcat (Memc[datetime], Memc[history], SZ_LINE)

	    call tbhadt (tp, "history", Memc[history])
	}

	call sfree (sp)
end


# cp_dat_tbl -- copy data to table
# This routine reads from an ASCII data file and writes the values to
# the table.  A subroutine is called to do the actual copying for each row.

procedure cp_dat_tbl (dname, tp, cptr, nskip, nlines, nrows)

char	dname[ARB]		# i: name of file containing table data
pointer tp			# i: pointer to table descriptor
pointer cptr[ARB]		# i: array of pointers to column descriptors
int	nskip			# i: number of lines to skip at beginning
int	nlines			# i: number of lines per data file record
int	nrows			# i: upper limit on number of rows (if > 0)
#--
pointer sp
pointer linebuf			# scratch for skipping header lines
int	fd			# file descriptor for ASCII data file
int	rownum			# row number
int	linenum			# line number counter
int	k			# loop index
int	stat
bool	done
int	access(), open(), getlline(), fstati()
bool	streq()

begin
	if (dname[1] == EOS) {
	    return
	} else if (access (dname, 0, 0) == NO) {
	    call eprintf ("WARNING:  file `%s' does not exist;\n")
		call pargstr (dname)
	    call eprintf (" ... an empty table will be created.\n")
	    return
	} else if (streq (dname, "STDIN")) {
	    # Print a prompt if the input is not redirected.
	    if (fstati (STDIN, F_REDIR) == NO) {
		call printf (
		"Give table data ... then newline & EOF to finish.\n")
		call flush (STDOUT)
	    }
	}
	fd = open (dname, READ_ONLY, TEXT_FILE)

	# Skip "header" lines.
	if (nskip > 0) {
	    call smark (sp)
	    call salloc (linebuf, SZ_LONG_LINE, TY_CHAR)
	    do k = 1, nskip
		stat = getlline (fd, Memc[linebuf], SZ_LONG_LINE)
	    call sfree (sp)			# done with scratch space
	}

	# Read each record (which may be more than one line) from the
	# data file, and write the values to the output row in the table.
	rownum = 1
	linenum = nskip			# number of lines read so far
	done = false
	while ( !done ) {
	    call row_copy (tp, fd, cptr, rownum, nlines, linenum, done)
	    rownum = rownum + 1
	    if (nrows > 0 && rownum > nrows)
		done = true
	}
	call close (fd)
end



# row_copy -- copy to a row of the table
# This routine reads one or more records from data file and writes
# a single row to the table.

procedure row_copy (tp, fd, cptr, rownum, nlines, linenum, done)

pointer tp			# i: pointer to table descriptor
pointer cptr[ARB]		# i: array of pointers to column descriptors
int	fd			# i: file descriptor for input data file
int	rownum			# i: row number in table
int	nlines			# i: number of lines per data file record
int	linenum			# io: line number counter
bool	done			# io: set to true when finished
#--
pointer sp
pointer lbuf			# buffer for reading from data file
int	ncols			# number of columns in table
int	col			# loop index (column number)
int	k			# loop index for skipping lines
int	dtype			# data type of a column (-n for char)
int	nelem			# number of elements in array
int	i			# loop index for element number
int	n			# counter for number of lines per table row
int	ip			# index in line buffer lbuf
int	nchar			# number of char in text string
int	stat
pointer str			# buffer for value to be put in table
double	dval			#   "
int	ival			#   "
bool	bval
int	tbpsta(), tbcigi(), g_next_l(), tcr_ctoi(), ctod(), ctowrd()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (lbuf, SZ_LONG_LINE, TY_CHAR)

	ncols = tbpsta (tp, TBL_NCOLS)

	n = 1
	if (g_next_l (fd, Memc[lbuf], SZ_LONG_LINE, linenum) == EOF) {
	    done = true
	    return
	}
	ip = 1
	for (col=1;  col<=ncols;  col=col+1) {
	    dtype = tbcigi (cptr[col], TBL_COL_DATATYPE)
	    nelem = tbcigi (cptr[col], TBL_COL_LENDATA)
	    switch (dtype) {
	    case TY_REAL,TY_DOUBLE:
		do i = 1, nelem {
		    if (ctod (Memc[lbuf], ip, dval) < 1) {
			if (nlines > 0 && n >= nlines)
			    return		# ignore any remaining columns

			# Print warning if we're not really at the end of line.
			call tcr_check_eol (Memc[lbuf+ip-1], linenum)

			if (g_next_l (fd, Memc[lbuf], SZ_LONG_LINE,
				    linenum) == EOF) {
			    done = true
			    return
			}
			n = n + 1
			ip = 1
			if (ctod (Memc[lbuf], ip, dval) < 1) {
			    call sprintf (Memc[str], SZ_LINE,
			    "badly out of synch in line %d in data file\n")
				call pargi (linenum)
			    call error (1, Memc[str])
			}
		    }
		    call tbaptd (tp, cptr[col], rownum, dval, i, 1)
		}

	    case TY_INT,TY_SHORT:
		do i = 1, nelem {
		    if (tcr_ctoi (Memc[lbuf], ip, ival, linenum) < 1) {
			if (nlines > 0 && n >= nlines)
			    return		# ignore any remaining columns

			call tcr_check_eol (Memc[lbuf+ip-1], linenum)

			if (g_next_l (fd, Memc[lbuf], SZ_LONG_LINE,
				    linenum) == EOF) {
			    done = true
			    return
			}
			n = n + 1
			ip = 1
			if (tcr_ctoi (Memc[lbuf], ip, ival, linenum) < 1) {
			    call sprintf (Memc[str], SZ_LINE,
			    "badly out of synch in line %d in data file\n")
				call pargi (linenum)
			    call error (1, Memc[str])
			}
		    }
		    call tbapti (tp, cptr[col], rownum, ival, i, 1)
		}

	    case TY_BOOL:
		do i = 1, nelem {
		    if (ctowrd (Memc[lbuf], ip, Memc[str], SZ_LINE) < 1) {
			if (nlines > 0 && n >= nlines)
			    return		# ignore any remaining columns

			call tcr_check_eol (Memc[lbuf+ip-1], linenum)

			if (g_next_l (fd, Memc[lbuf], SZ_LONG_LINE,
				    linenum) == EOF) {
			    done = true
			    return
			}
			n = n + 1
			ip = 1
			if (ctowrd (Memc[lbuf], ip, Memc[str], SZ_LINE) < 1) {
			    call sprintf (Memc[str], SZ_LINE,
			    "badly out of synch in line %d in data file\n")
				call pargi (linenum)
			    call error (1, Memc[str])
			}
		    }
		    call strlwr (Memc[str])
		    if (Memc[str] == 'y' || Memc[str] == 't')	# yes or true
			bval = true
		    else if (Memc[str] == 'n' || Memc[str] == 'f') # no or false
			bval = false
		    else {
			call strcat (" is not a valid Boolean value",
				    Memc[str], SZ_LINE)
			call error (1, Memc[str])
		    }
		    call tbaptb (tp, cptr[col], rownum, bval, i, 1)
		}

	    default:
		if (dtype >= 0)
		    call error (1, "invalid data type got past tbbtyp")

		do i = 1, nelem {
		    # Be careful to distinguish between a value of "" at the
		    # end of a line and actually reaching the end of the line;
		    # ctowrd returns 0 in both cases.  First skip whitespace.
		    while (IS_WHITE(Memc[lbuf+ip-1]))
			ip = ip + 1
		    if (Memc[lbuf+ip-1] == '\n' || Memc[lbuf+ip-1] == EOS) {
			if (nlines > 0 && n >= nlines)
			    return		# ignore any remaining columns
			if (g_next_l (fd, Memc[lbuf], SZ_LONG_LINE,
				    linenum) == EOF) {
			    done = true
			    return
			}
			n = n + 1
			ip = 1
		    }
		    nchar = ctowrd (Memc[lbuf], ip, Memc[str], SZ_LINE)
		    if (nchar > 0)
			call tbaptt (tp, cptr[col], rownum,
				Memc[str], nchar, i, 1)
		}
	    }
	}
	# Skip extra lines if all columns gotten in fewer than nlines lines.
	do k = n+1, nlines {
	    iferr (stat = g_next_l (fd, Memc[lbuf], SZ_LONG_LINE, linenum))
		break
	}

	call sfree (sp)
end

# tcr_check_eol -- check for end of data
# This routine checks whether the remainder of the line contains anything
# other than whitespace and comments.  If it does, a warning is printed.

procedure tcr_check_eol (lbuf, linenum)

char	lbuf[ARB]		# i: input line
int	linenum			# i: line number for warning message
#--
int	ip			# loop index into lbuf
bool	line_empty		# true if the line is empty
bool	done			# loop termination flag

begin
	line_empty = false

	ip = 1
	done = false
	while (!done) {
	    if (lbuf[ip] == ' ' || lbuf[ip] == '\t') {	# skip whitespace
		ip = ip + 1
	    } else if (lbuf[ip] == '\n' || lbuf[ip] == EOS) {
		line_empty = true
		done = true
	    } else if (lbuf[ip] == '#') {
		line_empty = true
		done = true
	    } else {
		line_empty = false
		done = true
	    }
	}

	if (!line_empty) {
	    call eprintf ("out of synch or extra data in line %d\n")
		call pargi (linenum)
	}
end

# tcr_nelem -- separate array length from data type
# This routine takes a character string as input and returns the data
# type, total array length, dimension of array, and length of each axis.
# The syntax for axis lengths is numbers separated by commas, enclosed in
# brackets or parentheses, following the data type.  For example, a 3-D
# array of 8-byte character strings with axis lengths of 30, 70, and 5
# would be specified as ch*8[30,70,5].  The first axis is the most rapidly
# varying (i.e. Fortran notation).
#
# The output data type is the usual integer code, e.g. TY_REAL, except
# that for a character string of length N the code is -N.  This is the
# data type that would be given as input to tbcdef.  The default data
# type is real (TY_REAL).

procedure tcr_nelem (chdtype, ndim, axlen, maxdim, nelem, dtype)

char	chdtype[ARB]	# i: data type and dimensions
int	ndim		# o: dimension of array
int	axlen[maxdim]	# o: length of each axis of array
int	maxdim		# i: size of array axlen
int	nelem		# o: total number of elements in array
int	dtype		# o: data type of column for input to tbcdef
#--
char	temp[SZ_DTYPE]	# scratch for copy of chdtype
char	lbracket	# '['
char	lparen		# '('
char	endchar		# ']' or ')', as appropriate
int	indexb, indexp	# locations of '[' and '(' in chdtype
int	ip, ctoi()
int	i, length	# current dimension and axis length
bool	done		# to stop loop over dimensions
int	stridx()
string	errmessage "data type `%s':\n"

begin
	lparen = '('
	lbracket = '['

	ndim = 1				# initial values
	nelem = 1
	do i = 1, maxdim
	    axlen[i] = 1

	if (chdtype[1] == EOS) {
	    dtype = TY_REAL			# default
	    return
	}

	call strcpy (chdtype, temp, SZ_DTYPE)
	indexb = stridx (lbracket, chdtype)	# "[" found?
	indexp = stridx (lparen, chdtype)	# "(" found?
	done = false
	if (indexb > 0 && indexp > 0) {
	    call eprintf (errmessage)
		call pargstr (chdtype)
	    call error (1, "can't include both '[' and '('")
	} else if (indexb > 0) {
	    endchar = ']'
	    ip = indexb
	    temp[ip] = EOS		# now temp is just the data type
	} else if (indexp > 0) {
	    endchar = ')'
	    ip = indexp
	    temp[ip] = EOS
	} else {
	    done = true			# don't try to extract array size
	}

	# Convert the string to integer code (e.g. "r" --> TY_REAL).
	iferr {
	    call tbbtyp (temp, dtype)
	} then {
	    call eprintf (errmessage)
		call pargstr (chdtype)
	    call erract (EA_ERROR)
	}

	# Read axis lengths from brackets, if given.
	i = 0
	ip = ip + 1				# skip over '['
	while (!done) {

	    if (ctoi (chdtype, ip, length) < 1) {
		call eprintf (errmessage)
		    call pargstr (chdtype)
		call error (1, "syntax error")

	    }

	    i = i + 1				# increment dimension
	    if (i > maxdim) {
		call eprintf (errmessage)
		    call pargstr (chdtype)
		call error (1, "dimension is too high")
	    }

	    if (length <= 0) {
		call eprintf (errmessage)
		    call pargstr (chdtype)
		call error (1, "axis lengths must be positive")
	    }

	    ndim = i
	    axlen[ndim] = length
	    nelem = nelem * length

	    if (chdtype[ip] == ',') {		# separator between dimensions
		ip = ip + 1
	    } else if (chdtype[ip] == endchar) {	# ']' or ')'
		done = true
	    } else if (chdtype[ip] == EOS) {
		call eprintf (errmessage)
		    call pargstr (chdtype)
		call eprintf ("info:  missing `%c' assumed\n")
		    call pargc (endchar)
		done = true
	    }
	}
end

# tcr_ctoi -- ctoi, ignoring "+" sign
# This calls ctoi after skipping over any whitespace and/or a plus sign.
# Note that we allow whitespace after the sign as well as before.  This
# is reasonable because we know (from the cdfile) that we're getting an
# integer rather than an arbitrary character string.
#
# After reading an integer value, if the next character in the input
# string is not whitespace and not the end of the line, the word will be
# reread from the input string as a double.  If the value is actually
# an integer, even though the string may contain a decimal point or an
# exponent (e.g. "5." or "1.e2"), the integer will be returned as the
# value of n.  If the value has a fractional part, that's an error.

int procedure tcr_ctoi (input, ip, n, linenum)

char	input[ARB]	# i: input string
int	ip		# io: starting/ending index in INPUT
int	n		# o: value read from string
int	linenum		# i: line number for possible error message
#--
pointer sp, word	# in case value is floating point
int	i		# local copy of integer value from string
int	nvals		# value returned by ctoi
bool	positive	# true if value is positive
int	ctoi(), ctod(), ctowrd()
int	ip_save		# so we can call ctod() or ctowrd()
double	x

begin
	positive = true			# initial value

	while (IS_WHITE(input[ip]))	# skip leading whitespace
	    ip = ip + 1

	if (input[ip] == '+')		# ignore "+" sign
	    ip = ip + 1

	if (input[ip] == '-') {		# make note of "-" sign
	    ip = ip + 1
	    positive = false
	}

	ip_save = ip

	nvals = ctoi (input, ip, i)

	# Allow for the possibility that the buffer contains a floating
	# point value.
	if (!IS_WHITE(input[ip]) && input[ip] != EOS) {

	    # Conversion to int was terminated before the end of the word.
	    ip = ip_save
	    nvals = ctod (input, ip, x)
	    if (nvals > 0) {
		i = int (x)
		if (x != double(i)) {
		    # There is a fractional part; this is an error.
		    call smark (sp)
		    call salloc (word, SZ_FNAME, TY_CHAR)
		    ip = ip_save
		    nvals = ctowrd (input, ip, Memc[word], SZ_FNAME)
		    call eprintf ("`%s' in line %d is not an integer\n")
			call pargstr (Memc[word])
			call pargi (linenum)
		    call error (1, "data type conflict with cdfile")
		}
	    }
	}

	if (nvals < 1)
	    n = INDEFI
	else if (positive)
	    n = i
	else
	    n = -i

	return (nvals)
end
