include <tbset.h>
include <ctype.h>
include "igi.h"
include "commands.h"

#define	BUFSIZE		1024	# max number of elements copied at one time

# Modified igrcolumn() to recognize comments (lines starting with "#")
# 1990 August 8, Z. G. Levay, STScI
# 2/4/91 Modified to parse text columns as whitespace-delimited words
# rather than numbers to avoid problems with invalid numerical values.
# ZGL
# 1/27/93  Fixed INDEF tests.
# Mar96: Uses the standard tables interface for text tables.  This
#        bypasses all the igi code to read text tables, though
#        the subroutines are still in this file.
# Apr 97: Added ability to work with 3-D tables, by allowing specification
#	  of row number containing array of data.  Column extraction from
#	  3-D tables based on 'taextract.x' in 'ttools/atools'.  Also, edited
#	  to allocate space and read in entire array at one time, then convert
#   	  to real values for use by IGI.
#	  (WJH)
#
# Aug 97:  Corrected behavior of command so that only those parameters entered
#		are passed along to the command buffer, not 2 parameters
#		regardless of what was input.  This was done by moving
#		'lcmdcat' for second parameter into its if...else loop.
#		(WJH)
#

procedure ig_column (cmd, igs)

int	cmd		# Command index
pointer	igs		# Parameters structure

int	in		# Input stream descriptor
pointer	tokvals		# Token value structure
int	token
int	igps
pointer	sp, column

int	readtok()

begin
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)

	call lcmdcat (igs, YES)
	igps = PLOT_PARMS(igs)

	if (MG_DATASRC(igps) == NO_DATA) {
	    call eprintf ("No data file specified ")
	    return
	}

	token = readtok (in, tokvals)

	# Get the column.
	call smark (sp)
	call salloc (column, SZ_COLNAME, TY_CHAR)

	switch (token) {
	case CONSTANT:
	    call sprintf (Memc[column], SZ_COLNAME, "c%d")
	    call pargi (LOP_VALI(tokvals))
	    

	case IDENTIFIER, STRING:
	    call strcpy (LOP_VALC(tokvals), Memc[column], SZ_COLNAME)

	default:
	    call eprintf ("Column name must be a number or string\n")
	    return
	}
	
	call lcmdcat (igs, NO)
	token = readtok (in, tokvals)

	if (token == CONSTANT) {
	    # Second argument:  ROW value of array in 3-D table
	    if (LOP_TYPE(tokvals) == TY_REAL) {
		# Set both the first row and last row values to same value
		# This will signify desire to read in array from this element
		MG_FROW(igps) = LOP_VALR(tokvals)
		MG_LROW(igps) = LOP_VALR(tokvals)
	    } else {
		MG_FROW(igps) = real (LOP_VALI(tokvals))
		MG_LROW(igps) = real (LOP_VALI(tokvals))
	    }
		call lcmdcat (igs, NO)
	}
	
	call igtbcol (cmd, igps, MG_FILE_NAME(igps), Memc[column])
	call cmdcat  (igs, NO)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Data read from rows %d to %d ")
	    call pargi (MG_FROW(igps))
	    call pargi (MG_LROW(igps))
	    call eprintf ("of column %s in table %s \n")
	    call pargstr (Memc[column])
	    call pargstr (MG_FILE_NAME(igps))
	}

	call sfree (sp)

	#call lcmdcat (igs, NO)
end



procedure igtxcol (cmd, igps, colnum, fd)

int	cmd
pointer	igps
int	colnum
int	fd

begin
	switch (cmd) {
	case XCOLUMN:
	    call igrcolumn (fd, colnum, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_XDATAP(igps), MG_XNPTS(igps))
	    MG_XLOG(igps) = NO

	case YCOLUMN:
	    call igrcolumn (fd, colnum, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_YDATAP(igps), MG_YNPTS(igps))
	    MG_YLOG(igps) = NO

	case ECOLUMN:
	    call igrcolumn (fd, colnum, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_EDATAP(igps), MG_ENPTS(igps))

	case PCOLUMN:
	    call igrcolumn (fd, colnum, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_PDATAP(igps), MG_PNPTS(igps))

	case SCOLUMN:
	    call igrcolumn (fd, colnum, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_SDATAP(igps), MG_SNPTS(igps))

	case LCOLUMN:
	    call igrcolumn (fd, colnum, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_LDATAP(igps), MG_LNPTS(igps))
	}

	MG_COLNUM(igps) = colnum
end



procedure igtbcol (cmd, igps, table, column)

int	cmd
pointer	igps
char	table[ARB], column[ARB]

char	line[SZ_LINE]

begin
	switch (cmd) {

	case XCOLUMN:
	    # X data
	    call gstcol (table, column, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_XDATAP(igps), MG_XNPTS(igps), 
		MG_XLABEL(igps))
	    MG_XLOG(igps) = NO

	case YCOLUMN:
	    # Y data
	    call gstcol (table, column, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_YDATAP(igps), MG_YNPTS(igps), 
		MG_YLABEL(igps))
	    MG_YLOG(igps) = NO

	case ECOLUMN:
	    # Errors
	    call gstcol (table, column, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_EDATAP(igps), MG_ENPTS(igps), 
		line)

	case PCOLUMN:
	    # Point marker styles
	    call gstcol (table, column, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_PDATAP(igps), MG_PNPTS(igps), 
		line)

	case SCOLUMN:
	    # Point marker styles
	    call gstcol (table, column, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_SDATAP(igps), MG_SNPTS(igps), 
		line)

	case LCOLUMN:
	    # Point marker styles
	    call gstcol (table, column, 
		MG_FROW(igps), MG_LROW(igps), 
		MG_LDATAP(igps), MG_LNPTS(igps), 
		line)
	}

	call strcpy (column, MG_COLNAME(igps), SZ_LINE)
end



#  IGRCOLUMN -- Read a column of data from the input list into a data vector.

# Modified to recognize comments (lines starting with "#")
# 1990 August 8, Z. G. Levay, STScI
# 2/4/91 Modified to parse text columns as whitespace-delimited words
# rather than numbers to avoid problems with invalid numerical values.
# ZGL

procedure igrcolumn (fd, colnum, frow, lrow, data, npts)

pointer	fd		# Input file descriptor
int	colnum		# Column number
int	frow, lrow	# Range of rows to read
pointer	data		# Data array
int	npts		# Number of points

int	inrow, outrow
int	col
real	value
pointer	sp, line
int	ip
pointer	word
int	wip

int	getline(), ctor(), ctowrd()

begin
	if (data == NULL) {
	    # No space allocated;
	    # Allocate scratch space
	    npts = DATA_SIZE
	    call malloc (data, npts, TY_REAL)
	}

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (word, SZ_LINE, TY_CHAR)

	inrow  = 0
	outrow = 0

	while (getline (fd, Memc[line]) != EOF
		&& (IS_INDEFI (lrow) || inrow < lrow)) {

	    # Read an input file row
	    inrow = inrow + 1
	    if (IS_INDEFI (frow) || inrow >= frow) {
		# Read the row
		for (ip = line;  IS_WHITE(Memc[ip]);  ip = ip + 1)
		    # Strip white space from BOL
		    ;

		if (Memc[ip] == '#')
		    # Skip comments
		    next

		# Skip blank lines.
		if (Memc[ip] == EOS)
		    next

		outrow = outrow + 1
		if (outrow > npts) {
		    # Ran out of room;  Allocate more scratch space
		    npts = npts + DATA_SIZE
		    call realloc (data, npts, TY_REAL)
		}

		for (col = 1; col <= colnum; col = col + 1) {
		    # Find the right column
		    if (ctowrd (Memc, ip, Memc[word], SZ_LINE) != 0) {
			# We use ctowrd to circumvent some parsing problems
			# (I hope)
			wip = word
			if (ctor (Memc, wip, value) == 0)
			    value = INDEFR
		    }
		}

		# Pop the cell into the data array
		Memr[data+outrow-1] = value
	    }
	}

	npts = outrow
	# Resize the data buffer
	call realloc (data, npts, TY_REAL)

#	if (IS_INDEFI (frow))
#	    frow = 1
#
#	if (IS_INDEFI (lrow))
#	    lrow = npts

	call sfree (sp)
end



#  GSTCOL -- Read a data vector from a column in an SDAS table.  Read the 
#  range of rows between frow and lrow.  Build an axis label from the 
#  column name and column units from the table.

procedure gstcol (table, column, ifrow, ilrow, data, npts, label)

char	table[SZ_FNAME]		# Table name
char	column[SZ_COLNAME]	# Data column name
int	ifrow, ilrow		# Range of rows to read
pointer	data			# Data array
int	npts			# Number of points
char	label[SZ_LINE]		# Axis label

int	frow, lrow		# Range of rows to read
pointer	tdp			# Pointers to table descriptor
pointer	cdp			# Pointers to column descriptors
pointer	sp
pointer	scrdat			# Pointer to scratch data vector
pointer	null			# Pointer to null vector
pointer	colunit			# Column units
int	inrows
int	numrows			# Number of column rows
int	numcols			# Number of table columns

char	root[SZ_FNAME]		# Table root name
char	rowselect[SZ_FNAME]	# 3-D Table row selector
char	colselect[SZ_FNAME]	# 3-D Table column selector

int	row			# Row number of 3-D element
int	datatype		# data type of column
int	nelem			# input length of array, output number of rows
int	ncopy			# number of elements to copy at once
int	first			# first and last elements (or rows)
pointer	x			# pointer to temp array of input data from 3-D tables
char	tmptab[SZ_FNAME]

pointer	tbtopn()
int	tbpsta(), strlen()
int	tbagtr(), tbagtd(), tbagti(), tbagts()
int	tbcigi()

begin
	numcols = 1

	call strcpy(table, tmptab, SZ_FNAME)

	call sprintf(colselect,SZ_FNAME,"[c:%s]")
		call pargstr(column)

	call strcat (colselect, tmptab, SZ_FNAME)
	call rdselect(tmptab, root, rowselect, colselect, SZ_FNAME) 

	# Open the table
	tdp = tbtopn (root, READ_ONLY, 0)

	# Check if the column exists
	call tbcfnd (tdp, column, cdp, numcols)
	
	if (cdp <= 0) {
	    call eprintf ("Cannot find column %s in table %s ")
		call pargstr (column)
		call pargstr (table)
	    return
	}

	# Number of table rows
	numrows = tbpsta (tdp, TBL_NROWS)

	if (IS_INDEFI (ifrow))
	    frow = 1
	else 
	    frow = ifrow

	if (IS_INDEFI (ilrow))
	    lrow = numrows
	else
	    lrow = ilrow

	# Allocate space for the table column and null flag
	call smark (sp)
	call salloc (colunit, SZ_COLUNITS, TY_CHAR)
	
	# We are working with a 3-D table, so extract column from 
	# element specified by column name and row number
	if (frow == lrow || rowselect[1] != EOS) {

		# Get number of elements to copy.
		nelem = tbcigi (cdp, TBL_COL_LENDATA)		
		ncopy = nelem
		first = 1
		call malloc (scrdat, ncopy, TY_REAL)

		if(rowselect[1] == EOS) {

		# No row selector provided, so just use row number given with
		# column command...
		#
		row = lrow
	
		# Copy the data.
		datatype = tbcigi (cdp, TBL_COL_DATATYPE)
		if (datatype == TY_REAL) {		
			if (tbagtr (tdp, cdp, row, Memr[scrdat], first, ncopy) < ncopy)
			    call igta_disaster (tdp, "error reading input")
	
		} else if (datatype == TY_DOUBLE) {

		    call salloc (x, ncopy, TY_DOUBLE)

			if (tbagtd (tdp, cdp, row, Memd[x], first, ncopy) < ncopy) {
			    call igta_disaster (tdp, "error reading input")
			}
		        call achtdr(Memd[x],Memr[scrdat], ncopy)
	
		} else if (datatype == TY_INT) {
		    call salloc (x, ncopy, TY_INT)

			if (tbagti (tdp, cdp, row, Memi[x], first, ncopy) < ncopy)
			    call igta_disaster (tdp, "error reading input")
		    call achtir(Memi[x],Memr[scrdat],ncopy)

		} else if (datatype == TY_SHORT) {
		    call salloc (x, ncopy, TY_SHORT)

			if (tbagts (tdp, cdp, row, Mems[x], first, ncopy) < ncopy)
			    call igta_disaster (tdp, "error reading input")
		    call achtsr(Mems[x],Memr[scrdat],ncopy)

		} else if (datatype == TY_BOOL) {
			    call igta_disaster (tdp, "error reading input: Data type wrong (BOOL)")			
		} else if (datatype < 0) {		# character string
			    call igta_disaster (tdp, "error reading input: Data type wrong (CHAR)")	
		} else {
		    call igta_disaster (tdp, "unknown data type")
		}
	
		inrows = ncopy

		} else {
		# Row selector selected...
		# If no row number provided in column command, then assume row = 1
		if (frow == lrow) 
			row = frow 
		else 
			row = 1

		# Row selector provided, so use new function to read in 
		# relative row number given in 'column' command...
			call rd_relrow (tdp, colselect, rowselect, row, scrdat, nelem)
			inrows = nelem
		} 
	} else {
	    # working with a standard SDAS or text table, so work as usual...
	    call malloc (scrdat, numrows, TY_REAL)
	    call salloc (null, numrows, TY_BOOL)

	    # Read the column
	    call tbcgtr (tdp, cdp, Memr[scrdat], Memb[null], 1, numrows)

	    inrows = lrow - frow + 1
	}

	if (data == NULL)
	    # No data vector allocated;  allocate it
	    call malloc (data, inrows, TY_REAL)
	else if (inrows != npts)
	    # Not enough space allocated for data vector;  reallocate it
	    call realloc (data, inrows, TY_REAL)

	if (frow == lrow) {	
	# If we are working with a 3-d table, copy entire array over	
		call amovr (Memr[scrdat], Memr[data], inrows)
	} else {
	# If we are working with an SDAS table, copy over only those
	# rows specified in 'lines' command, starting with 'frow'
		call amovr (Memr[scrdat+frow-1], Memr[data], inrows)
	}		


	# Build the axis label from the column name
	call sprintf (label, SZ_LINE, "%s")
	    call pargstr (column)

	# Find the column units 
	call tbcigt (cdp, TBL_COL_UNITS, Memc[colunit], SZ_COLUNITS)
	if (Memc[colunit] != EOS) {
	    # Column units exist;  append to axis label
	    call sprintf (label[strlen (label)+1], SZ_LINE, " [%s]")
	    call pargstr (Memc[colunit])
	}

	# Close the table
	call tbtclo (tdp)
	call mfree (scrdat,TY_REAL)
	call sfree (sp)
	npts = inrows 
end

# igta_disaster -- clean up and call error
 
procedure igta_disaster (itp, message)
 
pointer itp        # io: pointers to table struct
char    message[ARB]    # i: error message

begin
        call tbtclo (itp)
        call error (1, message)
end
	
