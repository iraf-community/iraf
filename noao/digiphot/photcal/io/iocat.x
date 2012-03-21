include	<error.h>
include <time.h>
include	"../lib/io.h"


# IO_GCATDAT - Get catalog data from a list of files. These data will be
# stored in memory as a symbol table for later use.

procedure io_gcatdat (catdir, list, ctable, ncat, nvars)

char	catdir[ARB]		# name of the catalog directory
int	list			# file list
pointer	ctable			# catalog table (output)
int	ncat			# number of table entries (output)
int	nvars			# number of catalog variables

int	i, fd, num, col, ip, tp, index
pointer	sp, input, fname, line, token, dummy, indices, sym, map
real	rval

#bool	clgetb()
int	fntgfnb(), access(), ctowrd(), ctor(), open(), stnsymbols()
int	pr_findmap(), io_getline(), io_lineid() 
pointer	stopen(), stfind(), stenter()

begin
	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf ("io_gcatdat.in: (list=%d) (nvars=%d)\n")
		#call pargi (list)
		#call pargi (nvars)
	#}

	# Map catalog variables.
	call pr_catmap (map, nvars)

	# Open a symbol table for catalog data.
	ctable = stopen ("catalog", 2 * LEN_CATDAT, LEN_CATDAT,
	    10 * LEN_CATDAT)

	# Allocate temporary space.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (line, MAX_CONT * SZ_LINE, TY_CHAR)
	call salloc (token, SZ_LINE, TY_CHAR)
	call salloc (dummy, SZ_LINE, TY_CHAR)
	call salloc (indices, nvars, TY_INT)

	# Read the catalog data.
	call fntrewb (list)
	while (fntgfnb (list, Memc[input], SZ_FNAME) != EOF) {

	    # Create the file name.
	    call sprintf (Memc[fname], SZ_FNAME, "%s%s.dat")
		call pargstr (catdir)
		call pargstr (Memc[input])
	    if (access (Memc[fname], READ_ONLY, TEXT_FILE) == NO)
		call strcpy (Memc[input], Memc[fname], SZ_FNAME)

	    # Try to open the input file.
	    iferr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
		call erract (EA_WARN)
		next
	    }

	    # Read the file lines.
	    call io_getline_init ()
	    while (io_getline (fd, Memc[line], MAX_CONT * SZ_LINE) != EOF) {

		# Get the line id from the first column.
		ip = 1
		if (io_lineid (Memc[line], ip, Memc[dummy], Memc[token],
		    SZ_LINE) == 0)
		    next

		# Enter the line identifier in the symbol table.
		if (stfind (ctable, Memc[token]) == NULL) {
		    sym = stenter (ctable, Memc[token], nvars)
		    call amovkr (INDEFR, Memr[P2R(sym)], nvars)
		} else
		    next

		# Get the values from the next columns.
		col = 2
		while (ctowrd (Memc[line], ip, Memc[token], SZ_LINE) > 0) {

		    # Enter value into symbol table if it was declared in the
		    # configuration file.

		    tp = 1
		    if (ctor (Memc[token], tp, rval) > 0) {
		        num = pr_findmap (map, col, Memi[indices], nvars)
			do i = 1, num {
			    index = Memi[indices+i-1]
		            if (! IS_INDEFI (index))
			        Memr[P2R(sym+index-1)] = rval
			}
		    }

		    # Count columns
		    col = col + 1
		}
	    }

	    # Close file
	    call close (fd)
	}

	# Get number of entries.
	ncat = stnsymbols (ctable, 0)

	# Free mapped variables.
	call pr_unmap (map)

	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf ("io_gcatdat.out: (ctable=%d) (ncat=%d) (nvars=%d)\n")
		#call pargi (list)
		#call pargi (ncat)
		#call pargi (nvars)
	#}
	#call dg_dcatdat ("from io_gcatdat", ctable, nvars)

	call sfree (sp)
end


# IO_GCATOBS - Get catalog observations from a list of files, and store them
# in a multicolumn table. The catalog data will be appended to the last
# columns of the table, so each line will be "complete".

procedure io_gcatobs (list, ctable, ncatvars, getid, logfile, otable, ntable,
	nobs)

int	list			# file list
pointer	ctable			# catalog table
int	ncatvars		# number of catalog variables
int	getid			# match the ids
char	logfile[ARB]		# output log file
pointer	otable			# catalog observations table (output)
pointer	ntable			# name table (output)
int	nobs			# number of observations (output)

char	eoschar
int	i, log, fd, nvars, num, col, ip, tp, index
pointer	sp, input, line, token, dummy, indices, map, sym
real	rval

#bool	clgetb()
int	fntgfnb(), ctowrd(), ctor(), open(), pr_findmap()
int	io_getline(), io_lineid()
pointer	stfind(), mct_getrow()

data	eoschar /EOS/

begin
	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf ("io_gcatobs.in: (list=%d) (ctable=%d) (ncatv=%d)\n")
		#call pargi (list)
		#call pargi (ctable)
		#call pargi (ncatvars)
	#}

	# Map the observational variables.
	call pr_obsmap (map, nvars)

	# Allocate the catalog observation table.
	call mct_alloc (otable, LEN_CATOBS, nvars + ncatvars, TY_REAL)

	# Allocate the star name table.
	if (getid == YES)
	    call mct_alloc (ntable, LEN_CATOBS, SZ_FNAME + 1, TY_CHAR)
	else
	    call mct_alloc (ntable, LEN_CATOBS, 2, TY_CHAR)

	# Open the log file.
	if (logfile[1] == EOS) {
	    log = NULL
	} else {
	    iferr (log = open (logfile, APPEND, TEXT_FILE)) {
		call erract (EA_WARN)
		log = NULL
	    }
	}

	# Allocate working space.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (line, MAX_CONT * SZ_LINE, TY_CHAR)
	call salloc (token, SZ_LINE, TY_CHAR)
	call salloc (dummy, SZ_LINE, TY_CHAR)
	call salloc (indices, nvars, TY_INT)

	# Print banner.
	if (log != NULL)
	    call fprintf (log, "\n#UNMATCHED OBJECTS\n\n")

	# Read the catalog observations.
	nobs = 0
	call fntrewb (list)
	while (fntgfnb (list, Memc[input], SZ_LINE) != EOF) {

	    # Try to open the input file.
	    iferr (fd = open (Memc[input], READ_ONLY, TEXT_FILE)) {
		call erract (EA_WARN)
		next
	    }

	    # Read the file lines.
	    call io_getline_init ()
	    while (io_getline (fd, Memc[line], MAX_CONT * SZ_LINE) != EOF) {

		# Get line id from first column if there is a catalog to
		# match them. Otherwise assume that there is no catalog.

		ip = 1
		if (getid == YES) {
		    if (io_lineid (Memc[line], ip, Memc[dummy], Memc[token],
			SZ_LINE) == 0)
		        next
		    col = 2
		} else
		    col = 1

		# Search for this string in the catalog symbol table if
		# one is defined. If it's not found skip to the next
		# line or observation.

		if (ctable != NULL) {
		    sym = stfind (ctable, Memc[token])
		    if (sym == NULL) {
			if (log != NULL) {
			    call fprintf (log,
			        "File: %s  Object: %s was unmatched\n")
				call pargstr (Memc[input])
				call pargstr (Memc[token])
			}
			next
		    }
		}

		# Count the observations.
		nobs = nobs + 1

		# Add the symbol to the name table.
		if (getid == YES) {
		    call mct_putc (ntable, nobs, 1, eoschar)
		    call strcpy (Memc[token], Memc[mct_getrow(ntable, nobs)],
		        SZ_FNAME)
		} else
		    call mct_putc (ntable, nobs, 1, eoschar)

		# Scan input colums and get all variable values.
		while (ctowrd (Memc[line], ip, Memc[token], SZ_LINE) > 0) {

		    # Enter variable value into the observation table
		    # if it was declared in the configuration file.

		    tp = 1
		    if (ctor (Memc[token], tp, rval) > 0) {
		        num = pr_findmap (map, col, Memi[indices], nvars)
			do i = 1, num {
			    index = Memi[indices+i-1]
		            if (!IS_INDEFI (index))
			        call mct_putr (otable, nobs, index, rval)
			}
		    }

		    # Count input columns
		    col = col + 1
		}

		# Now append to the current row all the variable values
		# from the catalog table for the same line id, if
		# matching is enabled.

		if (ncatvars > 0) {
		    if (ctable == NULL) {
			do num = 1, ncatvars
			    call mct_putr (otable, nobs, nvars + num, INDEFR)
		    } else {
		        do num = 1, ncatvars {
			    rval = Memr[P2R(sym+num-1)]
			    call mct_putr (otable, nobs, nvars + num, rval)
		        }
		    }
		}

	    }

	    # Close file.
	    call close (fd)
	}

	if (log != NULL) {
	    call fprintf (log, "\n")
	    call close (log)
	}

	# Free mapped variables.
	call pr_unmap (map)

	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf ("io_gcatobs.out: (otable=%d) (nobs=%d)\n")
		#call pargi (otable)
		#call pargi (nobs)
	#}
	#call dg_dcatobs ("from io_gcatobs", otable)

	call sfree (sp)
end


# IO_LOGTIME -- Write a time stamp in the unmatched stars log file.

procedure io_logtime (logfile)

char	logfile[ARB]	# the name of the log file

int	log
pointer	sp, timestr
int	open()
long	clktime()

begin
	if (logfile[1] == EOS)
	    return

	iferr (log = open (logfile, APPEND, TEXT_FILE)) {
	    call erract (EA_WARN)
	    return
	}

	call smark (sp)
	call salloc (timestr, SZ_TIME, TY_CHAR)
	call cnvtime (clktime(0), Memc[timestr], SZ_TIME)
	call strupr (Memc[timestr])
	call fprintf (log, "\n#%s\n") 
	    call pargstr (Memc[timestr])
	call sfree (sp)

	call close (log)

end


# IO_TITLE -- Write the equation title to the logfile

procedure io_title (logfile, title, sym)

char	logfile[ARB]	# the name of the log file
char	title		# title
int	sym		# equation symbol

int	log
int	open()
pointer	pr_xgetname()

begin
	if (logfile[1] == EOS)
	    return

	iferr (log = open (logfile, APPEND, TEXT_FILE)) {
	    call erract (EA_WARN)
	    return
	}

	call fprintf (log, "%s %s\n\n")
	    call pargstr (title)
	    call pargstr (Memc[pr_xgetname(sym)])

	call close (log)
end
