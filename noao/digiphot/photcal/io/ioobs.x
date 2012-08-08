include "../lib/io.h"

# IO_GOBS - Get next observation from a file, either from data in the catalog
# or not. For data found in the catalog return its catalog values at the end of
# the array. Otherwise append INDEF values. If catalog matching is not being
# used don't append anything. Return the number of variables read, or EOF.

int procedure io_gobs (fd, ctable, map, type, vars, nvars, getid, ulineid,
	clineid, maxch)

int	fd			# file descriptor
pointer	ctable			# catalog table
pointer	map			# mapped columns
int	type			# type of object to be processed
real	vars[nvars]		# observations (output)
int	nvars			# number of variables
int	getid			# get the object id
char	ulineid[maxch]		# user's line identifier (output)
char	clineid[maxch]		# compresses line identifier (output)
int	maxch			# max number of chars.

int	i, num, col, ip, tp, index, nread
pointer	sp, line, token, indices, sym
real	rval

#bool	clgetb()
int	ctowrd(), ctor(), pr_findmap(), io_getline(), io_lineid()
pointer	stfind()

begin
	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf ("io_obs.in: (fd=%d) (ctable=%d) (map=%d) ")
		#call pargi (fd)
		#call pargi (ctable)
		#call pargi (map)
	    #call eprintf ("(nvars=%d) (maxch=%d)\n")
		#call pargi (nvars)
		#call pargi (maxch)
	#}

	# Allocate working space.
	call smark (sp)
	call salloc (line, MAX_CONT * SZ_LINE, TY_CHAR)
	call salloc (token, SZ_LINE, TY_CHAR)
	call salloc (indices, nvars, TY_INT)

	# Loop reading lines until the next desired data is found.
	# Return EOF if there are no more lines, and the number of
	# variables read otherwise.

	repeat {

	    # Get next line from file
	    if (io_getline (fd, Memc[line], MAX_CONT * SZ_LINE) == EOF) {
		call sfree (sp)
		return (EOF)
	    }

	    #if (clgetb ("debug.iocode")) {
		#call eprintf ("[%s]\n") 
		    #call pargstr (Memc[line])
	    #}

	    # Get the line id if catalog matching is being used.
	    ip = 1
	    if (getid == YES) {
		if (io_lineid (Memc[line], ip, ulineid, clineid, maxch) == 0)
		    next
		col = 2
	    } else {
		col = 1
		ulineid[1] = EOS
	    }

	    # Break the loop when the appropiate data type is found.
	    # This is always the case when all the data type is selected,
	    # or no catalog matching is being used.

	    if (ctable == NULL) {
	        sym = NULL
		break
	    } else {
		sym = stfind (ctable, clineid)
		if (type == TYPE_ALL)
		    break
		else if ((type == TYPE_PROGRAM) && (sym == NULL))
		    break
		else if  ((type == TYPE_STANDARDS) && (sym != NULL))
		    break
	    }
	}

	# Initialize the variables array to INDEF.
	call amovkr (INDEFR, vars, nvars)

	# Scan input colums and get all observational variable values.
	nread = 0
	while (ctowrd (Memc[line], ip, Memc[token], SZ_LINE) > 0) {

	    # Enter variable value into symbol table if it was declared
	    # in the configuration file.

	    tp = 1
	    if (ctor (Memc[token], tp, rval) > 0) {
	        num = pr_findmap (map, col, Memi[indices], nvars)
		do i = 1, num {
		    index = Memi[indices+i-1]
	            if (IS_INDEFI (index))
		        next
	    	    if (index > nvars)
		        call error (0, "Array index out of bounds (io_gobs)")
		    vars[index] = rval
		    nread = max (nread, index)
		}
	    } else if (col == 1 && getid == YES)
		call strcpy (Memc[token], ulineid, SZ_LINE)

	    # Count the input columns.
	    col = col + 1
	}

	# If catalog matching is being used append to the output array
	# all the catalog values from the catalog table. If these values
	# are not defined (not found in the catalog) append INDEF values.

	if (sym != NULL) {
	    do num = nread + 1, nvars
		vars[num] = Memr[P2R(sym+num-nread-1)]
	    nread = nvars
	}

	# Debug ?
	#if (clgetb ("debug.iocode")) {
	    #call eprintf ("io_obs.out: (nread=%d)")
		#call pargi (nread)
	    #do ip = 1, nread {
		#call eprintf (" (%g)")
		    #call pargr (vars[ip])
	    #}
	    #call eprintf (" (ulineid=%s) (clineid=%s)\n")
		#call pargstr (ulineid)
		#call pargstr (clineid)
	#}

	call sfree (sp)

	# Return the number of variables read.
	return (nread)
end
