include	"../lib/io.h"

# PH_GHDR_RECORD -- Reconstruct a formated header record from an input catalog
# produced  by MKCATALOG by joining lines which compose a single record.
# All header records begin with a '#' followed by a blank character. If
# the header record is empty it is skipped. Header reading is terminated
# when an empty line is encountered. Lines which begin with a '*' in column
# one followed by a blank are part of the previous record.

int procedure ph_ghdr_record (fd, record, sz_hdr_record)

int	fd		# the input file descriptor
char	record[ARB]	# the output record
int	sz_hdr_record	# size of the record

int	stat, nchars, ip, op
pointer	sp, line
int	getline(), strncmp()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	op = 1
	repeat {

	    # Read lines from the catalog. Header reading is terminated
	    # by an empty line.

	    stat = getline (fd, Memc[line])
	    if (stat == EOF) {
		call sfree (sp)
		return (EOF)
	    }
	    if (Memc[line] == EOS || Memc[line] == '\n') {
		call sfree (sp)
		return (EOF)
	    }

	    # Empty records are skipped.
	    if ((strncmp (MKCAT_COMMENTSTR, Memc[line], MKCAT_SZSTR) == 0) &&
		(Memc[line+MKCAT_SZSTR] == '\n'))
		next

	    # Strip off the newline from the record.
	    if (Memc[line+stat-1] == '\n')
	        nchars = stat - 1
	    else
		nchars = stat

	    # Reconstruct the output record. Terminate the reconstruction
	    # when the number of characters copied is the same as the
	    # size of the record plus the comments marker string.

	    if (strncmp (MKCAT_CONTSTR, Memc[line], MKCAT_SZSTR) == 0)
		ip = 1 + MKCAT_SZSTR
	    else
		ip = 1
	    call strcpy (Memc[line+ip-1], record[op], nchars - ip + 1)
	    op = op + nchars - ip + 1

	} until (op > (sz_hdr_record + MKCAT_SZSTR))

	call sfree (sp)

	return (op - 1)
end


# PH_GDRECORD_INIT -- Initialize data record reading.

procedure ph_gdrecord_init ()

int	pending				# pending line in buffer ?

common	/phgetcom/	pending

begin
	pending = NO
end


# PH_GDRECORD -- Get an input data record from a catalog created with
# MKCATALOG. The returned record may be composed by one or more physical
# lines in the catalog file. Lines which begin with a '*' in column 1
# followed by a blank are part of the previous record. Blank records and
# comment records are passed to the output unless they occur in the middle
# of a long record.

int procedure ph_gdrecord (fd, line, maxch)

int	fd				# file descriptor
char	line[maxch]			# line from file
int	maxch				# line size

bool	first				# first line ?
int	pending				# pending line in buffer ?
char	buffer[SZ_LINE]			# line buffer

common	/phgetcom/	pending

int	fscan(), strmatch()

begin
	# Initialize flag to differentiate the first input line
	# within the loop.
	first = true

	# Read lines until the end of the file is reached. Lines starting
	# with a continuation character are concatenated.
	repeat {

	    # Get next line. If there is no pending line already in
	    # the buffer, read a new line from the file. Otherwise,
	    # use the pending line and clear the pending flag.

	    if (pending == NO) {
		if (fscan (fd) != EOF)
	            call gargstr (buffer, SZ_LINE)
		else if (first)
		    return (EOF)
		else
		    return (OK)
	    } else
		pending = NO

	    # If the input line contains a continuation character, then
	    # concatenate it to the accumulated line. Otherwise, leave
	    # it in the buffer, and set the pending flag. For the first
	    # input line no continuation characters are allowed.

	    if (first) {
		if (strmatch (buffer, CONTINUATION) != 0)
		    call error (0,
		"Continuation character found in first line of catalog record")
		else {
		    call strcpy (buffer, line, maxch)
		    first = false
		    next
		}
	    } else {
		if (strmatch (buffer, CONTINUATION) != 0) {
		    if (buffer[1] != EOS && buffer[1] != 'n' && buffer[1] !=
		        '#')
		        call strcat (buffer[3], line, maxch)
		    next
		} else {
		    pending = YES
		    return (OK)
		}
	    }
	}
end


# PH_PDRECORD -- Write the formatted record to the output catalog, breaking
# the record into chunks which hold an integral number of columns and that
# are less that or equal to maxch_perline characters long.

procedure ph_pdrecord (fd, record, sz_record, wcols, ncols, rectype,
	maxch_perline)

int	fd		# the output file descriptor
char	record[ARB]	# the input record
int	sz_record	# the size of the input record
int	wcols[ARB]	# widths of the input record columns
int	ncols		# number of columns
char	rectype[ARB]	# string identifying the output record type
int	maxch_perline	# maximum number of characters output per line

int	i, ip, nlines, length
int	strlen()

begin
	# Initialize.
	nlines = 1
	ip = 1

	# Write out the string defining the record type. The record
	# types are comment, data and blank.
	length = strlen (rectype)
	call fprintf (fd, "%*.*s")
	    call pargi (-MKCAT_SZSTR)
	    call pargi (MKCAT_SZSTR)
	    call pargstr (rectype)

	# Construct the record piece by piece.
	for (i = 1; i <= ncols; i = i + 1) {

	    # Compute the contribution of each field to the length of the
	    # output line which must less than or equal to maxch_perline
	    # characters. Fields may not be broken apart by a continutation
	    # marker.

	    length = length + wcols[i]
	    if (length < maxch_perline)
		next
	    if (length > maxch_perline) {
	        length = length - wcols[i]
		i = i - 1
	    }

	    # Output portions of the record to the catalog file. 
	    # If nlines is greater than 1 prepend the continuations
	    # string to the record.

	    if (nlines == 1) {
	        call fprintf (fd, "%*.*s\n")
		    call pargi (-(length - MKCAT_SZSTR))
		    call pargi (length - MKCAT_SZSTR)
		    call pargstr (record[ip])
		ip = ip + length - MKCAT_SZSTR
	    } else {
	        call fprintf (fd, "%*.*s")
		    call pargi (-MKCAT_SZSTR)
		    call pargi (MKCAT_SZSTR)
		    call pargstr ("* ")
	        call fprintf (fd, "%*.*s\n")
		    call pargi (-(length - MKCAT_SZSTR))
		    call pargi (length - MKCAT_SZSTR)
		    call pargstr (record[ip])
	        ip = ip + length - MKCAT_SZSTR
	    }

	    nlines = nlines + 1
	    length = MKCAT_SZSTR
	}

	# Output any remaining piece of the record.
	if (ip <= sz_record) {
	    if (nlines == 1) {
	        call fprintf (fd, "%s\n")
	            call pargstr (record[ip])
	    } else {
	        call fprintf (fd, "%*.*s%s\n")
	            call pargi (-MKCAT_SZSTR)
	            call pargi (MKCAT_SZSTR)
		    call pargstr (MKCAT_CONTSTR)
	            call pargstr (record[ip])
	    }
	}

end
