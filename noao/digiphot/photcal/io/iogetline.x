include	"../lib/io.h"


# IO_GETLINE -- Get an input line from the data file. The line returned may
# be composed by one or more physical lines in the output file, if subsequent
# lines start with a continuation character. The continuation character must
# be the first character in the subsequent lines. Comment and blank lines
# are skipped.

int procedure io_getline (fd, line, maxch)

int	fd				# file descriptor
char	line[maxch]			# line from file
int	maxch				# line size

bool	first				# first line ?
int	pending				# pending line in buffer ?
char	buffer[SZ_LINE]			# line buffer

common	/iogetcom/	pending

int	fscan()
int	strlen(), strmatch()

begin
	# Initialize flag to differentiate the first input line
	# within the loop.
	first = true

	# Read lines until a non-comment and non-blank line is found, or
	# the end of the file is reached. Lines starting with a continuation
	# character are concatenated.
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

	    # Skip blank and comment lines 
	    if (strlen (buffer) == 0)
	        next
	    if (strmatch (buffer, COMMENT) != 0)
	        next

	    # If the input line contains a continuation character, then
	    # concatenate it to the accumulated line. Otherwise, leave
	    # it in the buffer, and set the pending flag. For the first
	    # input line no continuation characters are allowed.
	    if (first) {
		if (strmatch (buffer, CONTINUATION) != 0)
		    call error (0, "Continuation character found in first line")
		else {
		    call strcpy (buffer, line, maxch)
		    first = false
		    next
		}
	    } else {
		if (strmatch (buffer, CONTINUATION) != 0) {
		    call strcat (buffer[2], line, maxch)
		    next
		} else {
		    pending = YES
		    return (OK)
		}
	    }
	}
end


# IO_GETLINE_INIT -- Initialize get line.

procedure io_getline_init ()

int	pending				# pending line in buffer ?

common	/iogetcom/	pending

begin
	pending = NO
end
