include <chars.h>
include <ctype.h>
include "tbltext.h"

# tbzlin -- get next line from text file
# This routine calls getlline (get long line) to get a line from a text file.
# The line_type indicates what type of line (comment, data, etc) was read.
# If the input line is data rather than a comment, the returned line will
# be terminated by EOS rather than by '\n'.  If the line is a comment, the
# entire line (including newline) is returned.  In the case of in-line
# comments, if there are non-whitespace characters preceding the '#' they
# will be returned, and the '#' will be replaced by EOS.  A '#' which is
# enclosed in quotes (single or double) or preceded by '\' will not be
# counted as a comment character.  A newline terminates the string regardless
# of what precedes or follows it; a newline within a string is an error,
# though.  The function value is the number of characters preceding the EOS,
# or EOF is returned when the end of file is reached.
#
# The buffer is scanned to be sure it contains a '\n'.  If not, we haven't
# read the entire line (i.e. it's longer than maxch); we regard that as
# a serious error.
#
# Phil Hodge,  6-Feb-1992  Function created.
# Phil Hodge, 19-Jan-1995  Allow continuation lines.
# Phil Hodge, 26-Dec-1995  Errchk getlline.
# Phil Hodge, 21-Apr-1999  Change error message if newline not found;
#		allow last line of file to not have a newline.
# Phil Hodge,  7-Jun-1999  Change last argument from comment flag (bool)
#		to line type (int); add line, and increment.

int procedure tbzlin (fd, buf, maxch, line, line_type)

int	fd		# i: fd for the file
char	buf[ARB]	# o: buffer for the line that was read
int	maxch		# i: size of line buffer
int	line		# io: incremented each time a line is read from file
int	line_type	# o: type of line returned in buf
#--
pointer sp
pointer scratch
char	ch		# a character from the string which was read
int	ip		# counter for a character in the string
int	nchar		# number of char read by getlline
bool	done		# loop-termination flag
bool	at_end		# true when we reach the end of the line ('\n')
bool	odd_squotes	# true if current character is within quoted string
bool	odd_dquotes	# true if current char is within double quoted string
int	getlline(), strlen(), strncmp()
errchk	getlline

begin
	line_type = COMMENT_LINE		# default

	nchar = getlline (fd, buf, maxch)
	if (nchar == EOF)
	    return (EOF)			# end of file reached
	line = line + 1

	# Check for special cases.

	done = false
	if (strncmp (buf, "#k ", 3) == 0 ||
	    strncmp (buf, "#K ", 3) == 0) {
	    line_type = KEYWORD_LINE
	    done = true
	} else if ((strncmp (buf, "#c ", 3) == 0 ||
		    strncmp (buf, "#C ", 3) == 0) && nchar > 4) {
	    # the test on nchar is because a column name must be specified
	    line_type = COLDEF_LINE
	    done = true
	}
	if (done) {
	    if (buf[nchar] == NEWLINE) {
		buf[nchar] = EOS
		nchar = nchar - 1
	    }
	    return (nchar)
	}

	# Blank up through maxch?  Treat it as a comment.
	ip = 1
	while (IS_WHITE(buf[ip]) && ip < maxch)		# skip whitespace
	    ip = ip + 1
	if (ip >= maxch)
	    line_type = COMMENT_LINE
	else if (buf[ip] == NEWLINE || buf[ip] == '#' || buf[ip] == EOS)
	    line_type = COMMENT_LINE
	else
	    line_type = DATA_LINE

	# If there's no newline, and if this is not the last line of the
	# file, we haven't read the entire line (i.e. it's longer than the
	# buffer size).
	# Also check whether the newline has been escaped, in which case
	# we need to read further lines and concatenate them.
	done = false
	while (!done) {
	    ip = strlen (buf)
	    if (buf[ip] != NEWLINE) {
		# The last line of the file need not have a newline.
		call smark (sp)
		call salloc (scratch, 2*SZ_LINE, TY_CHAR)
		nchar = getlline (fd, Memc[scratch], 2*SZ_LINE)
		if (nchar != EOF) {
		    call error (1,
			"Unknown table type, or line too long for text file.")
		}
		call sfree (sp)
		break
	    }
	    done = true				# may be reset below
	    if (ip > 1) {
		if (buf[ip-1] == ESCAPE) {	# newline is escaped
		    done = false
		    # Read another line, clobbering the '\' and newline.
		    ip = ip - 1			# now points to the '\'
		    nchar = getlline (fd, buf[ip], maxch-ip)
		    line = line + 1

		    if (line_type == COMMENT_LINE) {
			# Remove comment character from continuation line.
			while (IS_WHITE(buf[ip]) && ip < maxch)
			    ip = ip + 1
			if (buf[ip] == '#')
			    buf[ip] = ' '
		    }
		}
	    }
	}

	if (line_type != COMMENT_LINE) {

	    # The current line is not a comment or blank;
	    # replace '\n' or '#' by EOS.

	    # Skip leading whitespace and check whether the first character
	    # begins a quoted string.
	    ip = 1
	    while (IS_WHITE(buf[ip]) && ip < maxch)
		ip = ip + 1
	    if (buf[ip] == SQUOTE)
		odd_squotes = true
	    else
		odd_squotes = false
	    if (buf[ip] == DQUOTE)
		odd_dquotes = true
	    else
		odd_dquotes = false

	    ip = ip + 1
	    at_end = false
	    while ( !at_end ) {
		ch = buf[ip]
		# Check for end of buffer or newline or in-line comment.
		if (ch == NEWLINE) {
		    buf[ip] = EOS
		    at_end = true
		} else if (ch == SQUOTE) {
		    # Toggle flag for in/out of quoted string.
		    odd_squotes = !odd_squotes
		    ip = ip + 1
		} else if (ch == DQUOTE) {
		    odd_dquotes = !odd_dquotes
		    ip = ip + 1
		} else if (ch == '#') {
		    # '#' is not a comment if it's in a quoted string
		    if (odd_squotes || odd_dquotes) {
			ip = ip + 1
		    #	... or if it's escaped.
		    } else if (buf[ip-1] == ESCAPE) {
			ip = ip + 1
		    } else {			# it's an in-line comment
			buf[ip] = EOS
			at_end = true
		    }
		} else if (ch == EOS) {
		    at_end = true
		} else if (ip >= maxch) {	# end of buffer reached
		    at_end = true		# (can't get here)
		    buf[maxch+1] = EOS
		} else {
		    ip = ip + 1
		}
	    }
	    if (odd_squotes || odd_dquotes)
		call error (1, "tbzlin:  newline within string")
	    nchar = ip - 1
	}
	return (nchar)
end
