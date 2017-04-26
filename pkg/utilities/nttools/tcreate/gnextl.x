include <chars.h>
include <ctype.h>

# g_next_l -- get next line from text file
# This routine calls getlline (get long line) to get a line from a text file.
# A flag is set to true if the line is a comment rather than a data line.
# The returned line will be terminated by EOS rather than by '\n'.
# If the line is a comment, the line will be skipped.  In-line comments
# are stripped off.  A '#' which is enclosed in quotes (single or double)
# or preceded by '\' will not be
# counted as a comment character.  A newline terminates the string regardless
# of what precedes or follows it; a newline within a string is an error,
# though.  The function value is the number of characters preceding the EOS,
# or EOF is returned when the end of file is reached.
# The linenum I/O parameter is incremented each time a line is read,
# regardless of whether the line is data, a comment, or blank.
#
# The buffer is scanned to be sure it contains a '\n'.  If not, we haven't
# read the entire line (i.e. it's longer than maxch); we regard that as
# a serious error.
#
# Phil Hodge, 22-May-1992  Function created based on tbzlin.
# Phil Hodge, 10-May-1993  Include line number in error messages.

int procedure g_next_l (fd, buf, maxch, linenum)

int	fd		# i: fd for the file
char	buf[ARB]	# o: buffer to receive the line that was read
int	maxch		# i: size of line buffer
int	linenum		# io: line number counter
#--
pointer sp
pointer errmes		# scratch for error message
char	ch		# a character from the string which was read
int	ip		# counter for a character in the string
int	nchar		# number of char read by getlline
bool	done		# flag for terminating loop
bool	comment		# is current line commented out?
bool	at_end		# true when we reach the end of the line ('\n')
bool	odd_squotes	# true if current character is within quoted string
bool	odd_dquotes	# true if current char is within double quoted string
int	getlline()

begin
	done = false
	# This loop is terminated by EOF or by reading a line that
	# is not a comment or blank.
	while ( !done ) {

	    nchar = getlline (fd, buf, maxch)
	    if (nchar == EOF)
		return (EOF)			# end of file reached

	    linenum = linenum + 1		# increment line number counter

	    # If there's no newline, we haven't read the entire line.
	    at_end = false
	    do ip = 1, maxch {
		if (buf[ip] == EOS) {
		    break
		} else if (buf[ip] == '\n') {
		    at_end = true			# we've read entire line
		    break
		}
	    }
	    if (!at_end) {
		call smark (sp)
		call salloc (errmes, SZ_LINE, TY_CHAR)
		call sprintf (Memc[errmes], SZ_LINE,
			"Input line %d is too long.")
		    call pargi (linenum)
		call error (1, Memc[errmes])
	    }

	    ip = 1
	    while (IS_WHITE(buf[ip]) && ip < maxch)
		ip = ip + 1

	    # Blank up through maxch?  Treat it as a comment.
	    if (ip >= maxch) {
		comment = true

	    } else if (buf[ip] == NEWLINE || buf[ip] == '#' || buf[ip] == EOS) {
		comment = true

	    } else {
		comment = false

		# The current line is not a comment or blank;
		# replace '\n' or '#' by EOS.  First check whether the
		# first non-blank character begins a quoted string.
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
			} else {		# it's an in-line comment
			    buf[ip] = EOS
			    at_end = true
			}
		    } else if (ch == EOS) {
			at_end = true		# (can't get here)
		    } else if (ip >= maxch) {	# end of buffer reached
			at_end = true		# (can't get here)
			buf[maxch+1] = EOS
		    } else {
			ip = ip + 1
		    }
		}
		if (odd_squotes || odd_dquotes) {
		    call smark (sp)
		    call salloc (errmes, SZ_LINE, TY_CHAR)
		    call sprintf (Memc[errmes], SZ_LINE,
			    "Unbalanced quotes in line %d.")
			call pargi (linenum)
		    call error (1, Memc[errmes])
		}
		nchar = ip - 1
	    }

	    # If the line is a comment (or blank), read another line.
	    if (!comment)
		done = true
	}
	return (nchar)
end
