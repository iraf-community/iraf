include <ctype.h>		# for IS_WHITE
include "tbtables.h"
define	INCR_BUFFSIZE	1024	# minimum amount by which buffer is increased

# tbbcmt -- append comment line to buffer
# This routine takes a line of text and appends it to the comment buffer for
# the current text table.  If the line of text does not begin with "#", then
# "# " will first be appended to the comment buffer.  If the line is not
# terminated with a newline, a newline will be appended to the comment buffer
# after appending the line.  Since each comment line ends with a newline,
# if we print (or write to a file) the full comment buffer, it should come out
# on several lines.
# If the comment buffer is not long enough it will be reallocated.

# Phil Hodge,  6-Mar-1992  Subroutine created.
# Phil Hodge, 22-Apr-1994  Check to be sure TB_COMMENT is not NULL;
#			prefix with # and/or append newline if necessary.
# Phil Hodge, 11-May-1994  Just return if input string is empty.

procedure tbbcmt (tp, buf)

pointer tp			# i: pointer to table descriptor
char	buf[ARB]		# i: string to be appended to comment buffer
#--
int	len1			# length of string to be appended
int	len2			# length of comment buffer
int	ip			# for ignoring leading whitespace
int	new_sz_comment		# new value of allocated buffer length
int	strlen()

begin
	if (buf[1] == EOS)
	    return

	len1 = strlen (buf)

	if (TB_COMMENT(tp) == NULL) {

	    # Allocate the comment buffer.
	    new_sz_comment = len1 + INCR_BUFFSIZE
	    call malloc (TB_COMMENT(tp), new_sz_comment, TY_CHAR)
	    TB_SZ_COMMENT(tp) = new_sz_comment

	} else {

	    len2 = strlen (Memc[TB_COMMENT(tp)])

	    # If the combined length is too long, reallocate the comment buffer.
	    if (len1 + len2 > TB_SZ_COMMENT(tp)) {
		new_sz_comment = TB_SZ_COMMENT(tp) + len1 + INCR_BUFFSIZE
		call realloc (TB_COMMENT(tp), new_sz_comment, TY_CHAR)
		TB_SZ_COMMENT(tp) = new_sz_comment
	    }
	}

	# Does the line of text begin with "#"?
	ip = 1
	while (IS_WHITE(buf[ip]))
	    ip = ip + 1
	if (buf[ip] != '#' && buf[ip] != EOS && buf[ip] != '\n')
	    call strcat ("# ", Memc[TB_COMMENT(tp)], TB_SZ_COMMENT(tp))

	# Append the string to the comment buffer.
	call strcat (buf, Memc[TB_COMMENT(tp)], TB_SZ_COMMENT(tp))

	# Not newline terminated?
	if (buf[len1] != '\n')
	    call strcat ("\n", Memc[TB_COMMENT(tp)], TB_SZ_COMMENT(tp))
end
