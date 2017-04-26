include <tbset.h>	# for subtype codes
include "tbtables.h"
include "tbltext.h"

# tbzsub -- determine text table subtype
#
# Phil Hodge,  7-Jun-1999  Subroutine created.

procedure tbzsub (tp, buf, maxch, line, line_type, subtype)

pointer tp		# i: pointer to table info structure
char	buf[ARB]	# o: buffer for the line that was read
int	maxch		# i: size of line buffer
int	line		# io: line number in input file
int	line_type	# o: type of line read by tbzlin
int	subtype		# o: subtype of text table
#--
int	fd		# fd for the file
bool	read_enough	# true if we have read enough to determine the subtype
int	nchar, tbzlin()
errchk	tbzlin

begin
	fd = TB_FILE(tp)
	read_enough = false
	subtype = TBL_SUBTYPE_UNKNOWN

	while (!read_enough) {

	    nchar = tbzlin (fd, buf, maxch, line, line_type)

	    if (nchar == EOF) {
		subtype = TBL_SUBTYPE_SIMPLE
		read_enough = true
		buf[1] = EOS

	    } else if (line_type == COLDEF_LINE) {
		subtype = TBL_SUBTYPE_EXPLICIT
		read_enough = true

	    } else if (line_type == KEYWORD_LINE) {
		call tbzkey (tp, buf, 0)	# append to list of keywords
		buf[1] = EOS			# done with this line

	    } else if (line_type == COMMENT_LINE) {
		call tbbcmt (tp, buf)		# append to comment buffer
		buf[1] = EOS			# done with this line

	    } else {					# data
		subtype = TBL_SUBTYPE_SIMPLE
		read_enough = true
	    }
	}
end
