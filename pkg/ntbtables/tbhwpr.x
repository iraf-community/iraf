include <mach.h>
include <ctype.h>	# for IS_WHITE, IS_LOWER, TO_UPPER
include <tbset.h>
include "tbtables.h"

define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)	# size of packed par record

# tbhwpr -- write parameter record
# This procedure takes as input a string containing a header parameter record
# (keyword and value), packs it, and writes the packed string to the table.
#
# This routine supports STSDAS format tables and text tables, but not FITS
# tables.
#
# For text tables, the input string can be in one of two formats, STSDAS
# table format or text table format.  STSDAS format is:
#	keyword tvalue comment
# where t is a data type code.  t is the ninth character, and the value
# begins with the tenth character.
# Text table format is nearly free format, except that it must begin with
# "#k " or "#K ":
#	#k keyword = value comment
#
# Phil Hodge, 14-Feb-1992  Add option for text table type.
# Phil Hodge, 22-Apr-1994  For text table, append to comment buffer.
# Phil Hodge,  7-Jun-1999  Call tbzkey instead of tbbcmt for text table.

procedure tbhwpr (tp, parnum, str)

pointer tp			# i: pointer to table descriptor
int	parnum			# i: number of the parameter to be written
char	str[ARB]		# i: string containing the keyword and value
#--
pointer sp
pointer par			# for reformatting, or for a packed copy of str
int	maxch			# size of str, plus extra space
int	ip, op			# loop indexes
int	ch			# a character in the keyword
bool	done
long	locn			# location for reading in file
int	strlen(), strncmp()
errchk	seek, write, tbzkey

begin
	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {

	    # Allow extra space, for "#k " and two spaces around "=".
	    maxch = max (SZ_FNAME, strlen (str) + 5)
	    call smark (sp)
	    call salloc (par, maxch, TY_CHAR)

	    # Add to list of keywords in memory.

	    if (strncmp (str, "#k ", 3) == 0 || strncmp (str, "#K ", 3) == 0) {

		# str is already in text table format, but make sure the
		# keyword is upper case.
		call strcpy (str, Memc[par], maxch)
		ip = 3				# zero indexed
		while (IS_WHITE(Memc[par+ip]))
		    ip = ip + 1
		done = false
		while (!done) {
		    ch = Memc[par+ip]
		    if (IS_LOWER(ch))
			Memc[par+ip] = TO_UPPER(ch)
		    if (IS_WHITE(ch) || ch == '=' || ch == EOS)
			done = true
		}

	    } else {

		# STSDAS format; prepend "#k ", and replace the data type
		# code with " = ".
		call strcpy ("#k ", Memc[par], maxch)
		op = strlen (Memc[par])			# zero indexed
		do ip = 1, SZ_KEYWORD {
		    Memc[par+op] = str[ip]
		    op = op + 1
		}
		Memc[par+op] = EOS
		call strcat (" = ", Memc[par], maxch)
		call strcat (str[START_OF_VALUE], Memc[par], maxch)
	    }

	    call tbzkey (tp, Memc[par], parnum)
	    call sfree (sp)

	} else {

	    call smark (sp)
	    call salloc (par, SZ_PARREC, TY_CHAR)

	    locn = SZ_PACKED_REC * (parnum - 1) + SZ_SIZINFO + 1

	    call seek (TB_FILE(tp), locn)
	    call strpak (str, Memc[par], SZ_PARREC)
	    call write (TB_FILE(tp), Memc[par], SZ_PACKED_REC)
	    call flush (TB_FILE(tp))

	    call sfree (sp)
	}
end
