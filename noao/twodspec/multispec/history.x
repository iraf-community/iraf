include <time.h>
include	"ms.h"

# HISTORY - Add a dated comment string to the MULTISPEC database.

procedure history (ms, comment)

pointer	ms
char	comment[ARB]

char	time_string[SZ_TIME]

long	clktime()

begin
	# Get the clock time and convert to a date string.
	call cnvdate (clktime(0), time_string, SZ_TIME)

	# Append the following to the comment block:
	# (date string)(: )(comment string)(newline)

	call strcat (time_string, COMMENT(ms,1), SZ_MS_COMMENTS)
	call strcat (": ", COMMENT(ms,1), SZ_MS_COMMENTS)
	call strcat (comment, COMMENT(ms,1), SZ_MS_COMMENTS)
	call strcat ("\n", COMMENT(ms,1), SZ_MS_COMMENTS)

	# Write the updated comment block to the database.
	call mspcomments (ms)
end
