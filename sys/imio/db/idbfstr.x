# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>


# IDB_FILSTR -- Filter a string, removing any tabs or control characters.
# This is used to clean up strings we want to put in image headers.  A count
# of the output, filtered string is returned as the function value.  Tabs or
# newlines in the input are replaced by blanks.  Illegal or unprintable
# control characters in the input are deleted.

int procedure idb_filstr (s1, s2, maxch)

char	s1[ARB]				#I input string
char	s2[ARB]				#O output string
int	maxch				#I max chars out

int	op, ch, i

begin
	op = 1

	do i = 1, ARB {
	    ch = s1[i]
	    if (ch == EOS)
		break
	    else if (ch == '\t' || ch == '\n')
		ch = ' '
	    else if (!IS_PRINT (ch))
		next

	    s2[op] = ch
	    op = op + 1
	    if (op > maxch)
		break
	}

	s2[op] = EOS
	return (op - 1)
end
