# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>

define	QUERY	"Type `q' to verify quit, `return' to return to cursor loop:"

# GQVERIFY -- Print a message in the status line asking the user if they really
# want to quit, returning YES if they really want to quit, NO otherwise.

int procedure gqverify()

int	ch
int	getci()

begin
	call printf (QUERY)
	call flush (STDOUT)

	call fseti (STDIN, F_RAW, YES)
	while (getci (STDIN, ch) != EOF)
	    if (ch == 'q' || ch == '\r' || ch == '\n')
		break

	call printf ("\n\n")
	call flush (STDOUT)
	call fseti (STDIN, F_RAW, NO)

	if (ch == 'q')
	    return (YES)
	else
	    return (NO)
end
