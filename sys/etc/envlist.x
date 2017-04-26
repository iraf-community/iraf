# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"environ.h"

# ENVLIST -- Print the environment list on the output file as a sequence of
# SET commands.  The commands are given in the reverse of the order in which
# they were originally entered.  Printing of redefined variables may be
# inhibited if desired.

procedure envlist (fd, prefix, print_redefined_variables)

int	fd			# output file
char	prefix[ARB]		# prefix string to be prepended to each line
int	print_redefined_variables
pointer	el
include	"environ.com"

begin
	for (el = envbuf + last;  el > envbuf;  el = envbuf + E_LASTELEM(el))
	    if (E_REDEF(el) == NO || print_redefined_variables == YES) {
		call putline (fd, prefix)
		call putline (fd, E_SET(el))
		call putci (fd, '\n')
	    }
end
