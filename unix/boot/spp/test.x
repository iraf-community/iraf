# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# Test program.

task	hello = t_hello

procedure t_hello()

begin
	call printf ("hello, world\n")
end
