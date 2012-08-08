include <gio.h>

define 	FOO	Memr[Memi[$1+12]]	# test comment

define  BAR	Memr[$1]
define  BAR1	Memr[$1+1]
define  BAR2	Memr[TEST($1)]

define  FOOBAR	Memr[$1]

procedure hello()

pointer	xs, xe
define  XS Memr[xs+($1)-1]
define  XE Memr[xe+($1)-1]

begin
	call printf ("hello, world:  %d\n", FOO(1))
end
