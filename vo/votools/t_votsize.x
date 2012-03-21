#
# VOTSIZE -- Simply get the size of a VOTable.  We need to work on the
# VOTable itself since the TABLES tasks don't generally behave well when
# there are no rows in a table.

include <ctype.h>
include <votParse_spp.h>


procedure t_votsize ()

char	in[SZ_LINE]
char	out[SZ_LINE], line[SZ_LINE]

int	fd, nchars
int	vx_voinfo(), open(), getline()

begin
	# Get the table name.
	call clgstr ("input", in, SZ_LINE)

	call mktemp ("/tmp/sz", out, SZ_LINE)
        call vx_voinfo (3, "-s", out, in)

	fd = open (out, READ_ONLY, TEXT_FILE)
	nchars = getline (fd, line)
	call close (fd)

	call printf ("%s")
	    call pargstr (line)
	call delete (out)
end
