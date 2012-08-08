# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FSCAN -- Begin scanning a line from a file.

int procedure fscan (fd)

int	fd
int	getlline()
include	"scan.com"
errchk	getlline

begin
	if (getlline (fd, sc_scanbuf, SZ_SCANBUF) == EOF)
	    return (EOF)
	else {
	    call reset_scan()
	    return (OK)
	}
end


# SCAN -- Scan the standard input.

int procedure scan()

int	fscan()

begin
	return (fscan (STDIN))
end
