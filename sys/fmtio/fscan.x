# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FSCAN -- Begin scanning a line from a file.

int procedure fscan (fd)

int	fd
int	getline()
include	"scan.com"
errchk	getline

begin
	if (getline (fd, sc_scanbuf) == EOF)
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
