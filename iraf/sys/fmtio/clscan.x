# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLSCAN -- Begin a scan of the value of a CL parameter

int procedure clscan (param)

char	param[ARB]
int	getline(), strncmp(), clc_fetch()
include	"scan.com"
errchk	clreqpar, getline

begin
	# Fetch the value of a CL parameter.  First look in the parameter
	# cache, querying the CL for the value of the parameter only if it
	# is not found in the cache.

	if (clc_fetch (param, sc_scanbuf, SZ_SCANBUF) == ERR) {
	    call clreqpar (param)
	    if (getline (CLIN, sc_scanbuf) == EOF)
		return (EOF)
	}

	# Check for EOF on a list structured parameter; if not EOF initialize
	# formatted input for the clget procedures.

	if (strncmp ("EOF\n", sc_scanbuf, 4) == 0)
	    return (EOF)
	else {
	    call reset_scan()
	    return (OK)
	}
end
