# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	DUMMY	6

include	<error.h>
include	<gset.h>

# Test NCAR routine ISOSRFHR

procedure t_isosrf()

char	device[SZ_FNAME]
int	error_code, wkid
pointer	gp, gopen()

begin
	call clgstr ("device", device, SZ_FNAME)

	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, NEW_FILE, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)

	call tisosr (2, error_code)
	if (error_code == 0)
	    call printf ("Test successful\n")
	
	call gdawk (wkid)
	call gclwk (wkid)
	call gclks ()
end
