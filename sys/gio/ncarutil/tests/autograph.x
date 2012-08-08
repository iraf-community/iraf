# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	DUMMY	6

include	<error.h>
include	<gset.h>
include	<ctype.h>

# Test NCAR routine AUTOGRAPH - EZXY, EZMXY etc.

procedure t_autograph()

char	device[SZ_FNAME], command[SZ_LINE]
int	ierror, wkid, junk, cmd
int	ctoi()
pointer gp, gopen()

begin
	call clgstr ("device", device, SZ_FNAME)

	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, NEW_FILE, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)

	call tautog (ierror)
	if (ierror == 0)
	    call eprintf ("Test successful\n")
	call gdawk (wkid)
	call gclwk (wkid)
	call gclks ()
end
