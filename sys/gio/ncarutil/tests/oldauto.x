# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	DUMMY	6

include	<error.h>
include	<gset.h>
include	<ctype.h>

# Test NCAR routine AUTOGRAPH - EZXY, EZMXY etc.

procedure t_oldauto()

char	device[SZ_FNAME], command[SZ_LINE]
int	error_code, wkid
int	ctoi()
pointer gp, gopen()

begin
	call clgstr ("device", device, SZ_FNAME)

	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, NEW_FILE, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)

		call exmpl1
		call exmpl2
		call exmpl3
		call exmpl4
		call exmpl5
		call exmpl6
		call exmpl7
		call exmpl8
		# call exmpl9
		call xmpl11

	call gdawk (wkid)
	call gclwk (wkid)
	call gclks ()
end
