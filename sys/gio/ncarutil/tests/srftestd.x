# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task srftest = t_srftest

define	DUMMY	6

# Rewrite of pwrzs.t.f in spp to check things out.  

procedure t_srftest()

char	device[SZ_FNAME]
int	error_code, wkid
int	gp, gopen()

begin
	call clgstr ("device", device, SZ_FNAME)
	
	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, NEW_FILE, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)

	call srf_test()
	
	call gdawk (wkid)
	call gclwk (wkid)
	call gclks ()
end
