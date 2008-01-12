# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	DUMMY	6

include	<error.h>
include	<gset.h>
include	<ctype.h>

# Test NCAR routine AUTOGRAPH - EZXY, EZMXY etc.

task ezytst = t_ezytst

procedure t_ezytst()

char	device[SZ_FNAME], title[SZ_LINE]
int	wkid, i
real	y_vector[512]
pointer gp, gopen()

begin
	call clgstr ("device", device, SZ_FNAME)

	call gopks (STDERR)
	wkid = 1
	gp = gopen (device, NEW_FILE, STDGRAPH)
	call gopwk (wkid, DUMMY, gp)
	call gacwk (wkid)

	# Construct vector to be plotted
	do i = 1, 512
	    y_vector[i] = i

	call strcpy ("TIMING TEST: 512 POINT VECTOR$", title, SZ_LINE)
	call ezy (y_vector(1), 512, 'Timing Test: 512 Point Vector$')

	call gdawk (wkid)
	call gclwk (wkid)
	call gclks ()
end
