# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task	dump = t_dump

# DUMP -- Dump a termcap (GTY) device entry.

procedure t_dump()

char	fname[SZ_FNAME]
char	device[SZ_FNAME]
char	ufields[SZ_LINE]

pointer	gty
pointer	gtyopen()
pointer	gtycaps()

begin
	call clgstr ("fname", fname, SZ_FNAME)
	call clgstr ("device", device, SZ_FNAME)
	call clgstr ("ufields", ufields, SZ_LINE)

	gty = gtyopen (fname, device, ufields)
	call printf ("%s\n")
	    call pargstr (Memc[gtycaps(gty)])
	call gtyclose (gty)
end
