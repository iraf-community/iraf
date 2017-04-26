# HELLO.X -- GUI version of IRAF hello world.

procedure t_hello()

pointer	gp
real	x, y
int	wcs, key
char	strval[SZ_LINE]
char	device[SZ_FNAME]
char	guifile[SZ_FNAME]
int	clgcur()
pointer	gopenui()

begin
	call clgstr ("device", device, SZ_FNAME)
	call clgstr ("gui", guifile, SZ_FNAME)

	gp = gopenui (device, NEW_FILE, guifile, STDGRAPH)
	while (clgcur ("coords", x, y, wcs, key, strval, SZ_LINE) != EOF)
	    if (key == 'q' || key == 'Q')
		break
	    else {
		if (key == ':') {
		    call printf ("%g %g %d %c %s\n")
			call pargr (x)
			call pargr (y)
			call pargi (wcs)
			call pargi (key)
			call pargstr (strval)
		} else {
		    call printf ("%g %g %d %c\n")
			call pargr (x)
			call pargr (y)
			call pargi (wcs)
			call pargi (key)
		}
	    }
	call gclose (gp)
end
