# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	SZ_LBUF		2048
define	SZ_PATSTR	128
define	SZ_PATBUF	1024

# GDEVICES -- Print a summary of the graphics devices currently defined in
# graphcap.  The devices parameter, a list of pattern strings, defines the
# class of devices to be listed.  The devices aliases, X and Y resolution,
# and device description are output for each device.

procedure t_gdevices()

pointer	fnt, ip, op, gty
int	fd, junk, nalias, lnum, xr, yr
pointer	sp, devices, fname, patstr, patbuf, lbuf, device, devdes

bool	streq()
pointer	fntopn(), gtyopen()
int	patmake(), patmatch(), gtygeti()
int	open(), getlongline(), envfind(), fntgfn()
string	s_graphcap "graphcap"

begin
	call smark (sp)
	call salloc (devices, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (patstr, SZ_PATSTR, TY_CHAR)
	call salloc (patbuf, SZ_PATBUF, TY_CHAR)
	call salloc (lbuf, SZ_LBUF, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)

	# Get list of device patterns to be matched against graphcap.
	call clgstr ("devices", Memc[devices], SZ_LINE)

	# Get graphcap file name.
	call clgstr (s_graphcap, Memc[fname], SZ_PATHNAME)
	if (Memc[fname] == EOS)
	    call strcpy ("dev$graphcap", Memc[fname], SZ_PATHNAME)
	else if (streq (Memc[fname], s_graphcap))
	    if (envfind (s_graphcap, Memc[fname], SZ_PATHNAME) <= 0)
		call strcpy ("dev$graphcap", Memc[fname], SZ_PATHNAME)

	# Print table header.
	call printf ("#%39s  %4s %4s  %s\n")
	    call pargstr ("ALIASES")
	    call pargstr ("NX")
	    call pargstr ("NY")
	    call pargstr ("DESCRIPTION")

	fnt = fntopn (Memc[devices])
	while (fntgfn (fnt, Memc[patstr], SZ_PATSTR) != EOF) {
	    junk = patmake (Memc[patstr], Memc[patbuf], SZ_PATBUF)

	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    while (getlongline (fd, Memc[lbuf], SZ_LBUF, lnum) != EOF) {

		if (patmatch (Memc[lbuf], Memc[patbuf]) > 0) {
		    # Get device alias list and extract first device name
		    # into "device" string.

		    ip = lbuf
		    op = device
		    for (nalias=1;  Memc[ip] != EOS;  nalias=nalias+1) {
			devdes = ip
			while (Memc[ip] != EOS) {
			    if (Memc[ip] == '|') {
				Memc[ip] = ' '
				ip = ip + 1
				break
			    } else if (Memc[ip] == ':') {
				Memc[devdes-1] = EOS
				Memc[ip] = EOS
				break
			    } else {
				if (nalias == 1) {
				    Memc[op] = Memc[ip]
				    op = op + 1
				}
				ip = ip + 1
			    }
			}
			Memc[op] = EOS
		    }

		    # Fetch graphcap entry for device.
		    iferr (gty = gtyopen (Memc[fname], Memc[device], "")) {
			call eprintf ("cannot access entry for %s\n")
			    call pargstr (Memc[device])
			next
		    }

		    # Print information about graphics device, in the
		    # form  alias ... alias nx ny description.

		    iferr (xr = gtygeti (gty, "xr"))
			xr = 0
		    iferr (yr = gtygeti (gty, "yr"))
			yr = 0

		    call printf ("%40s %5d%5d  %0.27s\n")
			call pargstr (Memc[lbuf])
			call pargi (xr)
			call pargi (yr)
			call pargstr (Memc[devdes])

		    call flush (STDOUT)
		    call gtyclose (gty)
		}
	    }
	    call close (fd)
	}

	call fntcls (fnt)
	call sfree (sp)
end
