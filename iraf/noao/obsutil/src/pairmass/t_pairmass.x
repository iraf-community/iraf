# PAIRMASS -- plot the airmass for a given RA & Dec on a given date.

procedure t_pairmass()

pointer	sp, observat, timesys, ut, air, title
pointer	obs
double	ra, dec, epoch, ra0, dec0, epoch0
double	longitude, latitude, zone, st, ha, utd, resolution
int	day, month, year, nsteps, i, tsys
real	amin, amax

pointer	obsopen()
double	clgetd(), obsgetd(), ast_mst(), airmass()
bool	clgetb()
int	clgeti(), clgwrd()

begin
	call smark (sp)
	call salloc (observat, SZ_FNAME, TY_CHAR)
	call salloc (timesys, SZ_FNAME, TY_CHAR)

	ra0 = clgetd ("ra")
	dec0 = clgetd ("dec")
	epoch0 = clgetd ("epoch")

	year = clgeti ("year")
	month = clgeti ("month")
	day = clgeti ("day")

	# Set time to plot.
	tsys = clgwrd ("timesys", Memc[timesys], SZ_FNAME,
	    "|Universal|Standard|Siderial|")

	# Get observatory information.
	call clgstr ("observatory", Memc[observat], SZ_FNAME)
	obs = obsopen (Memc[observat])
	#call obslog (obs, "PAIRMASS", "latitude longitude timezone", STDOUT)
	call obsgstr (obs, "name", Memc[observat], SZ_FNAME)
	longitude = obsgetd (obs, "longitude")
	latitude = obsgetd (obs, "latitude")
	zone = obsgetd (obs, "timezone")
	call obsclose (obs)

	resolution = clgetd ("resolution")
	nsteps = nint (24 * resolution) + 1

	call salloc (ut, 3*nsteps, TY_REAL)
	call salloc (air, 3*nsteps, TY_REAL)
	call salloc (title, SZ_LINE, TY_CHAR)

	call ast_date_to_epoch (year, month, day, 12.d0, epoch)
	call ast_precess (ra0, dec0, epoch0, ra, dec, epoch)

	do i = 1, nsteps {
	    utd = (i-1) / resolution
	    call ast_date_to_epoch (year, month, day, utd, epoch)
	    st = ast_mst (epoch, longitude)
	    ha = st - ra

	    switch (tsys) {
	    case 1:
		Memr[ut+i-1] = utd
	    case 2:
		if (utd < zone)
		    Memr[ut+i-1] = utd - zone + 24
		else
		    Memr[ut+i-1] = utd - zone
	    case 3:
		Memr[ut+i-1] = st
	    }
	    Memr[air+i-1] = real (airmass (ha, dec, latitude))

	    Memr[ut+i-1+nsteps] = Memr[ut+i-1] - 24
	    Memr[ut+i-1+2*nsteps] = Memr[ut+i-1] + 24
	    Memr[air+i-1+nsteps] = Memr[air+i-1]
	    Memr[air+i-1+2*nsteps] = Memr[air+i-1]
	}
	call xt_sort2 (Memr[ut], Memr[air], 3*nsteps)
	call alimr (Memr[air], 3*nsteps, amin, amax)

	call sprintf (Memc[title], SZ_LINE,
	    "Airmass for %d/%d/%d\n%s\nRA=%h, Dec=%h (%g)")
	    call pargi (month)
	    call pargi (day)
	    call pargi (year)
	    call pargstr (Memc[observat])
	    call pargd (ra0)
	    call pargd (dec0)
	    call pargd (epoch0)

	if (clgetb ("listout")) {
	    call printf ("%s\nTime System=%s\n\n")
		call pargstr (Memc[title])
		call pargstr (Memc[timesys])
	    amax = clgetd ("wy2")
	    if (amax < 1)
		amax = 5
	    do i = 1, 3*nsteps {
		if (Memr[ut+i-1] < 0. || Memr[ut+i-1] >= 24.)
		    next
		if (Memr[air+i-1] > amax)
		    next
		call printf ("%6.0m\t%8.4f\n")
		    call pargr (Memr[ut+i-1])
		    call pargr (Memr[air+i-1])
	    }
	} else
	    call draw_vector (Memc[title], Memc[timesys], Memr[ut], Memr[air],
		3*nsteps, 0., 24., amin, amax)

	call sfree (sp)
end
