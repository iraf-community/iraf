# CREDIT -- Edit cosmic rays with an image display.

procedure credit (input, output)

begin
	imedit (input, output, cursor=cursor, logfile=logfile,
	    display=display, autodisplay=autodisplay,
	    autosurface=autosurface, aperture=aperture, radius=radius,
	    search=search, buffer=buffer, width=width, xorder=xorder,
	    yorder=yorder, value=value, sigma=sigma, angh=angh, angv=angv,
	    command=command, graphics=graphics, default=default,
	    fixpix=fixpix)
end
