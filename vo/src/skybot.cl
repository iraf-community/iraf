#{  SKYBOT -- Find the minor planets in an image, display it and mark the
#   asteroids.

procedure skybot (image)

string	image			{ prompt = "Input image"		  }

bool	display = yes		{ prompt = "Display result?"		  }
bool	ned     = no		{ prompt = "Overlay NED sources?"	  }
bool	grid    = no		{ prompt = "Overlay coordinate grid?"	  }

real	nhours  = 6.0		{ prompt = "Number of hours of movement"  }

bool	verbose = yes		{ prompt = "Verbose output?"		  }
int	status  = 0		{ prompt = "Service status code"	  }

begin
    string img, date, res, coords, tvcoords, cmd
    real   ra, dec, size, mjd, nh
    real   radius, x1, x2, y1, y2, vmag, dra, ddec
    bool   verb, disp, do_ned, do_grid


    img  = image
    verb = verbose
    disp = display

    nh      = nhours
    do_ned  = ned
    do_grid = grid

    set clobber = yes
    set imclobber = yes

    if (imaccess (img)) {
        # Compute the MJD from the DATE-OBS keyword
        hselect (img, "DATE-OBS", yes) | scan (date)
        if (date == "") {
            errror (0, "No DATE-OBS keyword in the image.")
        } else {
            print ("print(julday(\""//date//"\"))") | \
    	    astcalc (commands="STDIN") | scan (mjd)
        }

        iferr { wcsinfo (img) } then {
            error (0, "Cannot determine image coords for `"//img//"'")
        } else {
            ra  = wcsinfo.midx
            dec = wcsinfo.midy
            size = max (wcsinfo.width, wcsinfo.height) * 60.0 / 2.0
        }
    } else 
        error (0, "Image not found.")
    

    # Create temp files for the output
    res = mktemp ("tmp$imsky")

    # Call the SKYBOT task to do the search.
    sbquery(ra, dec, size, epoc=mjd, fields="ra,dec,vmag,dra,ddec,name", >& res)

#    if (verbose || display == no)
#       type (res)

    if (display) {
        cmd = mktemp ("tmp$imsky")
        coords = mktemp ("tmp$imsky")
        tvcoords = mktemp ("tmp$imsky")

        display (img, 1, fill+, bpmask="", select+, >& "dev$null")


	fields (res, "1-", >& "foo")
	list = "foo"
	while (fscan (list, ra, dec, vmag, dra, ddec, line) != EOF) {
	    printf ("%h %h %.1f\n", ra, dec, vmag) |& \
                wcsctran ("STDIN", "STDOUT", img, "world", "logical", 
    	            units="h n", verbose-, >& tvcoords )

	    # Compute a radius representing 2-hours of movement.
	    x = dra					    	# in "/hr
	    y = ddec					    	# in "/hr
	    rad = max (10., sqrt((x*x)+(y*y)) * wcsinfo.scale)  # in pixels

	    type (tvcoords) | scan (x, y)
	    printf ("%-11h %-12h %7.1f %7.1f %4.1f %6.4f %6.4f %4.1f %s\n",
		ra, dec, x, y, vmag, dra, ddec, rad, line)

            tvmark (1, tvcoords, mark="circle", radii="10", col=205, 
		nxoffset=12, nyoffset=2, lab+)

	    x1 = ra  - (nh * (dra  / 3600.) / 15.)
	    y1 = dec - (nh * (ddec / 3600.))
	    x2 = ra  + (nh * (dra  / 3600.) / 15.)
	    y2 = dec + (nh * (ddec / 3600.))

	    printf ("%h %h 101 s\n%h %h 101 s\n", 
		x1, y1, (ra-(dra/3600./15./2.)), (dec-(ddec/3600./2.))) |&
            wcsctran ("STDIN", "STDOUT", img, "world", "logical", 
    	        units="h n", verbose-, >& tvcoords )
            tvmark (1, coords="", commands=tvcoords, interact-, col=206, lab-)

	    printf ("%h %h 101 s\n%h %h 101 s\n", 
		(ra+(dra/3600./15./2.)), (dec+(ddec/3600./2.)), x2, y2) |&
            wcsctran ("STDIN", "STDOUT", img, "world", "logical", 
    	        units="h n", verbose-, >& tvcoords )
            tvmark (1, coords="", commands=tvcoords, interact-, col=204, lab-)
	}


	# Overlay the NED objects?
        if (do_ned) 
	    nedoverlay (img)

	# Draw the coordinate overlay grid?
        if (do_grid) 
	    wcslab (img, 1, use-, fill+, overplot+, append+, 
	        labout-, dev="imdy")

        # Clean up
        delete (cmd, verify-, >& "dev$null")
        delete (coords, verify-, >& "dev$null")
        delete (tvcoords, verify-, >& "dev$null")
    }

    # Clean up
    delete (res, verify-, >& "dev$null")
end

