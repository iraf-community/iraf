#{  SLOANSPEC --  Query the SDSS Spectral Data (DR4) given an image footprint,
#   object name, or equatorial position and search size.

procedure sloanspec (what)

string  what 			{ prompt = "Image or Object name"	  }

real	ra = INDEF		{ prompt = "RA of Field (hours)"	  }
real	dec = INDEF		{ prompt = "Dec of Field (degrees)"	  }
real	sr = 0.25		{ prompt = "Search size (degrees)"	  }

int     maxrecords  = 100       { prompt = "Max records to return"        }
bool	display     = no	{ prompt = "Display result?"		  }
bool	interactive = yes	{ prompt = "Interactive mode?"		  }
bool	autosave    = yes	{ prompt = "Auto-save downloaded data?"	  }
bool	verbose     = yes	{ prompt = "Verbose output?"		  }
bool	ned         = no	{ prompt = "Overlay NED objects?"	  }
bool	grid        = no	{ prompt = "Overlay coord grid?"	  }
bool	download    = no	{ prompt = "Automatically download data?" }

string  spectype    = "all"	{ prompt = "Spectra type (qso|galaxy|all)",
					enum="qso|galaxy|all"		  }
real	zlow        = INDEF	{ prompt = "Low redshift filter"	  }
real	zhigh       = INDEF	{ prompt = "High redshift filter"	  }

real	size = 15.0		{ prompt = "Size of marker (pixels)"	  }
int	color = 207		{ prompt = "Marker color"		  }
int	status = 0		{ prompt = "Service status code"	  }

string  *llist

begin
    string this, img, obj, stab, ttab, mtab, coords, tvcoords, filter
    string name, class, oclass, ch, cmd, tnames, imlist, sv_img, stype
    bool   verb, disp, do_grid, interact, saving, do_ned, found, getdata
    int    maxrec, mkcolor, n, wcs, siap, count
    real   szmark, d0, d1, xp, yp, z, x1, x2, y1, y2
    real   lra, ldec, lsr, sra, sdec, sz, sx, sy
    real   llx, lly, urx, ury, cx, cy, bsz, z1, z2
    string sname, sid, sclass, mission


    lra      = ra
    ldec     = dec
    this     = ""
    if (isindef(lra) && isindef(ldec))
        this     = what

    lsr      = sr
    disp     = display
    maxrec   = maxrecords
    szmark   = size
    mkcolor  = color
    verb     = verbose
    interact = interactive
    do_grid  = grid
    do_ned   = ned
    saving   = autosave
    stype    = spectype
    z1       = zlow
    z2       = zhigh
    getdata  = download
    sv_img   = ""

    # Set the environment.
    set clobber = yes
    set imclobber = yes


    if (imaccess (this)) {
        img = this
	obj = ""
        iferr { wcsinfo (this) } then {
            error (0, "Cannot determine image coords for `"//this//"'")
        } else {
            ra  = wcsinfo.midx
            dec = wcsinfo.midy
            size = max (wcsinfo.width, wcsinfo.height) / 60.0
        }

    } else { 
        # Assume it's an object name, resolve to coords.
        sesame (this)
        if (sesame.status < 0)
            error (0, "Cannot resolve object `" // this // "'")
        lra  = sesame.ra
        ldec = sesame.dec
        lsr  = sr
        img  = ""
        obj  = this
    }

    filter = "" 
    if (stype == "qso") {
	filter = "Qso" 
    } else if (stype == "galaxy") {
	filter = "Galaxy" 
    }
    ;

    # Create temp files.
    stab = mktemp ("tmp$stab")
    ttab = mktemp ("tmp$ttab")

    # Display an image early on so we have something to look at.
    if (display) {
	if (img == "" || imaccess(img) == no) {
	    # Display an image from the DSS.
	    img = mktemp ("tmp$sspec") // ".fits"
	    sv_img = img

    	    # Get an image of the field if the user didn't supply one.
    	    siap  = dalSiapSvc(regResolver("ivo://nasa.heasarc/skyview/dss1","siap"), lra,ldec,lsr,lsr)
    
    	    count = dalRecordCount(siap)
    	    if (count <= 0)
   	         error (0, "No optical images found")
    	    s1 = dalGetData(siap, 0, osfn(img))

            display (img//"[0]", 1, fill+, bpmask="", >& "dev$null")
	    img = ""
	} else {
            display (img, 1, fill+, bpmask="", >& "dev$null")
	}
    }

    
    # Query for the spectra.
    if (verb) {
	printf ("#\n")
	if (img != "")
            printf ("# Search Params: Img='%s' RA=%.5f Dec=%.5f SR=%.2f\n",
		img, lra, ldec, lsr)
	else if (obj != "")
            printf ("# Search Params: Obj='%s' RA=%.5f Dec=%.5f SR=%.2f\n",
		obj, lra, ldec, lsr)
	else
            printf ("# Search Position: RA=%.5f Dec=%.5f SR=%.2f\n",
		lra, ldec, lsr)
	print ("#\n")
    }
    ssquery (image=img, object=obj, ra=lra, dec=ldec, sr=(lsr * 60.0), 
	    zlow=z1, zhigh=z2, output="STDOUT", maxrec=maxrec, verb-) | \
	match (filter, "STDIN", >& stab)

    status = ssquery.status
    if (ssquery.status < 0)  {
	error (0, "No results found.")

    } else if (img != "") {
	wcsinfo (img)
	lra = wcsinfo.midx		# decimal degrees
	ldec = wcsinfo.midy

    } else if (obj != "") {
	sesame (obj)
	lra = sesame.ra			# decimal degrees
	ldec = sesame.dec
    }
    #printf("img='%s' obj='%s' ra=%.4f dec=%.4f sr=%.2f\n",img,obj,lra,ldec,lsr)



    # See whether we display the image or simply download a cutout of the
    # field we want.
    if (display) {
	coords = mktemp ("tmp$coords")
	tvcoords = mktemp ("tmp$coords")

	if (img == "" || imaccess(img) == no) {
#	    # Display an image from the DSS.
#	    img = mktemp ("tmp$sspec") // ".fits"
	    img = sv_img
#
#    	    # Get an image of the field if the user didn't supply one.
#    	    siap  = dalSiapSvc(regResolver("DSS2R","siap"), lra,ldec,lsr,lsr)
#    
#    	    count = dalRecordCount(siap)
#    	    if (count <= 0)
#   	         error (0, "No optical images found")
#    	    s1 = dalGetData(siap, 0, osfn(img))
#
#            display (img//"[0]", 1, fill+, bpmask="", >& "dev$null")
            fields (stab, "2,3", >& coords)
            wcsctran (coords, tvcoords, img//"[0]", "world", "logical", 
	        units="h n", columns="1 2", verb-)

	    wcsinfo (img//"[0]")

	    sv_img = img
	    img    = img // "[0]"

    	    if (!interact && imaccess(sv_img//".fits"))
	        delete (sv_img//".fits", verify-, >& "dev$null")

	} else {
	    # Display the image.
#            display (img, 1, fill+, bpmask="", >& "dev$null")
            fields (stab, "2,3", >& coords)
            wcsctran (coords, tvcoords, img, "world", "logical", 
	        units="h n", verb-)
	    sv_img = img
	    wcsinfo (img)
	}

        if (verb) {
     	    printf ("#\n")
            if (img != "")
     	        printf ("# Image: %s\n", img)
            else if (obj != "")
     	        printf ("# Object %s\n", obj)

            if (img == "" && obj == "")
       	        printf (   "# Search Pos: (%H,%h)  Size: %.2fx%.2f arcmin\n",
	            ra, dec, lsr, lsr)
            else
       	        printf (   "# Search Pos: (%H,%h)  Size: %.2fx%.2f arcmin\n",
	            wcsinfo.midx, wcsinfo.midy, wcsinfo.width, wcsinfo.height)
       	    printf ("#\n")
	    fields (stab, "1-5")
        }

	# Create local catalog of spectra.
        joinlines (tvcoords//","//stab) | match ("-", "STDIN", stop+, >& ttab)

    	if (access(coords))   delete (coords, verify-, >& "dev$null")
    	if (access(tvcoords)) delete (tvcoords, verify-, >& "dev$null")


        mtab = mktemp ("tmp$mtab")
	fields (ttab, "1,2,3", >& mtab)
type (mtab)
        tvmark (1, mtab, mark="circle", radii=szmark, col=mkcolor,
	    lab+, txsize=1)

	if (access(mtab)) delete (mtab, verify-, >& "dev$null")

        # Overlay the NED objects?
        if (do_ned) 
            nedoverlay (img)

        # Draw the coordinate overlay grid?
        if (do_grid) 
            wcslab (img, 1, use-, fill+, overplot+, append+, 
                labout-, dev="imdy")

        if (interact) {
	    # When verbose we printed the table above, otherwise we want some
	    # kind of summary visible here.
	    if (!verbose) 
		type (ttab)

	    print  ("\n\nIMSSPEC Command Summary:")
	    print  ("b    Batch display w/in a box")
	    print  ("d    Download spectrum")
	    print  ("i    Print Info about spectrum")
	    print  ("n    NED Overlay")
	    print  ("r    Radio contour overlay")
	    print  ("s    Show spectrum")
	    print  ("w    WCS Overlay")
	    print  ("q    Quit")
	    printf ("\nCommand? ")

	    while (fscan (imcur, x, y, wcs, cmd) != EOF) {
		# Find nearest point.  All other commands will depend on the
		# chosen spectrum.
		d0 = 999999999.0
		d1 = 0.0
		found = no
		llist = ttab
	    	while (fscan(llist, xp,yp, name,ra,dec,z,class,line) != EOF) {
		    d1 = sqrt ((xp-x)*(xp-x) + (yp-y)*(yp-y))
		    if (d1 < d0) {
			d0 = d1
		 	sx = xp    ; sy = yp
			sra = lra  ; sdec = ldec
			sz = z
			sname = name
			sclass = class
			found = yes
		    }
	    	}
		llist = ""
		if (!found)
		    print ("Error: object not found")

	  	print ("")
	        switch (cmd) {
	        case "i": 			# Show info on spectrum 
	            {   printf ("%s  %H %h  %.5f  %s\n", 
			    sname, (sra*15.), sdec, sz, sclass)
	            }   

	        case "b": 			# Get spectrum in the box
	            {   printf ("Again...")
			x1 = x
			y1 = y
			n = fscan (imcur, x2, y2, wcs, ch)
			tnames = mktemp ("tmp$tname")
			imlist = mktemp ("tmp$imlist")
			tabclip (ttab, "STDOUT", "c1","c2", x1,y1, x2,y2) | \
			    fields ("STDIN", "1,2,3,9", >& tnames)

			# Draw a box around the selected region
			printf ("%f %f 101 b\n%f %f 101 b\n", x1,y1,x2,y2) | \
			tvmark (1, "", commands="STDIN",  lab-, col=200)

			print ("\nDownloading images ....")
			llist = tnames
			while (fscan (llist, x, y, sname, sid) != EOF) {
			    name = substr (sname, 1, 10) // ".fits"
			    print (name, >>& imlist)
			    printf ("%f %f %s\n", x, y, sname) | \
			    tvmark (1, "STDIN", mark="circle", 
				radii=szmark, col=205, lab+, txsize=1)
			    if (imaccess(name) == no)
			        ssget (sid, name, verbose+)
			    else
				printf ("Image '%s' exists already...\n", name)
			}
			llist = ""

			printf ("Running SPECLOT ....")
			specplot ("@"//imlist, label="imname",cursor="dev$null")

			if (!saving)
			    imdelete ("@"//imlist, >& "dev$null")
			delete (imlist, verify-, >& "dev$null")
			delete (tnames, verify-, >& "dev$null")
			gflush

	            }   
	        case "d": 			# Get spectrum 
	            {   
			match (sname, stab) | scan (line)
			print (line) | fields ("STDIN","1,7") | scan(sname,sid)
			name = substr (sname, 1, 10) // ".fits"
			printf ("%f %f %s\n", sx, sy, sname) | \
			    tvmark (1, "STDIN", mark="circle", radii=szmark,
				col=205, lab+, txsize=1)
			if (imaccess(name) == no)
			    ssget (sid, name, verbose+)
	            }   
	        case "o":			# Observation Overlay
	            {   while (yes) {
			    printf ("Mission (HST|XMM|Spitzer|Chandra|Rosat)? "
			    i = scan (mission)
			    if (i > 0)
				break
			}
			obslogoverlay (img, mission)
	            }
	        case "r":			# NVSS Radio contour overlay
	            {   radiooverlay (img)
	            }
	        case "n":			# NED Overlay
	            {   nedoverlay (img)
	            }
	        case "w":			# WCS Overlay
	            {   wcslab (img, 1, use-, fill+, overplot+, append+, 
                	    labout-, dev="imdy")
	            }
	        case "s": 			# SPLOT spectrum 
	            {   
			match (sname, stab) | scan (line)
			print (line) | fields ("STDIN","1,7") | scan(sname,sid)
			name = substr (sname, 1, 10) // ".fits"
			printf ("%f %f %s\n", sx, sy, sname) | \
			    tvmark (1, "STDIN", mark="circle", radii=szmark,
				col=205, lab+, txsize=1)
			if (imaccess(name) == no)
			    ssget (sid, name, verbose+)

			if (imaccess(name) == yes) {
			    printf ("Running SPLOT on image '%s'....", name)
			    splot (name)
			} else
			    printf ("Error loading image '%s'\n", name)

			if (!saving)
			    imdelete (name, >& "dev$null")
			gflush
	            }
	        case "q":			# Quit
	            {   break
	            }
	        }

	        printf ("\nCommand? ")
	    }
        }

    } else {
	type (stab)

	if (getdata) {
	    spectab (stab, imroot="", verbose+)
	}
    }


    # Clean up
    if (access(stab))	 delete (stab, verify-, >& "dev$null")
    if (access(ttab))	 delete (ttab, verify-, >& "dev$null")

    ra = INDEF		# reset params
    dec = INDEF

    if (interact && imaccess(sv_img//".fits"))
	delete (sv_img//".fits", verify-, >& "dev$null")
end


