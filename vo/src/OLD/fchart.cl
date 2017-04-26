#  CHART -- Toy finder chart program using an object name, a user-supplied
#  image, or a position spec to display a point on the sky and an overlay
#  of known NED objects.

procedure chart (object)

string	object				  { prompt = "Object to plot"	    }
string  image   = ""			  { prompt = "Image name"	    }
real	ra      = INDEF			  { prompt = "RA of position"	    }
real	dec     = INDEF			  { prompt = "Dec of position"	    }
string  inwcs   = "icrs"		  { prompt = "Input coord system"   }
real	size    = 0.2			  { prompt = "Size (degrees)"	    }
string	fields  = "G_Lens,Xray,Radio,QSO" { prompt = "Objects to mark"      }
bool	grid    = yes			  { prompt = "Overlay coordinate grid?"	}

begin
    real    lra, ldec, lsz
    int     count, siap, col
    string  imname, pos, fstr, obj, img, isys
    bool    do_grid

    reset stdimage  = imt1024	#  Set the environment
    reset imclobber = yes
    reset clobber   = yes

    obj     = object		# get a local variable of the param value
    lsz     = size
    fstr    = fields
    lra     = ra
    ldec    = dec
    img     = image
    isys    = inwcs
    do_grid = grid

    # Get the starting position.  Use an image if specified, the check
    # for an ra/dec pos, and finally a user-supplied object name.
    if (imaccess (img, verbose=no)) {
        iferr { wcsinfo (img) } then {
            error (0, "Cannot determine image coords for `"//img//"'")
        } else {
            lra  = wcsinfo.midx
            ldec = wcsinfo.midy
            lsz = max (wcsinfo.width, wcsinfo.height) / 60.0
        }
        imname = img

    } else if (ra != INDEF && dec != INDEF) {
        # No image specified, use the parameters (assume it's in hours).
        lra   = ra * 15.
        ldec  = dec
        lsz   = size				# size in degrees
        imname = mktemp ("chart") // ".fits"	# create temp filenames

    } else {
        # Sample Test Objects: 
        #     abell2235		# has QSOs
        #     abell1689		# has G_Lens
        #     abell2255		# has lotsa Galaxies

        # Resolve the Abell number to coordinates.
        sesame (obj)
        lra    = sesame.ra
        ldec   = sesame.dec
        lsz   = size
        imname = mktemp ("chart") // ".fits"	# create temp filenames
    }

    # Convert alternate input coordinate system values.
    if (isys != "icrs") {
        print (lra // " " // ldec) | \
            skyctran ("STDIN", "STDOUT", isys, "icrs", transform+,
                olngu="deg", olatu="deg", olngf="%f", olatf="%f") | \
            fields ("STDIN","1,2",lines="9") | scan (x, y)
        if (verb) {
            print ("#")
            print ("#   Input coords: ("//lra//","//ldec//") ("//isys//")")
            print ("#  Output coords: ("//x//","//y//") (ICRS)")
            print ("#")
        }
        lra = x
        ldec = y
    }
    pos = mktemp ("chart")


    # Get an image of the field if the user didn't supply one.
    if (obj != "" || (ra != INDEF && dec != INDEF)) {

	dss ("", ra=ra, dec=dec)
	imname = dss.imname
    }

    # Display the image.  We're using DSS so we need to specify an image
    # extension, other SIAP services
    display (imname, 1, fill+, >& "dev$null")
    
    # Get NED sources
    #vocatalog (regResolver("NED","cone"), lra, ldec, lsz, output="ned.txt")
    s1 = "http://nedwww.ipac.caltech.edu/cgi-bin/nph-NEDobjsearch?search_type=Near+Position+Search&of=xml_main&"
    vocatalog (s1, lra, ldec, lsz, output="ned.txt")
    
    # Select the RA,Dec from the output NED table.
    fields ("ned.txt","3,4", >& "pos_all")
    
    # Expand the list of object types we want to mark.
    print (fstr) | translit ("STDIN",",","\n",del-, > "fields")

    #  Mark the Galaxies on the display.
    match (" G ","ned.txt")  | fields("STDIN","3,4", >& "pos_all")
    wcsctran ("pos_all",   "c1", imname, verb-,
        inwcs="world",outwcs="logical",units="n n")
    tvmark (frame=1,coords="c1",mark="plus", color=206, txsize=3)

    #  Mark the rest of the requested objects on the display.
    list = "fields"
    while (fscan (list, s1) != EOF) {
        match (s1,"ned.txt") | fields("STDIN","3,4,5", >& "pos_w")
	wcsctran ("pos_w","pos_l", imname, "world","logical",
		verb-, units="n n") | \
	tvmark (frame=1, coords="pos_l",mark="plus",color=204,txsize=3,lab+)
    }

    if (do_grid) 
	wcslab (imname, 1, use-, fill+, overplot+, append+, 
	    labout-, dev="imdy")

    # Clean up.
    if (access (pos))    delete (pos, verify-, >& "dev$null")
    if (access (imname)) delete (imname, verify-, >& "dev$null")

    # Update params.
    object = ""
    image  = ""
    ra     = INDEF
    dec    = INDEF
end
