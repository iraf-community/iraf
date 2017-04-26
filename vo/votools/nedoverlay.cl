#{  NEDOVERLAY -- Overlay NED catalog sources on an image display.

procedure nedoverlay (image)

string	image			{ prompt = "Input image"		}

bool	append 	 = yes		{ prompt = "Append display?"		}
real	size	 = 0.25		{ prompt = "Field size"			}
bool	galaxies = no		{ prompt = "Mark Galaxies?"		}
bool	radios   = yes		{ prompt = "Mark Radio sources?"	}
bool	xrays    = yes		{ prompt = "Mark X-Ray sources?"	}
int	mkcolor  = 208		{ prompt = "Marker color"		}
int	galcolor = 207		{ prompt = "Galaxy marker color"	}
bool	verbose  = no		{ prompt = "Verbose output?"		}
int	status 	 = 0		{ prompt = "Service status code"	}

begin
    string img, ned, coords, tvcoords, url, query
    string fields, pos_all, pos_l, pos_w, base
    real   ra, dec, sz
    bool   verb, app, gal, rad, xry
    int    mcol, gcol, nobjs


    img  = image		# Get params to local variables
    verb = verbose
    app  = append
    mcol = mkcolor
    gal  = galaxies
    rad  = radios
    xry  = xrays
    gcol = galcolor
    sz   = size

    base = "http://ned.ipac.caltech.edu/cgi-bin/nph-NEDobjsearch"
    base = base // "?search_type=Near+Position+Search&of=xml_main&" 

    if (imaccess (img)) {
        iferr { wcsinfo (img) } then {
            error (0, "Cannot determine image coords for `"//img//"'")
        } else {
            ra  = wcsinfo.midx
            dec = wcsinfo.midy
            sz = max (wcsinfo.width, wcsinfo.height) / 60.0
    	    if (app == no)
		display (img, 1, >& "dev$null")
        }
    } else {
        sesame (img, verbose-)
    	if (app == no)
	    dss (img, size=sz, use_display+)
        ra  = sesame.ra
        dec = sesame.dec
	img = "cache$" // img // ".fits"
    }


    # Get NED sources
    printf ("RA=%.4f&DEC=%.4f&SR=%.4f\n", ra, dec, sz) | scan (query)
    url = base // query // "&RUNID=" // vo.runid

    ned      = mktemp ("/tmp/ned")


    # Download and convert the file.
    votcopy (url, ned, format="ascii")
    
    # Create temp files for the output
    coords   = mktemp ("tmp$ned_coords_")
    tvcoords = mktemp ("tmp$ned_tvcoords_")
    pos_all  = mktemp ("tmp$pos_all_")
    pos_l    = mktemp ("tmp$pos_l_")
    pos_w    = mktemp ("tmp$pos_w_")

    # Select the RA,Dec from the output NED table.
    fields (ned, "3,4,5", >& pos_all)


    # Expand the list of object types we want to mark.
    fields   = mktemp ("tmp$ned_fields_")
    print ("G_Lens\nGClstr\nGPair\nQSO\n",  > fields)
    if (rad) print ("Radio\n", >> fields)
    if (xry) print ("Xray\n",  >> fields)

    #  Mark the Galaxies on the display.
    if (galaxies) {
        match (" G ", ned)  | fields("STDIN","3,4,5", >& pos_all)
        count (pos_all) | scan (nobjs)
        wcsctran (pos_all, "c1", img, verb-,
                inwcs="world", outwcs="logical", units="n n")
        tvmark (frame=1, coords="c1", mark="plus", radii=10, color=gcol, 
	    txsize=1, nxoffset=5, nyoffset=-10)
        delete ("c1", verify-, >& "dev$null")
    }
    if (verbose)
	printf ("%d objects found\n", nobjs)

    #  Mark the requested objects on the display.
    list = fields
    while (fscan (list, s1) != EOF) {
        match (s1, ned) | fields("STDIN", "3,4,5", >& pos_w)
        count(pos_w) | scan(nobjs)

	if (nobjs > 0) {
	    if (verbose) 
	        printf ("%-10s : %d found\n", s1, nobjs)
	    wcsctran (pos_w,pos_l,img,"world","logical",verb-,units="n n") | \
	    tvmark (frame=1, coords=pos_l, mark="circle", radii=10,
	        color=mcol, txsize=1, lab+, nxoffset=5, nyoffset=-10)
	}
    }
    list = ""
    
    # Clean up.
    delete ("tmp$ned*,tmp$pos_*", verify-, >& "dev$null")
end
