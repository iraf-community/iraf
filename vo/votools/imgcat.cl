#{  IMGCAT -- Create a catalog of stellar objects in an image.

procedure imgcat (image, catalog)

string	image			{ prompt = "Image name" 	    	      }
string  catalog 		{ prompt = "Output catalog name"	      }

string	format    = "text"	{ prompt = "Output format"  		      }
bool	verbose   = no		{ prompt = "Verbose?"	    		      }

bool	dosplit   = yes		{ prompt = "Split detected objects?"	      }
string	method    = "thresh"	{ prompt = "Detection method (ace|thresh)",
				    enum = "ace|thresh"  		      }
real	ellipse   = 0.33	{ prompt = "Ellipticity cutoff?"  	      }
real	threshold = 5.		{ prompt = "Detected threshold?"	      }

int	ndetected = 0		{ prompt = "Num detected objects"	      }
int	nel_clip  = 0		{ prompt = "Num clipped for ellipticity"      }
int	nobjs     = 0		{ prompt = "Num remaining objects"	      }
  
begin
    string  img, fmt, catdef, cat, ccscript
    string  tcat, tc1, tc2, tc3, tc4, tdef, expr, detcode
    int     nstars, nx, ny
    bool    verb, split
    real    ellip, sigma, stddev, immean, lthresh


    # Check for proper packages and reset environment.
        ;
    reset imtype  = "fits"
    reset clobber = yes
    #flpr 0


    # Get the task parameters.
    img	     = image
    cat	     = catalog
    verb     = verbose
    fmt      = format
    ellip    = ellipse
    detcode  = method
    split    = dosplit
    sigma    = threshold


    # If we have zero values, replace them with the image mean.
    minmax (img, update-, >& "dev$null")
    if (minmax.minval == 0) {
        imstat (img, field="mean", format-) | scan (immean)
        imreplace (img, immean, upper=0.0)
    }


    # Do the object detection.
    if (detcode == "ace") {
        # Initialize
        catdef = mktemp ("/tmp/cd")

        print ("WX\nWY\nPX\nPY\nFLUX\nELLIP\n", > catdef)
	
        # Remove the DATASEC keyword if present since it may not be valid for
        # the image, but will be used by ACE anyway.
	hselect (img, "DATASEC", yes) | scan (s1)
	if (nscan() == 1)
	   hedit (img, "DATASEC", delete+, update+, verify-, >& "dev$null")


	# Run the DETECT task to locate the stars.
	iferr {
            detect (img, catdefs=catdef, catalog=cat, hsigma=sigma,
	        doeval+, dosplit=split, dogrow+)
return
            detect (img, catdefs=catdef, catalog=cat, hsigma=sigma,
	        doeval+, dosplit=split, dogrow+) | \
	    match ("detected", "STDIN") | scan (nstars)
            delete (catdef, verify-)
	} then {
	    error (0, "Error creating image catalog\n")
	}

        # Now filter the catalog to pull out the "stellar" objects identified
        # by a low ellipticity.  Sort on FLUX.
        tcat = mktemp ("/tmp/tc")
        tinfo (cat, ttout-)
        nstars = tinfo.nrows
        fields (cat, "1-6", > tcat)
        if (ellip > 0.0) {
            expr = "c6 == INDEF || c6 < " // ellip
            tselect (tcat, cat, expr)
            tcopy (cat, tcat, >& "dev$null")
        }
        tinfo (tcat, ttout-)
        tsort (tcat, col="c5", ascend-)


        tcalc (tcat, "c1", "c1 * 15.0")
        tdump (tcat, cd="dev$null", pf="dev$null", data="STDOUT",
	    col="c1,c2,c3,c4,c5", row="1-", > cat)

        # Sort the table and save the results to parameters.
        tsort (cat, col="c4")

        # Clean up.
        if (access (tcat) == yes) 
	    delete (tcat, verify-, >& "dev$null")

    } else if (detcode == "thresh") {

	# Get the stddev of the image
	imstat (img, fields="stddev", format-) | scan (stddev)
	lthresh = 1 * stddev

	starfind (img, "image.obj", 1., lthresh, wcs="world", 
	    wxf="%12.2H", wyf="%12.1h")

	i = 0
	printf ("|id            |ra            |dec           |\n", > cat)
	list = "image.obj"
	while (fscan (list, z, z, x, y) != EOF) {
	    if (i > 18)
	        printf (" %14d %14.8f %14.8f\n", (i-18), x, y, 	    >> cat)
	    i = i + 1
	}
	list = ""
	nstars = i - 18

	delete ("image.obj", verify-, 	>& "dev$null")
	ellip = 0.0				# disable ellipticity check
    }

    ndetected = nstars
    nobjs     = tinfo.nrows

return
    # Run the cross-compare script.
    ccscript = osfn ("votools$cross_comp.sh")
    printf ("!%s %s 1.5 TWOMASS_PSC\n", ccscript, cat) | cl()
end
