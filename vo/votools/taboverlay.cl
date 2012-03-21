#{  TABOVERLAY -- Overlay a catalog of sources on an image display.  The
#   catalog is assumed to be a text file, the user supplies the columns
#   defining the RA/Dec columns (and optional label).  These will be
#   converted to image coords based on the WCS of the image.

procedure taboverlay (image, catalog)

string	image			{ prompt = "Input image"		}
string	catalog			{ prompt = "Input catalog"		}

int	labcol = 1		{ prompt = "Label column"		}
int	racol  = 2		{ prompt = "RA column"			}
int	deccol = 3		{ prompt = "Dec column"			}
int	mkcolor = 208		{ prompt = "Marker color"		}
int	status = 0		{ prompt = "Service status code"	}

begin
    string img, cat, coords, tvcoords, pos_all
    real   ra, dec, size
    int    mcol, c1, c2, c3


    # Get params to local variables.
    img  = image		
    mcol = mkcolor
    c1   = racol
    c2   = deccol
    c3   = labcol
    cat  = catalog

    if (imaccess (img)) {
        iferr { wcsinfo (img) } then {
            error (0, "Cannot determine image coords for `"//img//"'")
        } else {
            ra  = wcsinfo.midx
            dec = wcsinfo.midy
            size = max (wcsinfo.width, wcsinfo.height) / 60.0
        }
    } else {
        #error (0, "Image '"// img // "' not found.")
        sesame (img, verbose-)
        ra  = sesame.ra
        dec = sesame.dec
        img = "cache$" // img // ".fits"
    }
    

    # Create temp files for the output
    coords = mktemp ("tmp$to")
    tvcoords = mktemp ("tmp$to")
    pos_all = mktemp ("tmp$to")

    # Select the RA,Dec and optional columns from the table.
    fields (cat, c1//","//c2//","//c3, >& pos_all)
    
    # Mark the objects on the display.
    wcsctran (pos_all, "c1", img, verb-,
                inwcs="world", outwcs="logical", units="n n")
    tvmark (frame=1, coords="c1", mark="circle", radii=10, color=mcol, 
	txsize=1, nxoffset=5, nyoffset=-10)
    delete ("c1", verify-, >& "dev$null")

    if (mkcolor > 217) 				# update the marker color
	mkcolor = 204
    else
        mkcolor = mkcolor + 1

    # Clean up.
    delete (coords, verify-, >& "dev$null")
    delete (pos_all, verify-, >& "dev$null")
    delete (tvcoords, verify-, >& "dev$null")
end

