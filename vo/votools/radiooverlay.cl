#{ RADIOOVERLAY -- Overlay a radio contour on the image display.

procedure radiooverlay (image)

string	image			{ prompt = "Input image"		}

real	size	  = 0.25	{ prompt = "Query size"			}
int	ncontours = 0		{ prompt = "No. contours to draw"	}

bool	append    = yes		{ prompt = "Append display?"		}
bool	verbose   = yes		{ prompt = "Verbose output?"		}
string  device    = "imdred"	{ prompt = "Overlay device"		}
int	status    = 0		{ prompt = "Service status code"	}

begin
    string img, rimg, imname, ovdev, url, query, base
    string fields, pos_all, pos_l, pos_w, restab
    real   ra, dec, sz
    bool   verb, app, disp
    int    siap, count, ncont


    img    = image		# Get params to local variables
    sz     = size
    verb   = verbose
    app    = append
    ncont  = ncontours
    ovdev  = device
    restab = mktemp ("tmp$res") // ".xml"
    rimg   = mktemp ("tmp$img") // ".fits"


    if (imaccess (img)) {
        iferr { wcsinfo (img) } then {
            error (0, "Cannot determine image coords for `"//img//"'")
        } else {
            ra  = wcsinfo.midx
            dec = wcsinfo.midy
            sz = max (wcsinfo.width, wcsinfo.height) / 60.0
	    if (!app)
	        display (img, 1, >& "dev$null")
        }
    } else {
        sesame (img, verbose-)
        ra  = sesame.ra
        dec = sesame.dec
	if (!app)
	    dss (img, size=sz, use_display+)
    }


    # Create temp files for the output
    rimg = mktemp ("tmp$rad") // ".fits"

    # Get NVSS image, force the use of the SkyView service.
    base = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?survey=nvss&" 
    printf ("POS=%.5f,%.5f&SIZE=%.3f", ra, dec, sz) | scan (query)
    url = base // query // "&RUNID=" // vo.runid

    # Download the query string.
    urlget (url, restab, use_cache+, cache="cache$")

    tdump (restab, row=1, col="URL", pwidth=2048, cdfile="dev$null",
        pfile="dev$null") | scan (rimg)

    # Overlay the contours
    contour (rimg, dev=ovdev, title="", perim-, fill+, xres=64, yres=64,
	ncontours=ncont, >& "dev$null")
    
    # Clean up.
    delete (restab, verify-, >& "dev$null")
    imdelete (rimg, >& "dev$null")
end
