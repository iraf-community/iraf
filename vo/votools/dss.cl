#{  DSS -- Display a DSS image of a named field, image or position.

procedure dss (field)

string	field			{ prompt = "Input field"		}

real	ra		= 0.0	{ prompt = "RA of field"		}
real	dec		= 0.0	{ prompt = "Dec of field"		}
real	size		= 0.25	{ prompt = "Field size"			}
int	frame   	= 1	{ prompt = "Display frame"		}
bool	use_display 	= yes	{ prompt = "Verbose output?"		}
bool	grid 		= no	{ prompt = "Overlay coordinate grid?"	}
bool	verbose 	= yes	{ prompt = "Verbose output?"		}
bool	save	 	= yes	{ prompt = "Save image?"		}
string	imname		= ""	{ prompt = "Output image name"		}

begin
    string url, imurl, img, query, restab, base
    real   lra, ldec, fra, fdec, sz
    bool   lsave, isim, fcache, do_grid

    lra     = ra 					# Initialize 
    ldec    = dec
    sz      = size
    lsave   = save
    isim    = no
    do_grid = grid
    restab  = mktemp ("tmp$dss")

    base = "http://archive.eso.org/bin/dss_sia/dss.sia?VERSION=1.0&"


    fcache = yes
    if (field == "") {
        fra    = ra
        fdec   = dec
	lsave  = no
        isim   = no
        fcache = no
	img    = mktemp ("dss") // ".fits"
	if (imaccess ("cache$tmp.fits") == yes)
	    imdelete ("cache$tmp.fits", verify-, >& "dev$null")
	field  = "tmp"
	imname = "cache$" // field// ".fits"

    } else if (imaccess (field)) {
        iferr { wcsinfo (field, verb-) } then {
            error (0, "Cannot determine image coords for `"//field//"'")
        } else {
            fra   = wcsinfo.midx
            fdec  = wcsinfo.midy
            sz    = max (wcsinfo.width, wcsinfo.height) / 60.0
	    img   = field
            isim  = yes
	    field = strsub (field, "$", "_")
	    imname = field
        }
    } else {
	sesame (field, verbose-)
        fra  = sesame.ra
        fdec = sesame.dec
	img  = "cache$" // field// "_t.fits"
	imname = "cache$" // field// ".fits"
        isim = no
    }
    
    # Form the query string.
    printf ("POS=%.5f,%.5f&SIZE=%.3f", fra, fdec, sz) | scan (query)
    url = base // query
    if (vo.runid != "")
        url = url // "&RUNID=" // vo.runid


    # Perform the SIA query string.
    urlget (url, restab, extn="xml", use_cache=fcache, cache="cache$")

    # Extract the URL from the first row, this'll be the image we use.
    tdump (restab, row=1, col="Url", pwidth=2048, cdfile="dev$null",
	pfile="dev$null") | scan (imurl)

    # Download the image.
    if (vo.runid != "")
        imurl = imurl // "&RUNID=" // vo.runid
    urlget (imurl, img, extn="fits", use_cache=fcache, cache="cache$")
    if (lsave) {
        #copy ("cache$" // field //"_t.fits", "cache$" // field //".fits")
        copy (img, "cache$" // field //".fits")
	img = "cache$" // field // ".fits"
    } else {
	if (fcache)
	    img = "cache$" // field // "_t.fits[0]"
    }
    ;

    #  Display the image.
    if (use_display)
        display (img, frame, fill+, >& "dev$null")

    if (do_grid) 
        wcslab (img, 1, use-, fill+, overplot+, append+, 
            labout-, dev="imdc")

    # clean up
    delete (restab, verify-) 				
    if (! isim && ! lsave)
    	imdelete (img, go_ahead+, verify-, >& "dev$null")
end
