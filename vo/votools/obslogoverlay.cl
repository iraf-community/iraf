#{  OBSLOGOVERLAY -- Overlay an observation catalog on the display.

procedure obslogoverlay (field, mission)

string	field			{ prompt = "Query image/field name"	}
string	mission			{ prompt = "Mission name?"		}

real	size    = 0.25		{ prompt = "Query size"			}
int	frame   = 1		{ prompt = "Display frame"		}
bool	display = yes		{ prompt = "Display image/field?"	}
bool	print   = no		{ prompt = "List instead of overlay?"	}
int	mkcolor = 0		{ prompt = "Marker color"		}
int	mksize  = 60		{ prompt = "Marker size (arcsec)"	}
int	maxobs  = 0		{ prompt = "Max observations to draw"	}

bool	verbose = yes		{ prompt = "Verbose output?"		}
int	status = 0		{ prompt = "Service status code"	}

begin
    string obs, lname, olog, tvcoords, rad, raw, pcols
    real   ra, dec, sz, scale
    bool   verb, disp, llist
    int    mcol, fr, nres, max, mksz


    lname = field		# Get params to local variables
    obs   = mission
    fr    = frame
    sz    = size
    max   = maxobs
    mksz  = mksize * 10
    mcol  = mkcolor
    verb  = verbose
    llist = print
    if (llist)
	disp = no
    else
        disp  = display


    if (imaccess (lname) == yes) {
        iferr { wcsinfo (lname) } then {
    	    status = 1
            error (0, "Cannot determine image coords for `"//lname//"'")
        } else {
            ra    = wcsinfo.midx
            dec   = wcsinfo.midy
            sz    = wcsinfo.size
            scale = wcsinfo.scale
	    if (disp)
	        display (lname, fr, >& "dev$null")
        }
    } else {
        sesame (lname, verbose-)
        ra    = sesame.ra
        dec   = sesame.dec
        scale = 0.1
	if (disp)
	    dss (lname, use_disp+)
	lname = "cache$" // lname // ".fits"
    }
    

    # Do a generic 1' box
    printf ("%.1f,%.1f\n", (scale*mksz), (scale*mksz)) | scan(rad)

    # Create temp file for the output
    olog     = mktemp ("tmp$olog")
    raw      = mktemp ("tmp$olog")
	      
    # Get observation log sources
    switch (substr (strupr (obs), 1, 1)) {
	case "E":			# EXOSAT Observation log
	    { vodata ("ivo://nasa.heasarc/exolog", "", ra=ra, dec=dec, 
		size=sz, format="raw", >& raw)
	      pcols = "4,5"
	      if (mcol == 0)
	          mcol = 206
	    }
	case "F":			# FUSE Observation log
	    { vodata ("ivo://nasa.heasarc/fuselog", "", ra=ra, dec=dec, 
		size=sz, format="raw", >& raw)
	      pcols = "4,5"
	      if (mcol == 0)
	          mcol = 208
	    }
	case "H":			# HST Observation log
	    { vodata ("ivo://nasa.heasarc/hstaec", "", ra=ra, dec=dec, 
		size=sz, format="raw", >& raw)
	      pcols = "3,4"
	      if (mcol == 0)
	          mcol = 204
	    }
	case "I":			# IUE Observation log
	    { vodata ("ivo://nasa.heasarc/iuelog", "", ra=ra, dec=dec, 
		size=sz, format="raw", >& raw)
	      pcols = "3,4"
	      if (mcol == 0)
	          mcol = 208
	    }
	case "S":			# Spitzer Observation log
	    { vodata ("ivo://nasa.heasarc/spitzmastr", "", ra=ra, dec=dec, 
		size=sz, format="raw", >& raw)
	      pcols = "3,4"
	      if (mcol == 0)
	          mcol = 205
	    }
	case "X":			# XMM Observation log
	    { vodata ("ivo://nasa.heasarc/xmmmaster", "", ra=ra, dec=dec, 
		size=sz, format="raw", >& raw)
	      pcols = "5,6"
	      if (mcol == 0)
	          mcol = 207
	    }

	# =================================================================
	#    FIXME -- These no longer work.  Mar2012
	# =================================================================
	case "R":			# Rosat Observation log
	    { # FIXME
	      vodata ("rosatlog", "", ra=ra, dec=dec, size=sz, 
		format="raw", >& raw)
	      pcols = "10,11"
	      if (mcol == 0)
		 mcol = 208
	    }
	case "C":			# Chandra Observation log
	    { # FIXME
	      vodata ("ivo://CDS.VizieR/B/chandra", "", ra=ra, dec=dec, 
		size=sz, format="raw", >& raw)
	      pcols = "3,4"
	      if (mcol == 0)
		 mcol = 205
	    }
    }

    votpos (raw, number-, >& olog)
    count (olog) | scan (nres)
    if (max == 0)
	max = nres
    if (verbose)
	printf ("%d observations found for %s\n", max, strupr(obs))


    # Mark the display.
    if (llist) {
	type (raw)
    } else {
        tvcoords = mktemp ("tmp$tvc")

	type (olog) | head ("STDIN", nl=max) | \
          wcsctran ("STDIN", tvcoords, lname, verbose=verb,
            inwcs="world", outwcs="logical", units="n n")
        tvmark (frame=fr, coords=tvcoords, mark="rectangle", length=rad, 
	    color=mcol, txsize=1, lab+)

        delete (tvcoords, verify-, >& "dev$null")
    }

    # Clean up.
    #delete (raw,  verify-, >& "dev$null")
    #delete (olog, verify-, >& "dev$null")

    status = 0
end

