# WCSINFO --

procedure wcsinfo (image)

string	image			{ prompt="Image name" 	    	}
bool	verbose = no		{ prompt="Verbose?"	    	}
  
string	pos 	= ""		{ prompt="POS string"		}
real	size 	= 0.0		{ prompt="SIZE value"    	}
real	ra 	= 0.0		{ prompt="RA value"    		}
real	dec 	= 0.0		{ prompt="DEC value"    	}

real	llx 	= 0.0		{ prompt="LLX wcs corner"    	}
real	lly 	= 0.0		{ prompt="LLY wcs corner"    	}
real	urx 	= 0.0		{ prompt="URX wcs corner"    	}
real	ury 	= 0.0		{ prompt="URY wcs corner"    	}
real	midx 	= 0.0		{ prompt="X wcs midpoint"    	}
real	midy 	= 0.0		{ prompt="Y wcs midpoint"    	}
real	scale 	= 0.0		{ prompt="Scale (\"/pix)"    	}
real	rot 	= 0.0		{ prompt="Rotation (deg)"    	}
real	width 	= 0.0		{ prompt="Plt width (arcmin)"	}
real	height 	= 0.0		{ prompt="Plt height (arcmin)"	}
string	axmap 	= ""		{ prompt="Axis mapping"		}
real	epoch 	= 0.0		{ prompt="Epoch"		}
real	equinox	= 0.0		{ prompt="Equinox"		}
string	ctype1	= ""		{ prompt="CTYPE1"		}
string	ctype2	= ""		{ prompt="CTYPE2"		}
real	crval1	= 0.0		{ prompt="CRVAL1"		}
real	crval2	= 0.0		{ prompt="CRVAL2"		}
real	crpix1	= 0.0		{ prompt="CRPIX1"		}
real	crpix2	= 0.0		{ prompt="CRPIX2"		}
real	cd11	= 0.0		{ prompt="CD1_1"		}
real	cd12	= 0.0		{ prompt="CD1_2"		}
real	cd21	= 0.0		{ prompt="CD2_1"		}
real	cd22	= 0.0		{ prompt="CD2_2"		}
bool	ispix 	= no		{ prompt="Pixel only coords?" }

begin
    string  img, lpos
    real    cdelt1, cdelt2			# old wcs/pix increment
    real    crota1, crota2			# old rotation keywords
    real    sys_epoch, sys_equinox		# epoch info
    real    xrot, yrot				# plate scale and rotation
    real    x1, x2, y1, y2			# corner pos
    real    lllx, llly, lurx, lury, lmx, lmy 
    int	    nx, ny, cx, cy
    string  imtmp, img_orig, cfile, ofile
    bool    verb


    # Check for proper packages and reset environment.
    reset imtype = "fits"
    #flpr 0


    # Get the task parameters.
    img	    = image
    verb    = verbose

    # Initialize.
    cd11    = 1.0  ;   cd12    = 0.0
    cd21    = 0.0  ;   cd22    = 1.0
    crpix1  = 0.0  ;   crpix2  = 0.0
    crval1  = 0.0  ;   crval2  = 0.0
    cdelt1  = 0.0  ;   cdelt2  = 0.0
    ctype1  = ""   ;   ctype2  = ""
    crota1  = 0.0  ;   crota2  = 90.0	# assume normal x/y axes and
    scale   = 1.0  ;   rot     = 0.0	# default to 1"/pix scale
    xrot    = 0.0  ;   yrot    = 0.0  ; rot = 0.0
    sys_epoch   = 0.0
    sys_equinox = 0.0

    # Work on a copy of the image, not the original.
    imtmp = mktemp ("/tmp/w")
    imcopy (img, imtmp, verbose-)
    img_orig = img
    img = imtmp

    # Get the size and midpoint of the image (in pixels).
    hselect (img, "i_naxis1,i_naxis2", yes) | scan (nx, ny)
    cx = nx / 2
    cy = ny / 2


    # First, see whether we have FITS standard keywords or a plate
    # solution header (e.g. from DSS).  If the latter, fix it.
    hselect (img, "cd1_1,cd2_2", yes) | scan (x, y)
    if (nscan() < 2) {
      hselect (img, "amdx1,amdy1", yes) | scan (x, y)
      if (nscan() >= 1) {
	if (verb)
	    print ("Converting platesol header to std WCS... ")
	makewcs (img, verbose-)
      }
    }


    # Now convert the image explicitly to FK5.  This will also give us a
    # standard CD matrix instead of the CROTA/CDELT keywords
    imcctran (img, "FK5", nx=20, ny=20, longpole-, verbose-, update+)

    hselect (img, "crval1,crval2", yes) | scan (crval1, crval2)
    if (nscan() != 2) { 
	if (verb)
	    print ("No CRVAL keywords")
    }

    hselect (img, "crpix1,crpix2", yes) | scan (crpix1, crpix2)
    if (nscan() != 2) { 
	if (verb)
	    print ("No CRPIX keywords")
    }

    hselect (img, "ctype1,ctype2", yes) | scan (ctype1, ctype2)
    if (nscan() == 2) { 
	s1 = strlwr (ctype1)
	if (strstr ("dec", s1) == 1 || strstr ("lat", s1) == 1) {
	    axmap = "2 1"
	} else if (strstr ("ra", s1) == 1 || strstr ("lon", s1) == 1) {
	    axmap = "1 2"
	} else {
	    axmap = "1 2"
	    ispix = yes
	}
	;
	if (strstr ("-sip", strlwr (s1)) > 0) {
	    ctype1 = substr (ctype1, 1, (strlen(ctype1)-4))
	    ctype2 = substr (ctype2, 1, (strlen(ctype2)-4))
	    hedit (img, "ctype1", ctype1, verify-,update+,show-, >& "dev$null")
	    hedit (img, "ctype2", ctype2, verify-,update+,show-, >& "dev$null")
	    hedit (img, "WAT0_001", "", verify-,del+,update+,show-,>&"dev$null")
	    hedit (img, "WAT1_001", "", verify-,del+,update+,show-,>&"dev$null")
	    hedit (img, "WAT2_001", "", verify-,del+,update+,show-,>&"dev$null")
	}
	;

	# Check for an invalid WAT keyword suggesting physical coords.  If we
	# have them and CTYPE keywords, delete the WAT values.
        hselect (img, "WAT0_001", "yes") | scan (s1)
	if (s1 == "system=physical") {
	    hedit (img, "WAT0_001", del+, verify-, show-, update+)
	    hedit (img, "WAT1_001", del+, verify-, show-, update+)
	    hedit (img, "WAT2_001", del+, verify-, show-, update+)
	    hedit (img, "LTM1_1", del+, verify-, show-, update+)
	    hedit (img, "LTM2_2", del+, verify-, show-, update+)
	    hedit (img, "LTV1", del+, verify-, show-, update+)
	    hedit (img, "LTV2", del+, verify-, show-, update+)
	}
	;

    } else if (verb) {
	print ("No CTYPE keywords")
    }


    if (ispix) {
      # See if we have an RA/DEC keyword and put it at the middle of the image.
      hselect (img, "ra,dec", yes) | scan (x, y)
      if (nscan() == 2) {
	crval1 = x * 15.
	if (x > 24)
	    crval1 = x
	crval2 = y
	crpix1 = cx
	crpix2 = cy
	scale  = 1.
	rot    = 0.
      }


    } else {
      # If we have CD matrix keywords use 'em, otherwise try to fall back
      # to the older CDELT keywords.
      hselect (img, "cd1_1,cd2_2", yes) | scan (x, y)
      if (nscan() >= 1) {
        # Get the CD matrix from the image.  We initialize the values above
        # so if they don't exist in the header we won't change the value here
        # (but we need to read them one at a time).
        hselect (img, "cd1_1", yes) | scan (cd11)
        hselect (img, "cd1_2", yes) | scan (cd12)
        hselect (img, "cd2_1", yes) | scan (cd21)
        hselect (img, "cd2_2", yes) | scan (cd22)

        # Compute the plate scale (arcsec/pixel) for the image.
        scale = 3600. * sqrt ((cd11**2+cd21**2+cd12**2+cd22**2)/2.)
	xrot  = abs (datan ( cd21, cd11))
	yrot  = abs (datan (-cd12, cd22))
	rot   = (xrot + yrot) / 2.0

      } else {
        hselect (img, "cdelt1,cdelt2", yes) | scan (cdelt1,cdelt2)
        if (nscan() == 2)
	    scale = 3600. * sqrt ((cdelt1**2 + cdelt2**2) / 2.)

        hselect (img, "crota1", yes) | scan (crota1)
        if (nscan() == 1)
            xrot   = crota1
        hselect (img, "crota2", yes) | scan (crota2)
        if (nscan() == 1)
            yrot   = crota2
	rot = yrot

      }
    }


    # Get the epoch and equinox
    x = -1.0 ; y = -1.0
    hselect (img, "epoch", yes) | scan (x)
    if (nscan() == 1 && x > 0.0)
        sys_epoch = x
    else
        sys_epoch = 2000.0

    hselect (img, "equinox", yes) | scan (x)
    if (nscan() == 1 && y > 0.0)
        sys_equinox = y
    else
        sys_equinox = 2000.0

    epoch   = sys_epoch
    equinox = sys_equinox

    if (ispix) {

        lllx    = (crval1 - (cx/3600.))/15.;		# assumes hours of RA
	llly    = (crval2 - (cy/3600.));
        lurx    = (crval1 + (cx/3600.))/15.;		# assumes hours of RA
	lury    = (crval2 + (cy/3600.));
         lmx    = crval1 / 15.
         lmy    = crval2

	 llx    = lllx * 15.0	;    lly    = llly
	 urx    = lurx * 15.0	;    ury    = lury
        midx    = lmx * 15.0	;   midy    = lmy

    } else {

        cfile = mktemp ("/tmp/cf")
        ofile = mktemp ("/tmp/of")
        print ("1.0    1.0", 	>  cfile)
        print (nx // "    " // ny,  >> cfile)
        print (cx // "    " // cy,  >> cfile)
        skyctran (cfile, ofile, img, img//" world", >& "dev$null")

        tail (ofile, nl=1) | head ("STDIN", nl=1) | scan (x, y, lmx,  lmy )
        tail (ofile, nl=2) | head ("STDIN", nl=1) | scan (x, y, lurx, lury)
        tail (ofile, nl=3) | head ("STDIN", nl=1) | scan (x, y, lllx, llly)

        # Update the task parameters.
        if (axmap == "1 2") {
             llx   = lllx * 15.0	;  lly   = llly
             urx   = lurx * 15.0	;  ury   = lury
            midx   = lmx  * 15.0	; midy   = lmy
    	      ra   = midx		;   dec  = midy

        } else {
            lly    = lllx * 15.0	;  llx   = llly
            ury    = lurx * 15.0	;  urx   = lury
           midy    = lmx  * 15.0	; midx   = lmy
    	      ra   = midy	        ;   dec  = midx
        }
    	printf ("%g,%g\n", ra, dec) | scan (lpos)
	pos = lpos
    	size = max ((scale * nx), (scale * ny)) / 3600.	    # in degrees

        delete (cfile, verify-, >& "dev$null")
        delete (ofile, verify-, >& "dev$null")
    }
    width  = real (scale * nx) / 60. 		# plate width in arcmin
    height = real (scale * ny) / 60. 		# plate height in arcmin


    if (verb) {
	print  ("CRPIX1  = " // crpix1)
	print  ("CRPIX2  = " // crpix2)
	print  ("CTYPE1  = " // ctype1)
	print  ("CTYPE2  = " // ctype2)
	printf ("CRVAL1  = %g\t/ X wcs = %.3H\n", crval1, crval1)
	printf ("CRVAL2  = %g\t/ Y wcs = %.3h\n", crval2, crval2)
	print  ("CD1_1   = " // cd11)
	print  ("CD1_2   = " // cd12)
	print  ("CD2_1   = " // cd21)
	print  ("CD2_2   = " // cd22)
	print  ("CDELT1  = " // cdelt1) ; print ("CDELT2  = " // cdelt2)
	print  ("CROTA1  = " // crota1) ; print ("CROTA2  = " // crota2)

        print  ("EPOCH   = " // sys_epoch)
        print  ("EQUINOX = " // sys_equinox)
        print  ("PLTSCALE= " // scale // " 	/ arc/pix")
        print  ("PLT_ROT = " // rot // " 	/ degrees")
        printf ("PLT_WID = %-14.9g\t/ Field width (arcmin)\n", width);
        printf ("PLT_HGT = %-14.9g\t/ Field height (arcmin)\n", height);
        printf ("PLT_LLX = %-14.9g\t/ LL  X wcs = %.3h\n",  lllx*15., lllx)
        printf ("PLT_URX = %-14.9g\t/ UR  X wcs = %.3h\n",  lurx*15., lurx)
        printf ("PLT_MX  = %-14.9g\t/ Mid X wcs = %.3h\n", lmx*15.,  lmx)
        printf ("PLT_LLY = %-14.9g\t/ LL  Y wcs = %.3h\n",  llly, llly)
        printf ("PLT_URY = %-14.9g\t/ UR  Y wcs = %.3h\n",  lury, lury)
        printf ("PLT_MY  = %-14.9g\t/ Mid Y wcs = %.3h\n", lmy,  lmy)
        printf ("AXMAP   = %s\t\t\t/ Axis mapping\n", axmap)
        printf ("ISPIX   = %b\t\t\t/ Pixel mapping?\n", ispix)
    }

    # Clean up.
    if (imaccess (imtmp) == yes)
        delete (imtmp//".fits", verify-, >& "dev$null")
end
