include <fset.h>
include <imhdr.h>
include <mwset.h>
include <math.h>
include <pkg/skywcs.h>
include "wcsxymatch.h"

# T_SKYXYMATCH -- Compute a list of the tie points required to register an
# image to a reference image using WCS information in the image headers and
# the celestial coordinate transformation routines.

procedure t_skyxymatch()

bool	verbose
double	xmin, xmax, ymin, ymax, x1, x2, y1, y2
int	ilist, rlist, olist, clist, cfd, ofd
int	nx, ny, wcs, min_sigdigits, xcolumn, ycolumn, xunits, yunits
int	rstat, stat, npts
pointer	sp, refimage, image, xformat, yformat, rxformat, ryformat
pointer	rwxformat, rwyformat, txformat, tyformat, twxformat, twyformat, str
pointer	imr, im, mwr, mw, coor, coo, ctr, ct
pointer	rxl, ryl, rxw, ryw, trxw, tryw, ixl, iyl

bool	clgetb(), streq()
double	clgetd()
int	imtopen(), fntopnb(), clgeti(), clgwrd(), strdic(), imtlen()
int	fntlenb(), imtgetim(), fntgfnb(), open(), mw_stati(), sk_decim()
int	rg_rdxy(), rg_xytoxy(), sk_stati()
pointer	immap()
errchk	mw_gwattrs()

begin
	# Get some temporary working space.
	call smark (sp)
	call salloc (refimage, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (xformat, SZ_FNAME, TY_CHAR)
	call salloc (yformat, SZ_FNAME, TY_CHAR)
	call salloc (rwxformat, SZ_FNAME, TY_CHAR)
	call salloc (rwyformat, SZ_FNAME, TY_CHAR)
	call salloc (rxformat, SZ_FNAME, TY_CHAR)
	call salloc (ryformat, SZ_FNAME, TY_CHAR)
	call salloc (twxformat, SZ_FNAME, TY_CHAR)
	call salloc (twyformat, SZ_FNAME, TY_CHAR)
	call salloc (txformat, SZ_FNAME, TY_CHAR)
	call salloc (tyformat, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the input image and output file lists.
	call clgstr ("input", Memc[str], SZ_FNAME)
	ilist = imtopen (Memc[str])
	call clgstr ("reference", Memc[str], SZ_FNAME)
	rlist = imtopen (Memc[str])
	call clgstr ("output", Memc[str], SZ_FNAME)
	if (Memc[str] == EOS)
	    call strcpy ("STDOUT", Memc[str], SZ_FNAME)
	olist = fntopnb (Memc[str], NO) 

	# Determine the source of the input coordinates.
	call clgstr ("coords", Memc[str], SZ_FNAME)
	if (streq (Memc[str], "grid")) {
	    clist = NULL
	    xmin = clgetd ("xmin")
	    xmax = clgetd ("xmax")
	    ymin = clgetd ("ymin")
	    ymax = clgetd ("ymax")
	    nx = clgeti ("nx")
	    ny = clgeti ("ny")
	    wcs = clgwrd ("wcs", Memc[str], SZ_FNAME, RG_WCSLIST)
	} else {
	    clist = fntopnb (Memc[str], NO) 
	    xmin = INDEFD
	    xmax = INDEFD
	    ymin = INDEFD
	    ymax = INDEFD
	    nx = clgeti ("nx")
	    ny = clgeti ("ny")
	    wcs = clgwrd ("wcs", Memc[str], SZ_FNAME, RG_WCSLIST)
	    xcolumn = clgeti ("xcolumn")
	    ycolumn = clgeti ("ycolumn")
	    call clgstr ("xunits", Memc[str], SZ_FNAME)
	    xunits = strdic (Memc[str], Memc[str], SZ_FNAME, RG_UNITLIST)
	    if (xunits <= 0)
		xunits = RG_UNATIVE
	    call clgstr ("yunits", Memc[str], SZ_FNAME)
	    yunits = strdic (Memc[str], Memc[str], SZ_FNAME, RG_UNITLIST)
	    if (yunits <= 0)
		yunits = RG_UNATIVE
	}

	# Get the output coordinate formatting information.
	call clgstr ("xformat", Memc[xformat], SZ_FNAME)
	call clgstr ("yformat", Memc[yformat], SZ_FNAME)
	call clgstr ("rwxformat", Memc[rxformat], SZ_FNAME)
	call clgstr ("rwyformat", Memc[ryformat], SZ_FNAME)
	call clgstr ("wxformat", Memc[txformat], SZ_FNAME)
	call clgstr ("wyformat", Memc[tyformat], SZ_FNAME)
	min_sigdigits = clgeti ("min_sigdigits")

	# Get remaining parameters.
	verbose = clgetb ("verbose")

	# Check the formatting of the reference and input logical coordinates.
	if (Memc[xformat] == EOS) {
	    call sprintf (Memc[xformat], SZ_FNAME, "%%%d.%dg")
		call pargi (min_sigdigits + 3)
		call pargi (min_sigdigits)
	}
	if (Memc[yformat] == EOS) {
	    call sprintf (Memc[yformat], SZ_FNAME, "%%%d.%dg")
		call pargi (min_sigdigits + 3)
		call pargi (min_sigdigits)
	}

	# Check the reference image list length.
	if (imtlen (rlist) <= 0)
	    call error (0, "The reference image list is empty.")
	if (imtlen(rlist) > 1 && imtlen(rlist) != imtlen(ilist))
	    call error (0,
	        "The number of reference and input images is not the same.")

	# Check the output coordinate file length.
	if (fntlenb(olist) > 1 && fntlenb(olist) != imtlen(ilist))
	    call error (0,
	"The number of output coords files and input images is not the same.")

	# Check the reference coordinate list length.
	if (clist != NULL) {
	    if (fntlenb (clist) != imtlen (rlist))
		call error (0,
	    "The number of reference coords files and images are not the same")
	}

	# Initialize the reference image and coordinate list pointers.
	imr = NULL
	cfd = NULL

	# Loop over the input images.
	while (imtgetim (ilist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the reference image and reference coordinate file and
	    # compute the logical and world reference coordinates.
	    if (imtgetim (rlist, Memc[refimage], SZ_FNAME) != EOF) {

		# Open the reference image.
		if (imr != NULL) {
		    call mfree (rxl, TY_DOUBLE)
		    call mfree (ryl, TY_DOUBLE)
		    call mfree (rxw, TY_DOUBLE)
		    call mfree (ryw, TY_DOUBLE)
		    call mfree (trxw, TY_DOUBLE)
		    call mfree (tryw, TY_DOUBLE)
		    call mfree (ixl, TY_DOUBLE)
		    call mfree (iyl, TY_DOUBLE)
		    if (mwr != NULL)
		        call mw_close (mwr)
		    if (coor != NULL)
			#call mfree (coor, TY_STRUCT)
			call sk_close (coor)
		    call imunmap (imr)
		}
		imr = immap (Memc[refimage], READ_ONLY, 0)
		if (IM_NDIM(imr) > 2)
		    call error (0, "The reference image must be 1D or 2D")

		# Open the reference image wcs.
		rstat = sk_decim (imr, "logical", mwr, coor)

		# Check that the wcs dimensions are rational.
		if (mwr != NULL) {
		    if (mw_stati(mwr, MW_NPHYSDIM) < IM_NDIM(imr) || 
			mw_stati (mwr, MW_NDIM) != IM_NDIM(imr)) {
			call mw_close (mwr)
			mwr = NULL
		    }
		}

		# Compute the x limits of the logical reference coordinates.
		if (IS_INDEFD(xmin))
		    x1 = 1.0d0
		else
		    x1 = max (1.0d0, min (xmin, double(IM_LEN(imr,1))))
		if (IS_INDEFD(xmax))
		    x2 = double(IM_LEN(imr,1))
		else
		    x2 = max (1.0d0, min (xmax, double(IM_LEN(imr,1))))

		# Compute the y limits of the logical reference coordinates.
		if (IM_NDIM(imr) == 1)
		    y1 = 1.0d0
		else if (IS_INDEFD(ymin))
		    y1 = 1.0d0
		else
		    y1 = max (1.0d0, min (ymin, double(IM_LEN(imr,2))))
		if (IM_NDIM(imr) == 1)
		    y2 = 1.0d0
		else if (IS_INDEFD(ymax))
		    y2 = double(IM_LEN(imr,2))
		else
		    y2 = max (1.0d0, min (ymax, double(IM_LEN(imr,2))))

		# Compute the reference logical and world coordinates.
		if (clist != NULL) {

		    if (cfd != NULL)
			call close (cfd)

		    if (fntgfnb (clist, Memc[str], SZ_FNAME) != EOF) {
			cfd = open (Memc[str], READ_ONLY, TEXT_FILE)
			npts = rg_rdxy (cfd, rxw, ryw, wcs, xcolumn, ycolumn,
			    xunits, yunits)
		        call malloc (trxw, npts, TY_DOUBLE)
		        call malloc (tryw, npts, TY_DOUBLE)
		        call malloc (rxl, npts, TY_DOUBLE)
		        call malloc (ryl, npts, TY_DOUBLE)
		        call malloc (ixl, npts, TY_DOUBLE)
		        call malloc (iyl, npts, TY_DOUBLE)
			if (wcs == RG_WORLD)
		    	    ctr = rg_xytoxy (mwr, Memd[rxw], Memd[ryw],
			        Memd[rxl], Memd[ryl], npts, "world", "logical",
				1, 2)
			else
		    	    ctr = rg_xytoxy (mwr, Memd[rxw], Memd[ryw],
			        Memd[rxl], Memd[ryl], npts, "physical",
				"logical", 1, 2)
		    }

		} else {

		    if (IM_NDIM(imr) == 1)
			npts = nx
		    else
		        npts = nx * ny
		    call malloc (rxl, npts, TY_DOUBLE)
		    call malloc (ryl, npts, TY_DOUBLE)
		    call malloc (rxw, npts, TY_DOUBLE)
		    call malloc (ryw, npts, TY_DOUBLE)
		    call malloc (trxw, npts, TY_DOUBLE)
		    call malloc (tryw, npts, TY_DOUBLE)
		    call malloc (ixl, npts, TY_DOUBLE)
		    call malloc (iyl, npts, TY_DOUBLE)
		    if (IM_NDIM(imr) == 1)
		        call rg_rxyl (Memd[rxl], Memd[ryl], nx, 1, x1, x2,
			    y1, y2)
		    else
		        call rg_rxyl (Memd[rxl], Memd[ryl], nx, ny, x1, x2,
			    y1, y2)
		    if (wcs == RG_WORLD)
		        ctr = rg_xytoxy (mwr, Memd[rxl], Memd[ryl], Memd[rxw],
			    Memd[ryw], npts, "logical", "world", 1, 2)
		    else
		        ctr = rg_xytoxy (mwr, Memd[rxl], Memd[ryl], Memd[rxw],
			    Memd[ryw], npts, "logical", "physical", 1, 2)

		}
	    }

	    # Open the input image.
	    im = immap (Memc[image], READ_ONLY, 0)
	    if (IM_NDIM(im) > 2)
		call error (0, "The input image must be 1D or 2D")
	    if (IM_NDIM(im) != IM_NDIM(imr))
		call error (0,
	    "The input image must have same dimensionality as reference image")

	    # Open the input wcs.
	    stat = sk_decim (im, "logical", mw, coo)
	    if (mw != NULL) {
		if (mw_stati(mw, MW_NPHYSDIM) < IM_NDIM(im) || 
		    mw_stati (mw, MW_NDIM) != IM_NDIM(im)) {
		    call mw_close (mw)
		    mw = NULL
		}
	    }

	    # Open the output file.
	    if (fntgfnb (olist, Memc[str], SZ_FNAME) != EOF)
		ofd = open (Memc[str], NEW_FILE, TEXT_FILE)

	    # Print information about the reference and input coordinate
	    # systems and the reference and input files to the output
	    # file
	    if (ofd == STDOUT)
	        call fseti (ofd, F_FLUSHNL, YES)
	    if (streq (Memc[str], "STDOUT") || ofd == STDOUT)
                call fseti (ofd, F_FLUSHNL, YES)
            call fprintf (ofd, "\n")
	    call fprintf (ofd,
	        "# Reference image: %s  Input image: %s\n#     Coords: %s")
	        call pargstr (Memc[refimage])
	        call pargstr (Memc[image])
	    if (clist == NULL) {
		call pargstr ("grid")
		call fprintf (ofd, " Wcs: logical\n")
	    } else {
		call fstats (cfd, F_FILENAME, Memc[str], SZ_FNAME)
		call pargstr (Memc[str])
		call fprintf (ofd, " Wcs: %s\n")
		switch (wcs) {
		case RG_PHYSICAL:
		    call pargstr ("physical")
		case RG_WORLD:
		    call pargstr ("world")
		default:
		    call pargstr ("world")
		}
	    }
            if (rstat == ERR)
                call fprintf (ofd,
                    "# Error decoding the reference coordinate system\n")
            call sk_iiwrite (ofd, "Refsystem", Memc[refimage], mwr, coor)
            if (stat == ERR)
                call fprintf (ofd,
                    "# Error decoding the input coordinate system\n")
            call sk_iiwrite (ofd, "Insystem", Memc[image], mw, coo)

	    # Print information about the reference and input coordinate
	    # systems and the reference and input files to the standard
	    # output.
	    if (verbose && ofd != STDOUT) {
		call printf ("\n")
		call printf (
		    "Reference image: %s  Input image: %s\n    Coords: %s")
		    call pargstr (Memc[refimage])
		    call pargstr (Memc[image])
		if (clist == NULL) {
		    call pargstr ("grid")
		    call printf (" Wcs: logical\n")
		} else {
		    call fstats (cfd, F_FILENAME, Memc[str], SZ_FNAME)
		    call pargstr (Memc[str])
		    call printf (" Wcs: %s\n")
		    switch (wcs) {
		    case RG_PHYSICAL:
			call pargstr ("physical")
		    case RG_WORLD:
			call pargstr ("world")
		    default:
			call pargstr ("world")
		    }
		}
                if (rstat == ERR)
                    call printf (
                        "Error decoding the rference coordinate system\n")
                call sk_iiprint ("Refsystem", Memc[refimage], mwr, coor)
                if (stat == ERR)
                    call printf (
                        "Error decoding the input coordinate system\n")
                call sk_iiprint ("Insystem", Memc[image], mw, coo)
                call printf ("\n")
	    }

	    # Set the reference and input coordinate formats.
	    if (Memc[rxformat] == EOS)
		call rg_ssetfmt (mwr, wcs, sk_stati(coor, S_XLAX),
		    min_sigdigits, Memc[rwxformat], SZ_FNAME)
	    else
		call strcpy (Memc[rxformat], Memc[rwxformat], SZ_FNAME)

	    if (Memc[txformat] == EOS)
		call rg_ssetfmt (mw, wcs, sk_stati(coo, S_XLAX),
		    min_sigdigits, Memc[twxformat], SZ_FNAME)
	    else
		call strcpy (Memc[txformat], Memc[twxformat], SZ_FNAME)
	    if (Memc[ryformat] == EOS)
		call rg_ssetfmt (mwr, wcs, sk_stati(coor, S_YLAX),
		    min_sigdigits, Memc[rwyformat], SZ_FNAME)
	    else
		call strcpy (Memc[ryformat], Memc[rwyformat], SZ_FNAME)
	    if (Memc[tyformat] == EOS)
		call rg_ssetfmt (mw, wcs, sk_stati(coo, S_YLAX),
		    min_sigdigits, Memc[twyformat], SZ_FNAME)
	    else
		call strcpy (Memc[tyformat], Memc[twyformat], SZ_FNAME)


	    # Compute the output coordinates issuing a warning if the
	    # axes types are not compatable.
	    if (mwr == NULL || rstat == ERR) {
		call fprintf (ofd,
		    "# \tWarning: error decoding reference image wcs\n")
		if (verbose && ofd != STDOUT)
		    call printf (
		        "\tWarning: error decoding reference image wcs\n")
		if (IM_NDIM(imr) == 1)
		    call rg_rxyl (Memd[rxl], Memd[ryl], nx, 1, 1.0d0,
		        double(IM_LEN(im,1)), 1.0d0, 1.0d0)
		else
		    call rg_rxyl (Memd[rxl], Memd[ryl], nx, ny, 1.0d0,
		        double(IM_LEN(im,1)), 1.0d0, double(IM_LEN(im,2)))
		call amovd (Memd[rxl], Memd[ixl], npts)
		call amovd (Memd[ryl], Memd[iyl], npts)
		if (clist == NULL) {
		    call amovd (Memd[rxl], Memd[rxw], npts)
		    call amovd (Memd[ryl], Memd[ryw], npts)
		    call amovd (Memd[rxl], Memd[trxw], npts)
		    call amovd (Memd[ryl], Memd[tryw], npts)
		}
		ct = NULL
	    } else if (ctr == NULL) {
		call fprintf (ofd, "# \tWarning: Unable to compute reference \
logical <-> world transform\n")
		if (verbose && ofd != STDOUT)
		    call printf ("\tWarning: Unable to compute reference \
logical <-> world transform\n")
		if (IM_NDIM(imr) == 1)
		    call rg_rxyl (Memd[rxl], Memd[ryl], nx, 1, 1.0d0,
		        double(IM_LEN(im,1)), 1.0d0, 1.0d0)
		else
		    call rg_rxyl (Memd[rxl], Memd[ryl], nx, ny, 1.0d0,
		        double(IM_LEN(im,1)), 1.0d0, double(IM_LEN(im,2)))
		call amovd (Memd[rxl], Memd[ixl], npts)
		call amovd (Memd[ryl], Memd[iyl], npts)
		if (clist == NULL) {
		    call amovd (Memd[rxl], Memd[rxw], npts)
		    call amovd (Memd[ryl], Memd[ryw], npts)
		    call amovd (Memd[rxl], Memd[trxw], npts)
		    call amovd (Memd[ryl], Memd[tryw], npts)
		}
		ct = NULL
	    } else if (mw == NULL || stat == ERR) {
		call fprintf (ofd,
		    "# \tWarning: error decoding input image wcs\n")
		if (verbose && ofd != STDOUT)
		    call printf ("\tWarning: error decoding input image wcs\n")
		call amovd (Memd[rxl], Memd[ixl], npts)
		call amovd (Memd[ryl], Memd[iyl], npts)
		call amovd (Memd[rxw], Memd[trxw], npts)
		call amovd (Memd[ryw], Memd[tryw], npts)
		ct = NULL
	    } else {
		# Check axis status.
		if (wcs == RG_PHYSICAL) {
	            ct = rg_xytoxy (mw, Memd[rxw], Memd[ryw], Memd[ixl],
		        Memd[iyl], npts, "physical", "logical", 1, 2)
		    call amovd (Memd[rxw], Memd[trxw], npts)
		    call amovd (Memd[ryw], Memd[tryw], npts)
		    if (ct == NULL) {
		        call fprintf (ofd,
			    "# \tWarning: Unable to compute image physical -> \
logical transform\n")
			if (verbose && ofd != STDOUT)
		            call printf (
		                "\tWarning: Unable to compute image physical \
-> logical transform\n")
			if (IM_NDIM(imr) == 1)
		    	    call rg_rxyl (Memd[rxl], Memd[ryl], nx, 1, 1.0d0,
		        	double(IM_LEN(im,1)), 1.0d0, 1.0d0)
			else
		    	    call rg_rxyl (Memd[rxl], Memd[ryl], nx, ny, 1.0d0,
		        	double(IM_LEN(im,1)), 1.0d0,
				double(IM_LEN(im,2)))
			call amovd (Memd[rxl], Memd[ixl], npts)
			call amovd (Memd[ryl], Memd[iyl], npts)
		    }
		} else {
		    call rg_lltransform (coor, coo, Memd[rxw], Memd[ryw],
			Memd[trxw], Memd[tryw], npts)
		    if ((sk_stati (coor, S_PLNGAX) < sk_stati(coor,
		        S_PLATAX)) && (sk_stati (coo,S_PLNGAX) <
			sk_stati(coo, S_PLATAX)))
	                ct = rg_xytoxy (mw, Memd[trxw], Memd[tryw], Memd[ixl],
		            Memd[iyl], npts, "world", "logical", 1, 2)
		    else if ((sk_stati (coor, S_PLNGAX) > sk_stati(coor,
		        S_PLATAX)) && (sk_stati (coo,S_PLNGAX) >
			sk_stati(coo, S_PLATAX)))
	                ct = rg_xytoxy (mw, Memd[trxw], Memd[tryw], Memd[ixl],
		            Memd[iyl], npts, "world", "logical", 1, 2)
		    else
	                ct = rg_xytoxy (mw, Memd[tryw], Memd[trxw], Memd[ixl],
		            Memd[iyl], npts, "world", "logical", 1, 2)
		    if (ct == NULL) {
		        call fprintf (ofd,
			    "# \tWarning: Unable to compute image world -> \
logical transform\n")
			if (verbose && ofd != STDOUT)
		            call printf (
		                "\tWarning: Unable to compute image world -> \
logical transform\n")
			if (IM_NDIM(imr) == 1)
		    	    call rg_rxyl (Memd[rxl], Memd[ryl], nx, 1, 1.0d0,
		        	double(IM_LEN(im,1)), 1.0d0, 1.0d0)
			else
		    	    call rg_rxyl (Memd[rxl], Memd[ryl], nx, ny, 1.0d0,
		        	double(IM_LEN(im,1)), 1.0d0,
				double(IM_LEN(im,2)))
			call amovd (Memd[rxl], Memd[ixl], npts)
			call amovd (Memd[ryl], Memd[iyl], npts)
		    }
		}
	    }

	    # Write out the results.
	    if ((sk_stati (coor, S_PLNGAX) < sk_stati(coor, S_PLATAX)) &&
	        (sk_stati (coo,S_PLNGAX) < sk_stati(coo, S_PLATAX)))
	        call rg_swcoords (ofd, Memd[rxl], Memd[ryl], Memd[ixl],
		    Memd[iyl], Memd[rxw], Memd[ryw], Memd[trxw], Memd[tryw],
		    npts, Memc[xformat], Memc[yformat], Memc[rwxformat],
		    Memc[rwyformat], Memc[twxformat], Memc[twyformat])
	    else if ((sk_stati (coor, S_PLNGAX) > sk_stati(coor,
	        S_PLATAX)) && (sk_stati (coo,S_PLNGAX) > sk_stati(coo,
		S_PLATAX)))
	        call rg_swcoords (ofd, Memd[rxl], Memd[ryl], Memd[ixl],
		    Memd[iyl], Memd[rxw], Memd[ryw], Memd[trxw], Memd[tryw],
		    npts, Memc[xformat], Memc[yformat], Memc[rwxformat],
		    Memc[rwyformat], Memc[twxformat], Memc[twyformat])
	    else
	        call rg_swcoords (ofd, Memd[rxl], Memd[ryl], Memd[ixl],
		    Memd[iyl], Memd[rxw], Memd[ryw], Memd[tryw], Memd[trxw],
		    npts, Memc[xformat], Memc[yformat], Memc[rwxformat],
		    Memc[rwyformat], Memc[twxformat], Memc[twyformat])

	    # Close the input image and its wcs.
	    if (mw != NULL)
	        call mw_close (mw)
	    if (coo != NULL)
		#call mfree (coo, TY_STRUCT)
		call sk_close (coo)
	    call imunmap (im)

	    # Close the output coordinate file if it is not going to
	    # be appended to.
	    if (fntlenb(olist) == imtlen(ilist))
		call close (ofd)
	}

	if (imr != NULL) {
	    call mfree (rxl, TY_DOUBLE)
	    call mfree (ryl, TY_DOUBLE)
	    call mfree (rxw, TY_DOUBLE)
	    call mfree (ryw, TY_DOUBLE)
	    call mfree (trxw, TY_DOUBLE)
	    call mfree (tryw, TY_DOUBLE)
	    call mfree (ixl, TY_DOUBLE)
	    call mfree (iyl, TY_DOUBLE)
	    if (mwr != NULL)
	        call mw_close (mwr)
	    if (coor != NULL)
		#call mfree (coor, TY_STRUCT)
		call sk_close (coor)
	    call imunmap (imr)
	}
	if (cfd != NULL)
	    call close (cfd)
	if (fntlenb(olist) < imtlen(ilist))
	    call close (ofd)
	if (ilist != NULL)
	    call imtclose (ilist)
	if (rlist != NULL)
	    call imtclose (rlist)
	if (olist != NULL)
	    call fntclsb (olist)
	if (clist != NULL)
	    call fntclsb (clist)

	call sfree (sp)
end


# RG_SSETFMT -- Procedure to set the appropriate default format.

procedure rg_ssetfmt (mw, wcs, laxno, min_sigdigits, wformat, maxch)

pointer	mw			#I pointer to the image wcs
int	wcs			#I the input wcs type
int	laxno			#I the physical axis number
int	min_sigdigits		#I the minmum number of significant digits
char	wformat[ARB]		#O the output format string
int	maxch			#I the maximum size of the output format string

pointer	sp, str
bool	streq()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (mw == NULL) {
	    call sprintf (wformat, maxch, "%%%d.%dg")
		call pargi (min_sigdigits + 3)
		call pargi (min_sigdigits)
	} else if (wcs == RG_PHYSICAL) {
	    call strcpy ("%10.3f", wformat, maxch)
	}  else {
	    iferr {
		call mw_gwattrs (mw, laxno, "format", wformat, maxch)
	    } then {
		iferr {
		    call mw_gwattrs (mw, laxno, "axtype", Memc[str], SZ_FNAME)
		} then {
	    	    call sprintf (wformat, maxch, "%%%d.%dg")
		        call pargi (min_sigdigits + 3)
			call pargi (min_sigdigits)
		} else {
		    if (streq (Memc[str], "ra"))
                        call strcpy ("%12.2H", wformat, maxch)
                    else if (streq (Memc[str], "dec"))
                        call strcpy ("%11.1h", wformat, maxch)
                    else if (streq (Memc[str+1], "lon"))
                        call strcpy ("%11.1h", wformat, maxch)
                    else if (streq (Memc[str+1], "lat"))
                        call strcpy ("%11.1h", wformat, maxch)
                     else {
                        call sprintf (wformat, maxch, "%%%d.%dg")
                            call pargi (min_sigdigits + 3)
                            call pargi (min_sigdigits)
                    }
		}
	    }
	}

	call sfree (sp)
end


# RG_SWCOORDS -- Write out the reference and input logical coordinates of the
# tie points and the reference world coordinates.

procedure rg_swcoords (ofd, xref, yref, xin, yin, wxref, wyref, twxref, twyref,
	npts, xformat, yformat, wxformat, wyformat, twxformat, twyformat)

int	ofd			#I the output file descriptor
double	xref[ARB]		#I the reference logical x coordinates
double	yref[ARB]		#I the reference logical y coordinates
double	xin[ARB]		#I the input logical x coordinates
double	yin[ARB]		#I the input logical y coordinates
double	wxref[ARB]		#I the reference world x coordinates
double	wyref[ARB]		#I the reference world y coordinates
double	twxref[ARB]		#I the input world x coordinates
double	twyref[ARB]		#I the input world y coordinates
int	npts			#I the number of input points
char	xformat[ARB]		#I the logical x coordinates format
char	yformat[ARB]		#I the logical y coordinates format
char	wxformat[ARB]		#I the reference world x coordinates format
char	wyformat[ARB]		#I the reference world y coordinates format
char	twxformat[ARB]		#I the input world x coordinates format
char	twyformat[ARB]		#I the input world y coordinates format

int	i
pointer	sp, fmtstr

begin
	call smark (sp)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)

	# Write the column descriptions.
	call fprintf (ofd,
	    "# \tColumn 1: reference logical x coordinate\n")
	call fprintf (ofd,
	    "# \tColumn 2: reference logical y coordinate\n")
	call fprintf (ofd,
	    "# \tColumn 3: input logical x coordinate\n")
	call fprintf (ofd,
	    "# \tColumn 4: input logical y coordinate\n")
	call fprintf (ofd,
	    "# \tColumn 5: reference world x coordinate\n")
	call fprintf (ofd,
	    "# \tColumn 6: reference world y coordinate\n")
	call fprintf (ofd,
	    "# \tColumn 7: input world x coordinate\n")
	call fprintf (ofd,
	    "# \tColumn 8: input world y coordinate\n")
	call fprintf (ofd, "\n")

	call sprintf (Memc[fmtstr], SZ_LINE,
	    "%s  %s    %s  %s    %s  %s    %s  %s\n")
	    call pargstr (xformat)
	    call pargstr (yformat)
	    call pargstr (xformat)
	    call pargstr (yformat)
	    call pargstr (wxformat)
	    call pargstr (wyformat)
	    call pargstr (twxformat)
	    call pargstr (twyformat)

	do i = 1, npts {
	    call fprintf (ofd, Memc[fmtstr])
		call pargd (xref[i])
		call pargd (yref[i])
		call pargd (xin[i])
		call pargd (yin[i])
		call pargd (wxref[i])
		call pargd (wyref[i])
		call pargd (twxref[i])
		call pargd (twyref[i])
	}

	call sfree (sp)
end

