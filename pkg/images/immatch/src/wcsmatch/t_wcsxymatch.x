include <fset.h>
include <imhdr.h>
include <mwset.h>
include "wcsxymatch.h"

# T_WCSXYMATCH -- Compute a list of the tie points required to register an
# image to a reference image using WCS information in the image headers.

procedure t_wcsxymatch()

bool	verbose, transpose
double	xmin, xmax, ymin, ymax, x1, x2, y1, y2
int	ilist, rlist, olist, clist, cfd, ofd
int	nx, ny, npts, wcs, xcolumn, ycolumn
int	xunits, yunits, min_sigdigits, axstat, projstat
pointer	sp, refimage, image, xformat, yformat, rxformat, ryformat
pointer	wxformat, wyformat, str, paxno, rlaxno, laxno
pointer	im, imr, mw, mwr, rxl, ryl, rxw, ryw, ixl, iyl, ctr, ct

bool	clgetb(), streq()
double	clgetd()
int	imtopen(), fntopnb(), imtlen(), fntlenb(), imtgetim(), open(), clgeti()
int	clgwrd(), rg_rdxy(), fntgfnb(), rg_axstat(), rg_projstat(), mw_stati()
int	strdic()
pointer	immap(), mw_openim(), rg_xytoxy()
errchk	mw_openim(), mw_gwattrs()

begin
	# Get some temporary working space.
	call smark (sp)
	call salloc (refimage, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (xformat, SZ_FNAME, TY_CHAR)
	call salloc (yformat, SZ_FNAME, TY_CHAR)
	call salloc (wxformat, SZ_FNAME, TY_CHAR)
	call salloc (wyformat, SZ_FNAME, TY_CHAR)
	call salloc (rxformat, SZ_FNAME, TY_CHAR)
	call salloc (ryformat, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	call salloc (paxno, IM_MAXDIM, TY_INT)
	call salloc (rlaxno, IM_MAXDIM, TY_INT)
	call salloc (laxno, IM_MAXDIM, TY_INT)

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
	transpose = clgetb ("transpose")

	# Get the output coordinate formatting information.
	call clgstr ("xformat", Memc[xformat], SZ_FNAME)
	call clgstr ("yformat", Memc[yformat], SZ_FNAME)
	call clgstr ("wxformat", Memc[rxformat], SZ_FNAME)
	call clgstr ("wyformat", Memc[ryformat], SZ_FNAME)
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

	    # Open the output file.
	    if (fntgfnb (olist, Memc[str], SZ_FNAME) != EOF) {
		ofd = open (Memc[str], NEW_FILE, TEXT_FILE)
		if (ofd == STDOUT)
		    call fseti (ofd, F_FLUSHNL, YES)
		else if (fntlenb (olist) != imtlen (ilist))
	    	    call error (0,
	  "The number of output coords files and input images is not the same.")
	    }

	    # Open the reference image and reference coordinate file and
	    # compute the logical and world reference coordinates.
	    if (imtgetim (rlist, Memc[refimage], SZ_FNAME) != EOF) {

		# Open the reference image.
		if (imr != NULL) {
		    call mfree (rxl, TY_DOUBLE)
		    call mfree (ryl, TY_DOUBLE)
		    call mfree (rxw, TY_DOUBLE)
		    call mfree (ryw, TY_DOUBLE)
		    call mfree (ixl, TY_DOUBLE)
		    call mfree (iyl, TY_DOUBLE)
		    if (mwr != NULL)
		        call mw_close (mwr)
		    call imunmap (imr)
		}
		imr = immap (Memc[refimage], READ_ONLY, 0)
		if (IM_NDIM(imr) > 2)
		    call error (0, "The reference image must be 1D or 2D")

		# Open the reference image wcs.
		iferr (mwr = mw_openim (imr))
		    mwr = NULL

		# Check that the wcs dimensions are rational.
		if (mwr != NULL) {
		    if (mw_stati(mwr, MW_NPHYSDIM) < IM_NDIM(imr) || 
			mw_stati (mwr, MW_NDIM) != IM_NDIM(imr)) {
			call mw_close (mwr)
			mwr = NULL
		    }
		}

		# Get the reference image physical and logical axis maps.
		if (mwr != NULL) {
		    call mw_gaxmap (mwr, Memi[paxno], Memi[rlaxno],
			mw_stati(mwr, MW_NPHYSDIM))
		    call rg_laxmap (Memi[paxno], mw_stati(mwr, MW_NPHYSDIM),
			Memi[rlaxno], mw_stati(mwr, MW_NDIM))
		} else {
		    Memi[rlaxno] = 1
		    Memi[rlaxno+1] = 2
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
		        call malloc (rxl, npts, TY_DOUBLE)
		        call malloc (ryl, npts, TY_DOUBLE)
		        call malloc (ixl, npts, TY_DOUBLE)
		        call malloc (iyl, npts, TY_DOUBLE)
			if (wcs == RG_WORLD)
		    	    ctr = rg_xytoxy (mwr, Memd[rxw], Memd[ryw],
			        Memd[rxl], Memd[ryl], npts, "world",
				    "logical", 1, 2)
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
	    iferr (mw = mw_openim (im))
		mw = NULL
	    if (mw != NULL) {
		if (mw_stati(mw, MW_NPHYSDIM) < IM_NDIM(im) || 
		    mw_stati (mw, MW_NDIM) != IM_NDIM(im)) {
		    call mw_close (mw)
		    mw = NULL
		}
	    }

	    # Get the input image wcs physical and logical axis maps.
	    if (mw != NULL) {
		call mw_gaxmap (mw, Memi[paxno], Memi[laxno], mw_stati(mw,
		    MW_NPHYSDIM))
		call rg_laxmap (Memi[paxno], mw_stati(mw, MW_NPHYSDIM),
		    Memi[laxno], mw_stati(mw, MW_NDIM))
	    } else {
		Memi[laxno] = 1
		Memi[laxno+1] = 2
	    }

	    # Write the banner string.
	    call fprintf (ofd,
	        "\n# Reference image: %s  Input image: %s\n# \tCoords: %s")
	        call pargstr (Memc[refimage])
	        call pargstr (Memc[image])
	    if (clist == NULL) {
		call pargstr ("grid")
		call fprintf (ofd, "\n")
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

	    # Printe message on the terminal.
	    if (verbose && ofd != STDOUT) {
		call printf (
		    "\nReference image: %s  Input image: %s\n\tCoords: %s")
		    call pargstr (Memc[refimage])
		    call pargstr (Memc[image])
		if (clist == NULL) {
		    call pargstr ("grid")
		    call printf ("\n")
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
	    }

	    # Set the reference coordinate formats.
	    if (Memc[rxformat] == EOS)
		 call rg_wsetfmt (mwr, mw, wcs, Memi[rlaxno], Memi[laxno],
		    min_sigdigits, Memc[wxformat], SZ_FNAME)
	    else
		call strcpy (Memc[rxformat], Memc[wxformat], SZ_FNAME)

	    if (Memc[ryformat] == EOS)
		 call rg_wsetfmt (mwr, mw, wcs, Memi[rlaxno+1], Memi[laxno+1],
		    min_sigdigits, Memc[wyformat], SZ_FNAME)
	    else
		call strcpy (Memc[ryformat], Memc[wyformat], SZ_FNAME)

	    # Compute the output coordinates issuing a warning if the
	    # axes types are not compatable.
	    if (mwr == NULL) {
		call fprintf (ofd,
		    "# \tWarning: reference image wcs is undefined\n")
		if (verbose && ofd != STDOUT)
		    call printf (
		        "\tWarning: reference image wcs is undefined\n")
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
		}
		ct = NULL
	    } else if (ctr == NULL) {
		call fprintf (ofd, "# \tWarning: Unable to compute reference \
logical <-> world transform\n")
		if (verbose && ofd != STDOUT) {
		    call printf ("\tWarning: Unable to compute reference \
logical <-> world transform\n")
		}
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
		}
		ct = NULL
	    } else if (mw == NULL) {
		call fprintf (ofd,
		    "# \tWarning: input image wcs is undefined\n")
		if (verbose && ofd != STDOUT)
		    call printf ("\tWarning: input image wcs is undefined\n")
		call amovd (Memd[rxl], Memd[ixl], npts)
		call amovd (Memd[ryl], Memd[iyl], npts)
		ct = NULL
	    } else {
		# Check axis status.
		if (wcs == RG_PHYSICAL) {
		    axstat = RG_AXEQUAL
		    projstat = RG_AXEQUAL
	            ct = rg_xytoxy (mw, Memd[rxw], Memd[ryw], Memd[ixl],
		        Memd[iyl], npts, "physical", "logical", 1, 2)
		    if (ct == NULL) {
		        call fprintf (ofd,
			    "# \tWarning: Unable to compute image physical -> \
logical transform\n")
			if (verbose && ofd != STDOUT) {
		            call printf (
		                "\tWarning: Unable to compute image physical \
-> logical transform\n")
			}
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
		    axstat = rg_axstat (mwr, Memi[rlaxno], Memi[rlaxno+1],
		        mw, Memi[laxno], Memi[laxno+1], transpose)
		    projstat = rg_projstat (mwr, Memi[rlaxno], Memi[rlaxno+1],
		        mw, Memi[laxno], Memi[laxno+1])
		    switch (axstat) {
		    case RG_AXEQUAL, RG_AXNOTEQUAL:
	                ct = rg_xytoxy (mw, Memd[rxw], Memd[ryw], Memd[ixl],
		            Memd[iyl], npts, "world", "logical", 1, 2)
		    case RG_AXSWITCHED:
	                ct = rg_xytoxy (mw, Memd[ryw], Memd[rxw], Memd[ixl],
		            Memd[iyl], npts, "world", "logical", 1, 2)
		    }
		    if (ct == NULL) {
		        call fprintf (ofd,
			    "# \tWarning: Unable to compute image \
			     world -> logical transform\n")
			if (verbose && ofd != STDOUT) {
		            call printf (
		                "\tWarning: Unable to compute image world -> \
logical transform\n")
			}
			if (IM_NDIM(imr) == 1)
		    	    call rg_rxyl (Memd[rxl], Memd[ryl], nx, 1, 1.0d0,
		        	double(IM_LEN(im,1)), 1.0d0, 1.0d0)
			else
		    	    call rg_rxyl (Memd[rxl], Memd[ryl], nx, ny, 1.0d0,
		        	double(IM_LEN(im,1)), 1.0d0,
				double(IM_LEN(im,2)))
			call amovd (Memd[rxl], Memd[ixl], npts)
			call amovd (Memd[ryl], Memd[iyl], npts)
		    } else if (axstat == RG_AXNOTEQUAL) {
		        call fprintf (ofd,
		            "# \tWarning: Reference and image axtype \
attributes are different\n")
			if (verbose && ofd != STDOUT) {
		            call printf (
			        "\tWarning: Reference and image axtype \
attributes are different\n")
			}
			if (IM_NDIM(imr) == 1)
		    	    call rg_rxyl (Memd[rxl], Memd[ryl], nx, 1, 1.0d0,
		        	double(IM_LEN(im,1)), 1.0d0, 1.0d0)
			else
		    	    call rg_rxyl (Memd[rxl], Memd[ryl], nx, ny, 1.0d0,
		        	double(IM_LEN(im,1)), 1.0d0,
				double(IM_LEN(im,2)))
			call amovd (Memd[rxl], Memd[ixl], npts)
			call amovd (Memd[ryl], Memd[iyl], npts)
		    } else if (projstat == RG_AXNOTEQUAL) {
		        call fprintf (ofd,
		            "# \tWarning: Reference and image wtype \
attributes are different\n")
			if (verbose && ofd != STDOUT) {
		            call printf (
		                "\tWarning: Reference and image wtype \
attributes are different\n")
			}
		    }
		}
	    }

	    # Write out the results.
	    call rg_wcoords (ofd, Memd[rxl], Memd[ryl], Memd[ixl],
		Memd[iyl], Memd[rxw], Memd[ryw], npts, Memc[xformat],
		Memc[yformat], Memc[wxformat], Memc[wyformat])

	    # Close the input image and its wcs.
	    if (mw != NULL)
	        call mw_close (mw)
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
	    call mfree (ixl, TY_DOUBLE)
	    call mfree (iyl, TY_DOUBLE)
	    if (mwr != NULL)
	        call mw_close (mwr)
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


# RG_WSETFMT -- Set the world coordinate format.

procedure rg_wsetfmt (mwr, mw, wcs, rlaxno, laxno, min_sigdigits,
       wformat, maxch)

pointer	mwr			#I pointer to the reference image wcs
pointer	mw			#I pointer to the input image wcs
int	wcs			#I the input wcs type
int	rlaxno			#I the reference physical axis number
int	laxno			#I the input physical axis number
int	min_sigdigits		#I the minimum number of significant digits
char	wformat[ARB]		#O the output world coordinate format
int	maxch			#I the maximum size of the format string

pointer	sp, str
bool	streq()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (mwr == NULL || mw == NULL) {
	    call sprintf (wformat, maxch, "%%%d.%dg")
		call pargi (min_sigdigits + 3)
		call pargi (min_sigdigits)

	} else if (wcs == RG_PHYSICAL) {
	    call strcpy ("%10.3f", wformat, maxch)

	}  else {
	    iferr {
		call mw_gwattrs (mwr, rlaxno, "format", wformat, maxch)
	    } then {
		iferr {
		     call mw_gwattrs (mw, laxno, "format", wformat, maxch)
		} then {
		    iferr {
		        call mw_gwattrs (mwr, rlaxno, "axtype", Memc[str],
			    SZ_FNAME)
		    } then {
	    	        call sprintf (wformat, maxch, "%%%d.%dg")
		            call pargi (min_sigdigits + 3)
			    call pargi (min_sigdigits)
		    } else {
			if (streq (Memc[str], "ra"))
			    call strcpy ("%11.1H", wformat, maxch)
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
	}

	call sfree (sp)
end


# RG_AXSTAT -- Determine whether or not the two axes are equal.

int procedure rg_axstat (mw1, ax11, ax12, mw2, ax21, ax22, transpose)

pointer	mw1			#I pointer to the first wcs
int	ax11, ax12		#I the logical reference axes
pointer	mw2			#I pointer to the second wcs
int	ax21, ax22		#I the logical input axes
bool	transpose		#I transpose the world coordinates

int	stat
pointer	sp, xax1, yax1, xax2, yax2
bool	streq()
errchk	mw_gwattrs()

begin
	call smark (sp)
	call salloc (xax1, SZ_FNAME, TY_CHAR)
	call salloc (yax1, SZ_FNAME, TY_CHAR)
	call salloc (xax2, SZ_FNAME, TY_CHAR)
	call salloc (yax2, SZ_FNAME, TY_CHAR)

	iferr (call mw_gwattrs (mw1, ax11, "axtype", Memc[xax1], SZ_FNAME))
	    Memc[xax1] = EOS
	iferr (call mw_gwattrs (mw1, ax12, "axtype", Memc[yax1], SZ_FNAME))
	    Memc[yax1] = EOS
	iferr (call mw_gwattrs (mw2, ax21, "axtype", Memc[xax2], SZ_FNAME))
	    Memc[xax2] = EOS
	iferr (call mw_gwattrs (mw2, ax22, "axtype", Memc[yax2], SZ_FNAME))
	    Memc[yax2] = EOS

	if (transpose)
	    stat = RG_AXSWITCHED
	else if (streq (Memc[xax1], Memc[xax2]) && streq(Memc[yax1],
	    Memc[yax2]))
	    stat = RG_AXEQUAL
	else if (streq (Memc[xax1], Memc[yax2]) && streq(Memc[yax1],
	    Memc[xax2]))
	    stat = RG_AXSWITCHED
	else
	    stat = RG_AXNOTEQUAL

	call sfree (sp)

	return (stat)
end


# RG_PROJSTAT -- Determine whether or not the projections of two axes are equal.

int procedure rg_projstat (mw1, ax11, ax12, mw2, ax21, ax22)

pointer	mw1			#I pointer to the first wcs
int	ax11, ax12		#I the logical reference axes
pointer	mw2			#I pointer to the second wcs
int	ax21, ax22		#I the logical reference axes

int	stat
pointer	sp, xproj1, yproj1, xproj2, yproj2
bool	streq()
errchk	mw_gwattrs()

begin
	call smark (sp)
	call salloc (xproj1, SZ_FNAME, TY_CHAR)
	call salloc (yproj1, SZ_FNAME, TY_CHAR)
	call salloc (xproj2, SZ_FNAME, TY_CHAR)
	call salloc (yproj2, SZ_FNAME, TY_CHAR)

	iferr (call mw_gwattrs (mw1, ax11, "wtype", Memc[xproj1], SZ_FNAME))
	    Memc[xproj1] = EOS
	iferr (call mw_gwattrs (mw1, ax12, "wtype", Memc[yproj1], SZ_FNAME))
	    Memc[yproj1] = EOS
	iferr (call mw_gwattrs (mw2, ax21, "wtype", Memc[xproj2], SZ_FNAME))
	    Memc[xproj2] = EOS
	iferr (call mw_gwattrs (mw2, ax22, "wtype", Memc[yproj2], SZ_FNAME))
	    Memc[yproj2] = EOS

	if (streq (Memc[xproj1], Memc[xproj2]) && streq(Memc[yproj1],
	    Memc[yproj2]))
	    stat = RG_AXEQUAL
	else if (streq (Memc[xproj1], Memc[yproj2]) && streq(Memc[yproj1],
	    Memc[xproj2]))
	    stat = RG_AXSWITCHED
	else
	    stat = RG_AXNOTEQUAL

	call sfree (sp)

	return (stat)
end


# RG_WCOORDS -- Write out the reference and input logical coordinates of the
# tie points and the reference world coordinates.

procedure rg_wcoords (ofd, xref, yref, xin, yin, wxref, wyref, npts,
        xformat, yformat, wxformat, wyformat)

int	ofd			#I the output file descriptor
double	xref[ARB]		#I the reference logical x coordinates
double	yref[ARB]		#I the reference logical y coordinates
double	xin[ARB]		#I the input logical x coordinates
double	yin[ARB]		#I the input logical y coordinates
double	wxref[ARB]		#I the input reference world x coordinates
double	wyref[ARB]		#I the input reference world y coordinates
int	npts			#I the number of input points
char	xformat[ARB]		#I the logical x coordinates format
char	yformat[ARB]		#I the logical y coordinates format
char	wxformat[ARB]		#I the world x coordinates format
char	wyformat[ARB]		#I the world y coordinates format

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
	call fprintf (ofd, "\n")

	# Create the format string.
	call sprintf (Memc[fmtstr], SZ_LINE, "%s  %s    %s  %s    %s  %s\n")
	    call pargstr (xformat)
	    call pargstr (yformat)
	    call pargstr (xformat)
	    call pargstr (yformat)
	    call pargstr (wxformat)
	    call pargstr (wyformat)

	do i = 1, npts {
	    call fprintf (ofd, Memc[fmtstr])
		call pargd (xref[i])
		call pargd (yref[i])
		call pargd (xin[i])
		call pargd (yin[i])
		call pargd (wxref[i])
		call pargd (wyref[i])
	}

	call sfree (sp)
end


# RG_LAXMAP (paxno, wcsndim, laxno, ndim)

procedure rg_laxmap (paxno, wcsndim, laxno, ndim)

int     paxno[ARB]              #I the physical axis map
int     wcsndim                 #I the number of physical axis dimensions
int     laxno[ARB]              #O the physical axis map
int     ndim                    #I the number of logical axis dimensions

int     i, j

begin
        if (ndim < wcsndim) {
            do i = 1, ndim {
                laxno[i] = 0
                do j = 1, wcsndim {
                    if (paxno[j] != i)
                        next
                    laxno[i] = j
                    break
                }
            }
            do i = ndim + 1, wcsndim
                laxno[i] = 0
        } else {
            do i = 1, wcsndim
                laxno[i] = i
        }
end
