include	<imhdr.h>
include <error.h>
include <mwset.h>

# IP_LISTPIXELS -- Convert image pixels into a text stream, i.e., into a list.
# Each pixel is printed on a separate line, preceded by its coordinates.

procedure ip_listpix (im)

char	wcs[SZ_FNAME]
double	incoords[IM_MAXDIM], outcoords[IM_MAXDIM]
int	i, j, npix, ndim, wcsndim, laxis1, fmtstat
int	paxno[IM_MAXDIM], laxno[IM_MAXDIM]
long	v[IM_MAXDIM], vcoords[IM_MAXDIM]
pointer	im, line, mw, ct, fmtptrs[IM_MAXDIM]

int	imgnlr(), mw_stati()
pointer	mw_openim(), mw_sctran()

begin
	# Get info from the input image.
	ndim = IM_NDIM(im)
	npix = IM_LEN(im,1)

	# Get the wcs.
	call strcpy ("world", wcs, SZ_FNAME)
	ifnoerr (mw = mw_openim (im)) {
	    # Set up the transformation.
	    call mw_seti (mw, MW_USEAXMAP, NO)
	    ct = mw_sctran (mw, "logical", wcs, 0)
	    wcsndim = mw_stati (mw, MW_NPHYSDIM)

	    # Get the physical to logical axis map.
	    call mw_gaxmap (mw, paxno, laxno, wcsndim)

	    # Set the default wcs.
	    call mw_ssytem (mw, wcs)

	} else {
	    # Print the error message from the above loop.
	    call erract (EA_WARN)

	    # Set the transform to the identity transform.
	    mw = NULL
	    ct = NULL
	    wcsndim = ndim

	    # Set the default physical to logical axis map.
	    do i = 1, wcsndim
	        paxno[i] = i
	}

	# Initialize the v vectors.
	call amovkl (long (1), v, IM_MAXDIM)
	call amovkl (long (1), vcoords, IM_MAXDIM)

	# Initialize the coordinates.
	laxis1 = 0
	do i = 1, wcsndim {
	    if (paxno[i] == 0) {
	        incoords[i] = 1
	    } else if (paxno[i] == 1) {
	        laxis1 = i
	        incoords[i] = v[1]
	    } else {
	        incoords[i] = v[paxno[i]]
	    }
	}

	# Check and correct for the no axis mapping case.
	if (laxis1 == 0) {
	    laxis1 = 1
	    do i = 1, wcsndim
	        paxno[i] = i
	}

	# Get the logical to physical axis map for the format strings.
	do i = 1, ndim {
	    laxno[i] = 0
	    do j = 1, wcsndim {
	        if (paxno[j] != i)
	    	    next
	        laxno[i] = j
	            break
	    }
	}

	# Set the format strings for the logical axes.
	fmtstat = EOS
	do i = 1, ndim {
	    call malloc (fmtptrs[i], SZ_FNAME, TY_CHAR)
	    if (fmtstat != EOF)
	        call gargwrd (Memc[fmtptrs[i]], SZ_FNAME)
	    else
		Memc[fmtptrs[i]] = EOS
	    if (laxno[i] == 0)
	        call strcpy ("%0.15g ", Memc[fmtptrs[i]], SZ_FNAME)
	    else if (mw == NULL || ct == NULL)
	        call strcpy ("%0.15g ", Memc[fmtptrs[i]], SZ_FNAME)
	    else iferr (call mw_gwattrs (mw, laxno[i], "format",
	        Memc[fmtptrs[i]], SZ_FNAME))
	        call strcpy ("%0.15g ", Memc[fmtptrs[i]], SZ_FNAME)
	    else
	        call strcat (" ", Memc[fmtptrs[i]], SZ_FNAME)
	}

	# Print the pixels.
	while (imgnlr (im, line, v) != EOF) {
	    do i = 1, npix {
		incoords[laxis1] = i
	    	if (ct == NULL)
	    	    call amovd (incoords, outcoords, wcsndim)
	    	else
	    	    call mw_ctrand (ct, incoords, outcoords, wcsndim)
	    	do j = 1, ndim {	        # X, Y, Z, etc.
	    	    call printf (Memc[fmtptrs[j]])
	    	    if (laxno[j] == 0)
	    	        call pargd (double(vcoords[j]))
	    	    else
	    	        call pargd (outcoords[laxno[j]])
	    	}
	    	call printf (" %g\n")	        # pixel value
	    	    call pargr (Memr[line+i-1])
	    }
	    call amovl (v, vcoords, IM_MAXDIM)
	    do i = 1, wcsndim {
	    	if (paxno[i] == 0)
	    	    next
	    	incoords[i] = v[paxno[i]]
	    }
	}

	do i = 1, ndim
	    call mfree (fmtptrs[i], TY_CHAR)
	if (mw != NULL)
	    call mw_close (mw)
end
