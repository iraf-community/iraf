include	<error.h>
include	<imhdr.h>


# T_COINCOR -- Apply coincidence corrections to spectra

procedure t_coincor ()

int	root, start_rec, ccmode, npts, nrecs, coflag
real	dtime, power, expo
pointer	sp, image, ofile, str, recs, imin, imout, pixin, pixout

int	clpopni(), clgeti(), clgwrd(), imgeti()
int	get_next_image(), decode_ranges()
real	clgetr(), imgetr()
pointer	immap(), imgl1r(), impl1r()
errchk	coincor

begin
	# Allocate memory
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (ofile, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (recs, 300, TY_INT)

	# Get parameters
	root   = clpopni ("input")
	call clgstr ("output", Memc[ofile], SZ_FNAME)
	if (Memc[ofile] != EOS)
	    start_rec = clgeti ("start_rec")
	ccmode = clgwrd ("ccmode", Memc[str], SZ_LINE, ",photo,iids,")
	dtime = clgetr ("deadtime")
	power = clgetr ("power")
	call clgstr ("records", Memc[str], SZ_LINE)

	# Initialize
	if (decode_ranges (Memc[str], Memi[recs], 100, nrecs) == ERR)
	    call error (0, "Bad range specification")
	call reset_next_image ()

	# Loop over all input images by subsets
	while (get_next_image (root, Memi[recs], nrecs, Memc[image],
	    SZ_FNAME) != EOF) {

	    # Open input image and check coincidence flag
	    iferr (imin = immap (Memc[image], READ_WRITE, 0)) {
		call erract (EA_WARN)
	        start_rec = start_rec + 1
		next
	    }
	    iferr (coflag = imgeti (imin, "CO-FLAG"))
		coflag = -1
	    if (coflag > 0) {
		call printf ("[%s] already coincidence corrected\n")
		    call pargstr (IM_HDRFILE(imin))
		call flush (STDOUT)
		call imunmap (imin)
		next
	    }

	    # Open output image
	    if (Memc[ofile] != EOS) {
		call sprintf (Memc[str], SZ_LINE, "%s.%04d")
		    call pargstr (Memc[ofile])
		    call pargi (start_rec)
		start_rec = start_rec + 1

		imout = immap (Memc[str], NEW_COPY, imin)
		IM_PIXTYPE (imout) = TY_REAL
	    } else
		imout = imin

	    # Apply coincidence correction
	    pixin = imgl1r (imin)
	    pixout = impl1r (imout)
	    npts = IM_LEN (imin, 1)
	    iferr (expo = imgetr (imin, "EXPOSURE"))
		iferr (expo = imgetr (imin, "ITIME"))
		    iferr (expo = imgetr (imin, "EXPTIME"))
			expo = 1
	    call coincor (Memr[pixin], Memr[pixout], npts, expo, coflag,
		dtime, power, ccmode)

	    # Update flag and write status
	    call imaddi (imout, "CO-FLAG", coflag)
	    call printf ("[%s] --> [%s] %s\n")
		call pargstr (IM_HDRFILE(imin))
		call pargstr (IM_HDRFILE(imout))
		call pargstr (IM_TITLE(imout))
	    call flush (STDOUT)

	    # Close images
	    if (imout != imin)
		call imunmap (imout)
	    call imunmap (imin)
	}

	call clputi ("next_rec", start_rec)
	call clpcls (root)
	call sfree (sp)
end
