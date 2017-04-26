include <imhdr.h>
include <error.h>
include "quadgeom.h"

# QUADMAP --  Map subimages, one for each readout, for input or output

int procedure quadmap (rootname, mode, clobber, in, qg, out)

char	rootname[SZ_FNAME]	#I Root name for output images.
int	mode			#I Access mode.
bool	clobber			#I Clobber existing output images.
pointer	in			#I Input image pointer (for NEW_COPY).
pointer	qg			#I Pointer to quadgeom structure.
pointer	out[ARB]		#O Array of imio pointers for sub-images.

int	nopen			#O Number of subimages mapped.

int	i, j, x, y, nx[QG_MAXAMPS], nampsx, nampsy
char	fullname[SZ_LINE], id[SZ_AMPID]

pointer	immap()
int	ahivi(), imaccess()

begin
	switch (mode) {
	case NEW_COPY, NEW_IMAGE:

	    # Loop over active readouts
	    nopen = 0
	    do i = 1, QG_NAMPS(qg) {

		nopen = nopen + 1

		# The sub-section image need only be written if this is not a 
		# phantom
		if (QG_PHANTOM (qg, i) == NO) {

		    # Make sub-image name
		    call sprintf (fullname, SZ_LINE, "%s.%s")
			call pargstr (rootname)
			call pargstr (Memc[QG_AMPID(qg, nopen)])

		    # If clobber is set then we can delete any pre-existing
		    # sub-images. Otherwise it is an error if the sub-image already
		    # exists. However we leave it to the immap call to find out.
		    if (clobber) {
			if (imaccess (fullname, READ_ONLY) == YES) 
			    call imdelete (fullname)
		    }

		    iferr (out[nopen] = immap (fullname, mode, in)) {
			nopen = nopen - 1
			do j = 1, nopen 
			    call imunmap (out[j])
			call erract (EA_ERROR)
		    }

		    call quadwritehdr (qg, out[nopen], i)

		} else {
		    out[nopen] = NULL
		}
	    }


	case READ_ONLY, READ_WRITE:

	    # Loop over full grid of possible readout positions.
	    nopen = 0
	    do y = 1, QG_MAXAMPS {
		nx[y] = 0
		do x = 1, QG_MAXAMPS {

		    # Make readout id string
		    call sprintf (id, SZ_AMPID, "%1d%1d")
			call pargi (y)
			call pargi (x)

		    # Make sub-image name
		    call sprintf (fullname, SZ_LINE, "%s.%s")
			call pargstr (rootname)
			call pargstr (id)

		    # Attempt to map it.
		    nopen = nopen + 1
		    if (nopen > QG_MAXAMPS) {
			nopen = nopen - 1
			next
		    }

		    # Skip to next grid position if sub-image does not exist.
		    iferr (out[nopen] = immap (fullname, mode, in)) {
			nopen = nopen - 1
			next
		    }
		    nx[y] = nx[y] + 1
		    call quadreadhdr (qg, out[nopen], nopen, id)
		}
	    }

	    nampsx = ahivi (nx, QG_MAXAMPS)
	    nampsy = nopen / nampsx
	    QG_NAMPS(qg)  = nopen
	    QG_NAMPSX(qg) = nampsx
	    QG_NAMPSY(qg) = nampsy

	    # Consolidate quadgeom structure and perform consistancy checks
#	    call quaddump (qg)
	    call quadmerge (qg)

	}

	return (nopen)
end

# QUADWRITEHDR -- Add dimensions and section information to image header.

procedure quadwritehdr (qg, im, readout)

pointer	im			#I Pointer to output sub-image image.
pointer	qg			#I Pointer to open quadgeom structure.
int	readout			#I readout number.

int	amp
pointer	sp, section, keyword

int	hdmaccf()

begin
	call smark (sp)
	call salloc (section, SZ_LINE, TY_CHAR)
	call salloc (keyword, SZ_LINE, TY_CHAR)

	IM_LEN (im, 1) = QG_NX(qg, readout)
	IM_LEN (im, 2) = QG_NY(qg, readout)

	call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
	    call pargi (QG_DX1(qg, readout))
	    call pargi (QG_DX2(qg, readout))
	    call pargi (QG_DY1(qg, readout))
	    call pargi (QG_DY2(qg, readout))
	call hdmpstr (im, "datasec", Memc[section])

	call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
	    call pargi (QG_TX1(qg, readout))
	    call pargi (QG_TX2(qg, readout))
	    call pargi (QG_TY1(qg, readout))
	    call pargi (QG_TY2(qg, readout))
	call hdmpstr (im, "trimsec", Memc[section])

	call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
	    call pargi (QG_BX1(qg, readout))
	    call pargi (QG_BX2(qg, readout))
	    call pargi (QG_BY1(qg, readout))
	    call pargi (QG_BY2(qg, readout))
	call hdmpstr (im, "biassec", Memc[section])

	call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
	    call pargi (QG_CX1(qg, readout))
	    call pargi (QG_CX2(qg, readout))
	    call pargi (QG_CY1(qg, readout))
	    call pargi (QG_CY2(qg, readout))
	call hdmpstr (im, "ccdsec", Memc[section])

	# Delete zSECyx keywords for all other amps from header
	do amp = 1, QG_NAMPS(qg) {
	    if (amp != readout) {
	       call sprintf (Memc[keyword], SZ_LINE, "ASEC%2s")
		   call pargstr (Memc[QG_AMPID(qg, amp)])
		if (hdmaccf (im, Memc[keyword]) == YES)
		     call hdmdelf (im, Memc[keyword])

	       call sprintf (Memc[keyword], SZ_LINE, "BSEC%2s")
		   call pargstr (Memc[QG_AMPID(qg, amp)])
		if (hdmaccf (im, Memc[keyword]) == YES)
		     call hdmdelf (im, Memc[keyword])

	       call sprintf (Memc[keyword], SZ_LINE, "CSEC%2s")
		   call pargstr (Memc[QG_AMPID(qg, amp)])
		if (hdmaccf (im, Memc[keyword]) == YES)
		     call hdmdelf (im, Memc[keyword])

	       call sprintf (Memc[keyword], SZ_LINE, "DSEC%2s")
		   call pargstr (Memc[QG_AMPID(qg, amp)])
		if (hdmaccf (im, Memc[keyword]) == YES)
		     call hdmdelf (im, Memc[keyword])

	       call sprintf (Memc[keyword], SZ_LINE, "TSEC%2s")
		   call pargstr (Memc[QG_AMPID(qg, amp)])
		if (hdmaccf (im, Memc[keyword]) == YES)
		     call hdmdelf (im, Memc[keyword])
	    }
	}

	call sfree (sp)

end

# QUADREADHDR -- Get dimensions and section information from image header.

procedure quadreadhdr (qg, im, readout, id)

pointer	qg			#I Pointer to open quadgeom structure.
pointer	im			#I Pointer to input sub-image image.
int	readout			#I Readout number.
char	id[SZ_AMPID]		#I Readout identifier.

int	nx, ny
int	dx1, dx2, dxs, dy1, dy2, dys
int	tx1, tx2, txs, ty1, ty2, tys
int	bx1, bx2, bxs, by1, by2, bys
int	cx1, cx2, cxs, cy1, cy2, cys
pointer	sp, section

int	hdmaccf(), strdic()

begin
	call smark (sp)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Store QG_AMPID and set QG_AMPTYPE
	call malloc (QG_AMPID(qg, readout), SZ_AMPID, TY_CHAR)

	call strcpy (id, Memc[QG_AMPID(qg, readout)], SZ_AMPID)

	QG_AMPTYPE (qg, readout) = strdic (id, id, SZ_AMPID, AMPDICT)

	# Get input image dimensions.
	nx = IM_LEN (im, 1)
	ny = IM_LEN (im, 2)
	QG_NX(qg, readout) = nx
	QG_NY(qg, readout) = ny

	# Get datasec, trimsec etc. from image header, setting a null value
	# for any missing sections.
	if (hdmaccf (im, "datasec") == YES) {
	    call hdmgstr (im, "datasec", Memc[section], SZ_LINE)
	    dx1 = 1
	    dx2 = nx
	    dxs = 1
	    dy1 = 1
	    dy2 = ny
	    dys = 1
	    call ccd_section (Memc[section], dx1, dx2, dxs, dy1, dy2, dys)
	}
	QG_DX1(qg, readout) = dx1
	QG_DX2(qg, readout) = dx2
	QG_DY1(qg, readout) = dy1
	QG_DY2(qg, readout) = dy2

	if (hdmaccf (im, "trimsec") == YES) {
	    call hdmgstr (im, "trimsec", Memc[section], SZ_LINE)
	    tx1 = dx1
	    tx2 = dx2
	    txs = 1
	    ty1 = dy1
	    ty2 = dy2
	    tys = 1
	    call ccd_section (Memc[section], tx1, tx2, txs, ty1, ty2, tys)
	}
	QG_TX1(qg, readout) = tx1
	QG_TX2(qg, readout) = tx2
	QG_TY1(qg, readout) = ty1
	QG_TY2(qg, readout) = ty2

	if (hdmaccf (im, "biassec") == YES) {
	    call hdmgstr (im, "biassec", Memc[section], SZ_LINE)
	    bx1 = dx2 + 1
	    bx2 = nx
	    bxs = 1
	    by1 = 1
	    by2 = ny
	    bys = 1
	    call ccd_section (Memc[section], bx1, bx2, bxs, by1, by2, bys)
	}
	QG_BX1(qg, readout) = bx1
	QG_BX2(qg, readout) = bx2
	QG_BY1(qg, readout) = by1
	QG_BY2(qg, readout) = by2

	if (hdmaccf (im, "ccdsec") == YES) {
	    call hdmgstr (im, "ccdsec", Memc[section], SZ_LINE)
	    cx1 = dx1
	    cx2 = dx2
	    cxs = 1
	    cy1 = dy1
	    cy2 = dy2
	    cys = 1
	    call ccd_section (Memc[section], cx1, cx2, cxs, cy1, cy2, cys)
	}
	QG_CX1(qg, readout) = cx1
	QG_CX2(qg, readout) = cx2
	QG_CY1(qg, readout) = cy1
	QG_CY2(qg, readout) = cy2

	call sfree (sp)
end
